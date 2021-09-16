port module Eth.Token exposing
    ( CToken
    , CTokenSet
    , Token
    , TokenMsg(..)
    , TokenState
    , TokenTransactionMsg(..)
    , askCompCapFactoryAllowance
    , clearTokenState
    , emptyState
    , ethDecimals
    , getCTokenAddress
    , getCTokenByAddress
    , getTokenAddress
    , getUnderlyingTokenByAddress
    , getUnderlyingTokenDecimals
    , getUnderlyingTokenSymbol
    , isCAPFactoryApproved
    , isCEtherToken
    , tokenInit
    , tokenNewBlockCmd
    , tokenSubscriptions
    , tokenUpdate
    )

import BigInt
import CompoundComponents.Console as Console
import CompoundComponents.Eth.Decoders exposing (decimal, decodeAssetAddress, decodeContractAddress, decodeCustomerAddress)
import CompoundComponents.Eth.Ethereum as Ethereum exposing (Account(..), AssetAddress(..), ContractAddress(..), CustomerAddress(..), assetAddressToContractAddress, contractAddressToAssetAddress, getAssetAddressString, getContractAddressString, getCustomerAddressString)
import CompoundComponents.Eth.Network exposing (Network)
import CompoundComponents.Eth.TokenMath as TokenMath
import CompoundComponents.Ether.BNTransaction as BNTransaction exposing (BNTransactionState)
import CompoundComponents.Ether.FromEthereumUtils as FromEthereumUtils
import CompoundComponents.Ether.FunctionSpec as FunctionSpec
import CompoundComponents.Ether.Helpers
import CompoundComponents.Ether.Value as Value
import CompoundComponents.Ether.Web3 as EtherWeb3
import CompoundComponents.Functions exposing (default, handleError, maybeMap)
import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Eth.Config exposing (CTokenConfig, Config, TokenConfig)
import Json.Decode exposing (Value, decodeValue, field, int, string, succeed)
import Source.Infura exposing (loadEtherPrice)
import Utils.Http


type alias Token =
    { assetAddress : AssetAddress
    , name : String
    , symbol : String
    , decimals : Int
    }


type alias CToken =
    { contractAddress : ContractAddress
    , name : String
    , symbol : String
    , decimals : Int
    , underlying : Token
    }


type alias CTokenSet =
    Dict String CToken


type alias TokenState =
    { cTokens : CTokenSet
    , tokenAllowances : Dict String Decimal -- allowances key is a concat of asset-contract (0xdead-0xbeef)
    , infuraEtherPrice : Maybe Decimal
    , errors : List String
    }



-- Records used for processing port responses


type alias TokenAllowance =
    { asset : AssetAddress --This is the underlying ie BAT
    , contract : ContractAddress --This is the cToken ie cBAT
    , customer : CustomerAddress
    , allowance : Decimal
    }


type TokenMsg
    = SetTokenAllowance TokenAllowance
    | SetInfuraEtherUSD Decimal
    | Web3TransactionMsg TokenTransactionMsg
    | Error String


type TokenTransactionMsg
    = FaucetTokenAllocate Network ContractAddress AssetAddress CustomerAddress Int
    | FauceteerDrip Network ContractAddress ContractAddress AssetAddress CustomerAddress
    | FaucetTokenApprove Network ContractAddress AssetAddress CustomerAddress Bool


loadCTokenSet : Dict String CTokenConfig -> CTokenSet
loadCTokenSet dict =
    Dict.map
        (\k t ->
            let
                underlyingToken =
                    { assetAddress = contractAddressToAssetAddress t.underlying.address
                    , name = t.underlying.name
                    , symbol = t.underlying.symbol
                    , decimals = t.underlying.decimals
                    }
            in
            { contractAddress = t.address
            , name = t.name
            , symbol = t.symbol
            , decimals = t.decimals
            , underlying = underlyingToken
            }
        )
        dict


emptyState : TokenState
emptyState =
    { cTokens = Dict.empty
    , tokenAllowances = Dict.empty
    , infuraEtherPrice = Nothing
    , errors = []
    }


tokenInit : Config -> ( TokenState, Cmd TokenMsg )
tokenInit config =
    ( { cTokens = loadCTokenSet config.cTokens
      , tokenAllowances = Dict.empty
      , infuraEtherPrice = Nothing
      , errors = []
      }
    , askEtherPrice config
    )


ethDecimals : Int
ethDecimals =
    18


compDecimals : Int
compDecimals =
    18


tokenAllowancesKey : AssetAddress -> ContractAddress -> String
tokenAllowancesKey assetAddress contractAddress =
    getAssetAddressString assetAddress ++ "-" ++ getContractAddressString contractAddress


askEtherPrice : Config -> Cmd TokenMsg
askEtherPrice config =
    case config.maybeInvertedEtherPriceAsset of
        Just invertedEtherPriceAsset ->
            -- If we are pulling the Ether/USD price from another asset (USDC) we can ignore this ask safely.
            Cmd.none

        Nothing ->
            loadEtherPrice (handleError (Utils.Http.showError >> Error) SetInfuraEtherUSD)


askCompCapFactoryAllowance : Int -> CustomerAddress -> ContractAddress -> ContractAddress -> Cmd TokenMsg
askCompCapFactoryAllowance blockNumber customerAddress capFactoryAddress compAddress =
    let
        compAssetAddress =
            Ethereum.contractAddressToAssetAddress compAddress
    in
    askTokenAllowance blockNumber compAssetAddress capFactoryAddress customerAddress 18


tokenNewBlockCmd : Config -> TokenState -> Int -> Account -> Cmd TokenMsg
tokenNewBlockCmd config tokenState blockNumber maybeAccount =
    askEtherPrice config


{-| This update function is used to handle all non-web3 transactions messages. Eventually these messages
will probably be converted to web3 reads from the web3 elm module but until we do that change we'll
continue to separate the handling from the web3 transactions to make things easier to read.
-}
tokenUpdate : Config -> TokenMsg -> ( TokenState, BNTransactionState ) -> ( ( TokenState, Cmd TokenMsg ), ( BNTransactionState, Cmd msg ) )
tokenUpdate config msg ( { cTokens, tokenAllowances } as state, bnState ) =
    case msg of
        SetTokenAllowance { asset, contract, customer, allowance } ->
            let
                tokenAllownceKey =
                    tokenAllowancesKey asset contract

                updatedTokenAllowances =
                    Dict.insert tokenAllownceKey allowance tokenAllowances
            in
            ( ( { state | tokenAllowances = updatedTokenAllowances }, Cmd.none )
            , ( bnState, Cmd.none )
            )

        SetInfuraEtherUSD price ->
            ( ( { state | infuraEtherPrice = Just price }, Cmd.none )
            , ( bnState, Cmd.none )
            )

        Web3TransactionMsg transactionMsg ->
            ( ( state, Cmd.none )
            , tokenTransactionUpdate config transactionMsg ( state, bnState )
            )

        Error error ->
            ( ( { state | errors = error :: state.errors }, Console.error error )
            , ( bnState, Cmd.none )
            )


{-| This update function is used to actually create the web3 transactions and submit them for user confirmation.
We split up the handling of messages that trigger the web3 confirm transaction flow and other Token messages
that may update this modules state (like balances for instance).
-}
tokenTransactionUpdate : Config -> TokenTransactionMsg -> ( TokenState, BNTransactionState ) -> ( BNTransactionState, Cmd msg )
tokenTransactionUpdate config msg ( { cTokens }, bnState ) =
    case msg of
        FaucetTokenAllocate network cTokenAddress underlyingTokenAddress customerAddress underlyingDecimals ->
            --function allocateTo(address _owner, uint256 value)
            let
                maybeCToken =
                    getCTokenByAddress cTokens (getContractAddressString cTokenAddress)

                ownerAddressResult =
                    FromEthereumUtils.customerAddressToEtherAddress customerAddress

                maybeValue =
                    maybeCToken
                        |> Maybe.map
                            (\cToken ->
                                let
                                    underlyingWeiAdjustment =
                                        BigInt.pow (BigInt.fromInt 10) (BigInt.fromInt cToken.underlying.decimals)
                                in
                                BigInt.fromInt 100
                                    |> BigInt.mul underlyingWeiAdjustment
                            )

                dataResult =
                    ownerAddressResult
                        |> Result.andThen
                            (\ownerAddress ->
                                case maybeValue of
                                    Just value ->
                                        FunctionSpec.encodeCall
                                            "allocateTo"
                                            [ Value.Address ownerAddress
                                            , Value.UInt 256 value
                                            ]

                                    Nothing ->
                                        Result.Err "Unable to create value for allocateTo"
                            )

                underlyingAssetAddressResult =
                    FromEthereumUtils.assetAddressToEtherAddress underlyingTokenAddress

                ( trx, cmd ) =
                    case ( ownerAddressResult, underlyingAssetAddressResult, dataResult ) of
                        ( Ok fromAddress, Ok toAddress, Ok data ) ->
                            let
                                amountString =
                                    maybeValue
                                        |> Maybe.map BigInt.toString
                                        |> Maybe.withDefault "—"

                                bnTransaction =
                                    BNTransaction.newTransaction network fromAddress toAddress "allocateTo" [ getContractAddressString cTokenAddress, amountString ] bnState
                            in
                            ( Just bnTransaction
                            , EtherWeb3.sendTransaction
                                (BNTransaction.getTxModule network customerAddress)
                                bnTransaction.txId
                                { from = fromAddress
                                , to = toAddress
                                , data = data
                                }
                            )

                        _ ->
                            ( Nothing, Console.log "Could not encode data for Faucet.allocateTo" )
            in
            ( BNTransaction.appendTrx bnState trx, cmd )

        FauceteerDrip network fauceteerAddress cTokenAddress underlyingTokenAddress customerAddress ->
            -- function drip(EIP20NonStandardInterface token)
            let
                underlyingAssetAddressResult =
                    FromEthereumUtils.assetAddressToEtherAddress underlyingTokenAddress

                dataResult =
                    underlyingAssetAddressResult
                        |> Result.andThen
                            (\tokenAddress ->
                                FunctionSpec.encodeCall
                                    "drip"
                                    [ Value.Address tokenAddress
                                    ]
                            )

                customerAddressResult =
                    FromEthereumUtils.customerAddressToEtherAddress customerAddress

                fauceteerAddressResult =
                    FromEthereumUtils.contractAddressToEtherAddress fauceteerAddress

                ( trx, cmd ) =
                    case ( customerAddressResult, fauceteerAddressResult, dataResult ) of
                        ( Ok fromAddress, Ok toAddress, Ok data ) ->
                            let
                                bnTransaction =
                                    BNTransaction.newTransaction network fromAddress toAddress "drip" [ getContractAddressString cTokenAddress ] bnState
                            in
                            ( Just bnTransaction
                            , EtherWeb3.sendTransaction
                                (BNTransaction.getTxModule network customerAddress)
                                bnTransaction.txId
                                { from = fromAddress
                                , to = toAddress
                                , data = data
                                }
                            )

                        _ ->
                            ( Nothing, Console.log "Could not encode data for Fauceteer.drip" )
            in
            ( BNTransaction.appendTrx bnState trx, cmd )

        FaucetTokenApprove network cTokenAddress underlyingAssetAddress customerAddress yesOrNo ->
            -- function approve(address spender, uint256 amount) external returns (bool success)
            let
                cTokenAddressResult =
                    FromEthereumUtils.contractAddressToEtherAddress cTokenAddress

                amountBigInt =
                    if yesOrNo then
                        CompoundComponents.Ether.Helpers.negativeOne

                    else
                        BigInt.fromInt 0

                dataResult =
                    cTokenAddressResult
                        |> Result.andThen
                            (\spenderAddress ->
                                FunctionSpec.encodeCall
                                    "approve"
                                    [ Value.Address spenderAddress
                                    , Value.UInt 256 amountBigInt
                                    ]
                            )

                customerAddressResult =
                    FromEthereumUtils.customerAddressToEtherAddress customerAddress

                underlyingAssetAddressResult =
                    FromEthereumUtils.assetAddressToEtherAddress underlyingAssetAddress

                ( trx, cmd ) =
                    case ( customerAddressResult, underlyingAssetAddressResult, dataResult ) of
                        ( Ok fromAddress, Ok toAddress, Ok data ) ->
                            let
                                cTokenAddressString =
                                    Ethereum.getContractAddressString cTokenAddress

                                bnTransaction =
                                    BNTransaction.newTransaction network fromAddress toAddress "approve" [ cTokenAddressString ] bnState
                            in
                            ( Just bnTransaction
                            , EtherWeb3.sendTransaction
                                (BNTransaction.getTxModule network customerAddress)
                                bnTransaction.txId
                                { from = fromAddress
                                , to = toAddress
                                , data = data
                                }
                            )

                        _ ->
                            ( Nothing, Console.log "Could not encode data for Token.approve" )
            in
            ( BNTransaction.appendTrx bnState trx, cmd )


tokenSubscriptions : TokenState -> Sub TokenMsg
tokenSubscriptions state =
    Sub.batch
        [ giveTokenAllowance (handleError (Json.Decode.errorToString >> Error) SetTokenAllowance)
        ]


clearTokenState : TokenState -> TokenState
clearTokenState state =
    { state | tokenAllowances = Dict.empty }


isCEtherToken : Config -> CToken -> Bool
isCEtherToken config cToken =
    config.cEtherToken.address == cToken.contractAddress


isCAPFactoryApproved : Config -> TokenState -> Bool
isCAPFactoryApproved config tokenState =
    let
        capFactoryAllowance =
            case ( config.maybeCrowdProposalFactory, config.maybeCompToken ) of
                ( Just capFactory, Just compToken ) ->
                    let
                        tokenAllownceKey =
                            tokenAllowancesKey (contractAddressToAssetAddress compToken.address) capFactory
                    in
                    Dict.get tokenAllownceKey tokenState.tokenAllowances
                        |> Maybe.withDefault Decimal.zero

                _ ->
                    Decimal.zero
    in
    Decimal.gte capFactoryAllowance (Decimal.fromInt 100)



-- FUNCTIONS


getTokenAddress : Token -> String
getTokenAddress token =
    case token.assetAddress of
        Asset assetAddress ->
            assetAddress


getCTokenAddress : CToken -> String
getCTokenAddress cToken =
    case cToken.contractAddress of
        Contract contractAddress ->
            contractAddress


getCTokenByAddress : CTokenSet -> String -> Maybe CToken
getCTokenByAddress cTokens assetAddress =
    let
        cTokensList =
            cTokens
                |> Dict.values

        matchingCTokensList =
            cTokensList
                |> List.filter (\cToken -> getCTokenAddress cToken == assetAddress)
    in
    List.head matchingCTokensList


getUnderlyingTokenByAddress : CTokenSet -> String -> Maybe Token
getUnderlyingTokenByAddress cTokens assetAddress =
    let
        underlyingTokensList =
            cTokens
                |> Dict.values
                |> List.map .underlying

        matchingUnderlyingTokensList =
            underlyingTokensList
                |> List.filter (\underlyingToken -> getTokenAddress underlyingToken == assetAddress)
    in
    List.head matchingUnderlyingTokensList


getUnderlyingTokenSymbol : CTokenSet -> String -> Maybe String
getUnderlyingTokenSymbol cTokens assetAddress =
    getCTokenByAddress cTokens assetAddress
        |> maybeMap .underlying
        |> maybeMap .symbol


getUnderlyingTokenDecimals : CTokenSet -> String -> Maybe Int
getUnderlyingTokenDecimals cTokens assetAddress =
    getCTokenByAddress cTokens assetAddress
        |> maybeMap .underlying
        |> maybeMap .decimals



-- PORTS
-- Ask for token allowance (now only used by CAP Factory)


port askTokenAllowanceTokenPort : { blockNumber : Int, assetAddress : String, contractAddress : String, customerAddress : String, decimals : Int } -> Cmd msg


askTokenAllowance : Int -> AssetAddress -> ContractAddress -> CustomerAddress -> Int -> Cmd msg
askTokenAllowance blockNumber (Asset assetAddress) (Contract contractAddress) (Customer customerAddress) decimals =
    askTokenAllowanceTokenPort
        { blockNumber = blockNumber
        , assetAddress = assetAddress
        , contractAddress = contractAddress
        , customerAddress = customerAddress
        , decimals = decimals
        }


port giveTokenAllowanceTokenPort : (Value -> msg) -> Sub msg


giveTokenAllowance : (Result Json.Decode.Error TokenAllowance -> msg) -> Sub msg
giveTokenAllowance wrapper =
    let
        decoder =
            Json.Decode.map4 TokenAllowance
                (field "assetAddress" decodeAssetAddress)
                (field "contractAddress" decodeContractAddress)
                (field "customerAddress" decodeCustomerAddress)
                (field "allowance" decimal)
    in
    giveTokenAllowanceTokenPort
        (decodeValue decoder >> wrapper)
