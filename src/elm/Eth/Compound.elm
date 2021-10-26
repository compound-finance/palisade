port module Eth.Compound exposing
    ( CTokenBalances
    , CTokenBalancesDict
    , CTokenInterestBalances
    , CTokenMetadata
    , CTokenMetadataDict
    , CompoundMsg(..)
    , CompoundState
    , CompoundTransactionMsg(..)
    , cTokenIsApproved
    , cTokenIsLoaded
    , clearCompoundState
    , compoundInit
    , compoundNewBlockCmd
    , compoundSubscriptions
    , compoundUpdate
    , handleAccountLiquidityCalculation
    )

import BigInt
import CompoundApi.Presidio.Accounts.Decoders exposing (accountsResponseDecoder)
import CompoundApi.Presidio.Accounts.Models exposing (AccountResponse)
import CompoundApi.Presidio.Accounts.Urls
import CompoundComponents.Console as Console
import CompoundComponents.Eth.Decoders exposing (decimal, decodeAssetAddress, decodeContractAddress, decodeCustomerAddress)
import CompoundComponents.Eth.Ethereum exposing (Account(..), AssetAddress(..), ContractAddress(..), CustomerAddress(..), getContractAddressString, getCustomerAddressString)
import CompoundComponents.Eth.Network exposing (Network)
import CompoundComponents.Eth.TokenMath as TokenMath
import CompoundComponents.Ether.BNTransaction exposing (BNTransactionState)
import CompoundComponents.Ether.Helpers as EtherHelpers
import CompoundComponents.Functions exposing (handleError)
import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Eth.Config exposing (CTokenConfig, Config)
import Eth.Oracle exposing (OracleState)
import Eth.Token exposing (CToken, TokenState, isCEtherToken)
import Ether.Contracts.CToken as CTokenContract
import Ether.Contracts.Comptroller as ComptrollerContract
import Ether.Contracts.Maximillion as MaximillionContract
import Http
import Json.Decode exposing (Value, bool, decodeValue, field, int)
import Utils.Http



-- APP


type alias CTokenBalances =
    { customerAddress : CustomerAddress
    , cTokenWalletBalance : Decimal
    , underlyingAssetAddress : AssetAddress
    , underlyingBorrowBalance : Decimal
    , underlyingSupplyBalance : Decimal
    , underlyingTokenWalletBalance : Decimal
    , underlyingTokenAllowance : Decimal
    }


type alias CTokenBalancesDict =
    Dict String CTokenBalances


type alias CTokenInterestBalances =
    { underlyingBorrowInterestPaid : Maybe Decimal
    , underlyingSupplyInterestEarned : Maybe Decimal
    }


type alias CTokenInterestBalancesDict =
    Dict String CTokenInterestBalances


type alias CTokenMetadata =
    { exchangeRate : Decimal
    , borrowRate : Decimal
    , supplyRate : Decimal
    , collateralFactor : Decimal
    , reserveFactor : Decimal
    , totalBorrows : Decimal
    , totalUnderlyingCash : Decimal
    , totalReserves : Decimal
    , totalSupply : Decimal
    , totalSupplyUnderlying : Decimal
    , compSupplySpeedPerBlock : Decimal
    , compSupplySpeedPerDay : Decimal
    , compBorrowSpeedPerBlock: Decimal
    , compBorrowSpeedPerDay : Decimal
    , borrowCap : Decimal
    }


type alias CTokenMetadataDict =
    Dict String CTokenMetadata


type alias CompoundState =
    { balances : CTokenBalancesDict
    , interestBalances : CTokenInterestBalancesDict
    , cTokensMetadata : CTokenMetadataDict
    , maybeCollateralRatio : Maybe Decimal
    , maybeAccountLiquidityUsd : Maybe Decimal
    , maybeAccountShortfallUsd : Maybe Decimal
    , maybeAssetsIn : Maybe (List ContractAddress)
    , maybeTrxCount : Maybe Int
    , maybeCloseFactor : Maybe Decimal
    , maybeLiquidationIncentive : Maybe Decimal
    , maybeAccountLiquidityEth : Maybe Decimal
    , maybeAccountShortfallEth : Maybe Decimal
    , errors : List String
    }


type alias CTokenMetadataUpdate =
    { cTokenAddress : ContractAddress
    , exchangeRate : Decimal
    , supplyRatePerDay : Decimal
    , borrowRatePerDay : Decimal
    , collateralFactor : Decimal
    , reserveFactor : Decimal
    , totalBorrows : Decimal
    , totalUnderlyingCash : Decimal
    , totalReserves : Decimal
    , totalSupply : Decimal
    , totalSupplyUnderlying : Decimal
    , compSupplySpeedPerBlock : Decimal
    , compSupplySpeedPerDay : Decimal
    , compBorrowSpeedPerBlock: Decimal
    , compBorrowSpeedPerDay : Decimal
    , borrowCap : Decimal
    }


type alias CTokenBalanceUpdate =
    { cTokenAddress : ContractAddress
    , customerAddress : CustomerAddress
    , cTokenWalletBalance : Decimal
    , underlyingAssetAddress : AssetAddress
    , underlyingBorrowBalance : Decimal
    , underlyingSupplyBalance : Decimal
    , underlyingTokenWalletBalance : Decimal
    , underlyingTokenAllowance : Decimal
    }


type alias AccountLimits =
    { customerAddress : CustomerAddress
    , accountLiquidity : Decimal
    , accountShortfall : Decimal
    , assetsIn : List ContractAddress
    , trxCount : Int
    , closeFactor : Decimal
    , liquidationIncentive : Decimal
    }


compoundInit : ( CompoundState, Cmd CompoundMsg )
compoundInit =
    ( { balances = Dict.empty
      , interestBalances = Dict.empty
      , cTokensMetadata = Dict.empty
      , maybeCollateralRatio = Nothing
      , maybeAccountLiquidityUsd = Nothing
      , maybeAccountShortfallUsd = Nothing
      , maybeAssetsIn = Nothing
      , maybeTrxCount = Nothing
      , maybeCloseFactor = Nothing
      , maybeLiquidationIncentive = Nothing
      , maybeAccountLiquidityEth = Nothing
      , maybeAccountShortfallEth = Nothing
      , errors = []
      }
    , Cmd.none
    )


type CompoundMsg
    = SetAllCTokenMetadatas (List CTokenMetadataUpdate)
    | SetAllCTokenBalances (List CTokenBalanceUpdate)
    | SetAccountLimits AccountLimits
    | PresidioAccountResponse (Result Http.Error AccountResponse)
    | Web3TransactionMsg CompoundTransactionMsg
    | Error String


type CompoundTransactionMsg
    = CTokenBorrow Network ContractAddress Int CustomerAddress Decimal
    | CTokenMint Network ContractAddress Int CustomerAddress Decimal
    | CTokenRedeem Network ContractAddress Decimal Int Int CustomerAddress Decimal
    | CTokenRepayBorrow Network ContractAddress Int CustomerAddress Decimal
    | EnterMarkets Network ContractAddress (List ContractAddress) CustomerAddress
    | ExitMarket Network ContractAddress ContractAddress CustomerAddress


compoundNewBlockCmd : Int -> Dict String String -> Network -> ContractAddress -> Account -> Config -> Cmd CompoundMsg
compoundNewBlockCmd blockNumber apiBaseUrlMap network comptroller account config =
    let
        cTokenConfigs =
            Dict.values config.cTokens

        accountRequiredCmds =
            case account of
                Acct customerAddress _ ->
                    [ askCustomerBalances apiBaseUrlMap (Just network) blockNumber customerAddress cTokenConfigs config.compoundLens
                    , askAccountLimits blockNumber comptroller customerAddress config.compoundLens
                    ]

                UnknownAcct ->
                    []

                NoAccount ->
                    []
    in
    Cmd.batch <|
        askCTokenMetadata blockNumber config cTokenConfigs
            :: accountRequiredCmds



--TODO: We can remove this after we are fully onboard with new price


handleAccountLiquidityCalculation : OracleState -> CompoundState -> CompoundState
handleAccountLiquidityCalculation oracleState compoundState =
    case oracleState.isPriceFeedOracle of
        ( Just False, Just etherPrice ) ->
            let
                updatedAccountLiquidityUsd =
                    compoundState.maybeAccountLiquidityEth
                        |> Maybe.map (Decimal.mul etherPrice)

                updatedAccountShortfallUsd =
                    compoundState.maybeAccountShortfallEth
                        |> Maybe.map (Decimal.mul etherPrice)
            in
            { compoundState
                | maybeAccountLiquidityUsd = updatedAccountLiquidityUsd
                , maybeAccountShortfallUsd = updatedAccountShortfallUsd
            }

        _ ->
            compoundState


compoundUpdate : Config -> TokenState -> OracleState -> CompoundMsg -> ( CompoundState, BNTransactionState ) -> ( ( CompoundState, Cmd CompoundMsg ), ( BNTransactionState, Cmd msg ) )
compoundUpdate config tokenState oracleState msg ( state, bnState ) =
    case msg of
        SetAllCTokenMetadatas cTokenMetadataList ->
            let
                pow365 num =
                    (Decimal.toFloat num ^ 365)
                        |> Decimal.fromFloat
                        |> Maybe.withDefault Decimal.zero

                -- APY calcuation is: ( 1 + RATE_PER_DAY )^365 - 1
                -- Where RATE_PER_DAY = 5760 * supplyRatePerBlock / 1e18
                apyRate ratePerDay =
                    Decimal.sub (pow365 (Decimal.add Decimal.one ratePerDay)) Decimal.one

                updatedMetaData =
                    cTokenMetadataList
                        |> List.foldl
                            (\{ cTokenAddress, exchangeRate, supplyRatePerDay, borrowRatePerDay, collateralFactor, reserveFactor, totalBorrows, totalUnderlyingCash, totalReserves, totalSupply, totalSupplyUnderlying, compSupplySpeedPerBlock, compSupplySpeedPerDay, compBorrowSpeedPerBlock, compBorrowSpeedPerDay, borrowCap } acc ->
                                Dict.insert
                                    (getContractAddressString cTokenAddress)
                                    { exchangeRate = exchangeRate
                                    , borrowRate = apyRate borrowRatePerDay
                                    , supplyRate = apyRate supplyRatePerDay
                                    , collateralFactor = collateralFactor
                                    , reserveFactor = reserveFactor
                                    , totalBorrows = totalBorrows
                                    , totalUnderlyingCash = totalUnderlyingCash
                                    , totalReserves = totalReserves
                                    , totalSupply = totalSupply
                                    , totalSupplyUnderlying = totalSupplyUnderlying
                                    , compSupplySpeedPerBlock = compSupplySpeedPerBlock
                                    , compSupplySpeedPerDay = compSupplySpeedPerDay
                                    , compBorrowSpeedPerBlock = compBorrowSpeedPerBlock
                                    , compBorrowSpeedPerDay = compBorrowSpeedPerDay
                                    , borrowCap = borrowCap
                                    }
                                    acc
                            )
                            state.cTokensMetadata
            in
            ( ( { state | cTokensMetadata = updatedMetaData }, Cmd.none )
            , ( bnState, Cmd.none )
            )

        SetAllCTokenBalances cTokenBalancesList ->
            let
                updatedBalances =
                    cTokenBalancesList
                        |> List.foldl
                            (\{ cTokenAddress, customerAddress, cTokenWalletBalance, underlyingAssetAddress, underlyingBorrowBalance, underlyingSupplyBalance, underlyingTokenWalletBalance, underlyingTokenAllowance } acc ->
                                --TODO: Should compare the current account addres this the account address coming in here...
                                Dict.insert
                                    (getContractAddressString cTokenAddress)
                                    { customerAddress = customerAddress
                                    , cTokenWalletBalance = cTokenWalletBalance
                                    , underlyingAssetAddress = underlyingAssetAddress
                                    , underlyingBorrowBalance = underlyingBorrowBalance
                                    , underlyingSupplyBalance = underlyingSupplyBalance
                                    , underlyingTokenWalletBalance = underlyingTokenWalletBalance
                                    , underlyingTokenAllowance = underlyingTokenAllowance
                                    }
                                    acc
                            )
                            state.balances
            in
            ( ( { state | balances = updatedBalances }, Cmd.none )
            , ( bnState, Cmd.none )
            )

        SetAccountLimits { accountLiquidity, accountShortfall, assetsIn, trxCount, closeFactor, liquidationIncentive } ->
            let
                ( maybeAccountLiquidityUsd, maybeAccountShortfallUsd ) =
                    case oracleState.isPriceFeedOracle of
                        ( Just False, Just etherPrice ) ->
                            --If we are not using a price feed oracle then technically the liquidity coming back is in Eth.
                            ( Decimal.mul accountLiquidity etherPrice
                                |> Just
                            , Decimal.mul accountShortfall etherPrice
                                |> Just
                            )

                        ( Just False, Nothing ) ->
                            ( Nothing, Nothing )

                        _ ->
                            ( Just accountLiquidity, Just accountShortfall )

                ( maybeAccountLiquidityEth, maybeAccountShortfallEth ) =
                    case oracleState.isPriceFeedOracle of
                        ( Just True, _ ) ->
                            ( Nothing, Nothing )

                        ( Just False, _ ) ->
                            ( Just accountLiquidity, Just accountShortfall )

                        _ ->
                            ( Just accountLiquidity, Just accountShortfall )

                updatedState =
                    { state
                        | maybeAccountLiquidityUsd = maybeAccountLiquidityUsd
                        , maybeAccountShortfallUsd = maybeAccountShortfallUsd
                        , maybeAssetsIn = Just assetsIn
                        , maybeTrxCount = Just trxCount
                        , maybeCloseFactor = Just closeFactor
                        , maybeLiquidationIncentive = Just liquidationIncentive
                        , maybeAccountLiquidityEth = maybeAccountLiquidityEth
                        , maybeAccountShortfallEth = maybeAccountShortfallEth
                    }
            in
            ( ( updatedState, Cmd.none )
            , ( bnState, Cmd.none )
            )

        PresidioAccountResponse result ->
            case result of
                Ok accountResponse ->
                    let
                        maybeFirstAccount =
                            List.head accountResponse.accounts

                        updatedInterestBalances =
                            case maybeFirstAccount of
                                Just firstAccount ->
                                    List.foldl
                                        (\accountCToken ->
                                            let
                                                interestData =
                                                    { underlyingBorrowInterestPaid = accountCToken.lifetime_borrow_interest_accrued
                                                    , underlyingSupplyInterestEarned = accountCToken.lifetime_supply_interest_accrued
                                                    }
                                            in
                                            Dict.insert accountCToken.address interestData
                                        )
                                        Dict.empty
                                        firstAccount.tokens

                                Nothing ->
                                    Dict.empty
                    in
                    ( ( { state | interestBalances = updatedInterestBalances }, Cmd.none )
                    , ( bnState, Cmd.none )
                    )

                Err errMsg ->
                    ( ( state, Console.error ("Error getting account values from Account API, " ++ Utils.Http.showError errMsg) )
                    , ( bnState, Cmd.none )
                    )

        Web3TransactionMsg transactionMsg ->
            ( ( state, Cmd.none )
            , compoundTransactionUpdate config tokenState transactionMsg ( state, bnState )
            )

        Error error ->
            ( ( { state | errors = error :: state.errors }, Console.error error )
            , ( bnState, Cmd.none )
            )


compoundTransactionUpdate : Config -> TokenState -> CompoundTransactionMsg -> ( CompoundState, BNTransactionState ) -> ( BNTransactionState, Cmd msg )
compoundTransactionUpdate config { cTokens } transactionMsg ( state, bnState ) =
    case transactionMsg of
        CTokenMint network cTokenAddress underlyingTokenDecimals customerAddress amount ->
            let
                maybeUnderlyingAmountWei =
                    TokenMath.getTokenWei amount underlyingTokenDecimals
                        |> Decimal.toString
                        |> BigInt.fromString
            in
            case maybeUnderlyingAmountWei of
                Just underlyingAmountWei ->
                    CTokenContract.mint config network customerAddress cTokenAddress underlyingAmountWei bnState

                Nothing ->
                    ( bnState, Console.log "Unable to create underlying wei value for CToken.mint" )

        CTokenRedeem network cTokenAddress exchangeRate cTokenDecimals underlyingTokenDecimals customerAddress underlyingAmount ->
            if Decimal.eq underlyingAmount Decimal.minusOne then
                -- If user wants to redeem -1 (aka MAX) then we should redeem their full cToken balance with redeem() instead of redeemUnderlying
                let
                    maybeCTokenBalances =
                        Dict.get (getContractAddressString cTokenAddress) state.balances
                in
                case maybeCTokenBalances of
                    Just cTokenBalances ->
                        let
                            maybeCTokenAmountWei =
                                TokenMath.getTokenWei cTokenBalances.cTokenWalletBalance cTokenDecimals
                                    |> Decimal.toString
                                    |> BigInt.fromString
                        in
                        case maybeCTokenAmountWei of
                            Just cTokenAmountWei ->
                                CTokenContract.redeem config network customerAddress cTokenAddress cTokenAmountWei bnState

                            Nothing ->
                                ( bnState, Console.log "Unable to create ctoken balance wei value for CToken.redeem" )

                    Nothing ->
                        ( bnState, Console.log "Attempted to redeem on a CToken with no balance." )

            else
                let
                    maybeUnderlyingAmountWei =
                        TokenMath.getTokenWei underlyingAmount underlyingTokenDecimals
                            |> Decimal.toString
                            |> BigInt.fromString
                in
                case maybeUnderlyingAmountWei of
                    Just underlyingAmountWei ->
                        CTokenContract.redeemUnderlying config network customerAddress cTokenAddress underlyingAmountWei bnState

                    Nothing ->
                        ( bnState, Console.log "Unable to create underlying wei value for CToken.redeemUnderlying" )

        CTokenBorrow network cTokenAddress underlyingTokenDecimals customerAddress amount ->
            let
                maybeUnderlyingAmountWei =
                    TokenMath.getTokenWei amount underlyingTokenDecimals
                        |> Decimal.toString
                        |> BigInt.fromString
            in
            case maybeUnderlyingAmountWei of
                Just underlyingAmountWei ->
                    CTokenContract.borrow config network customerAddress cTokenAddress underlyingAmountWei bnState

                Nothing ->
                    ( bnState, Console.log "Unable to create underlying wei value for CToken.borrow" )

        CTokenRepayBorrow network cTokenAddress underlyingTokenDecimals customerAddress underlyingAmount ->
            if cTokenAddress == config.cEtherToken.address then
                let
                    cEtherRepayAmount =
                        if Decimal.eq underlyingAmount Decimal.minusOne then
                            let
                                borrowBalance =
                                    state.balances
                                        |> Dict.get (getContractAddressString cTokenAddress)
                                        |> Maybe.map .underlyingBorrowBalance
                                        |> Maybe.withDefault Decimal.zero

                                tinyAdjustment =
                                    Decimal.fromString "0.0035"
                                        |> Maybe.withDefault Decimal.zero
                                        |> Decimal.mul borrowBalance

                                -- We'll pass in max as BorrowBalance + BorrowBalance * (0.0035)
                                adjustedAmount =
                                    Decimal.add borrowBalance tinyAdjustment
                            in
                            adjustedAmount

                        else
                            underlyingAmount

                    maybeRepayUnderlyingAmountWei =
                        TokenMath.getTokenWei cEtherRepayAmount underlyingTokenDecimals
                            |> Decimal.toString
                            |> BigInt.fromString
                in
                case maybeRepayUnderlyingAmountWei of
                    Just repayUnderlyingAmountWei ->
                        MaximillionContract.repayBehalf config network customerAddress cTokenAddress repayUnderlyingAmountWei bnState

                    Nothing ->
                        ( bnState, Console.log "Unable to create underlying wei value for CToken.repayBorrow" )

            else
                let
                    maybeUnderlyingAmountWei =
                        if Decimal.eq underlyingAmount Decimal.minusOne then
                            EtherHelpers.negativeOne
                                |> Just

                        else
                            TokenMath.getTokenWei underlyingAmount underlyingTokenDecimals
                                |> Decimal.toString
                                |> BigInt.fromString
                in
                case maybeUnderlyingAmountWei of
                    Just underlyingAmountWei ->
                        CTokenContract.repayBorrow config network customerAddress cTokenAddress underlyingAmountWei bnState

                    Nothing ->
                        ( bnState, Console.log "Unable to create underlying wei value for CToken.repayBorrow" )

        EnterMarkets network comptrollerAddress cTokenAddressList customerAddress ->
            ComptrollerContract.enterMarkets config network customerAddress comptrollerAddress cTokenAddressList bnState

        ExitMarket network comptrollerAddress cTokenAddress customerAddress ->
            ComptrollerContract.exitMarket config network customerAddress comptrollerAddress cTokenAddress bnState


compoundSubscriptions : Sub CompoundMsg
compoundSubscriptions =
    Sub.batch
        [ giveCTokenMetadata (handleError (Json.Decode.errorToString >> Error) SetAllCTokenMetadatas)
        , giveCTokenBalancesAllUpdate (handleError (Json.Decode.errorToString >> Error) SetAllCTokenBalances)
        , giveAccountLimits (handleError (Json.Decode.errorToString >> Error) SetAccountLimits)
        ]


clearCompoundState : CompoundState -> CompoundState
clearCompoundState state =
    { state
        | balances = Dict.empty
        , interestBalances = Dict.empty
        , maybeAccountLiquidityUsd = Nothing
        , maybeAccountShortfallUsd = Nothing
        , maybeAssetsIn = Nothing
        , maybeTrxCount = Nothing
        , maybeCloseFactor = Nothing
        , maybeLiquidationIncentive = Nothing
        , maybeAccountLiquidityEth = Nothing
        , maybeAccountShortfallEth = Nothing
    }



-- Helpers


cTokenIsLoaded : Config -> CToken -> CompoundState -> Bool
cTokenIsLoaded config cToken compoundState =
    let
        hasUnderlyingTokenAllowance =
            Dict.member (getContractAddressString cToken.contractAddress) compoundState.balances
    in
    if isCEtherToken config cToken then
        True

    else
        hasUnderlyingTokenAllowance


cTokenIsApproved : Config -> CToken -> CompoundState -> Bool
cTokenIsApproved config cToken compoundState =
    let
        ( tokenAllowance, tokenBalance ) =
            Dict.get (getContractAddressString cToken.contractAddress) compoundState.balances
                |> Maybe.map
                    (\{ underlyingTokenAllowance, underlyingTokenWalletBalance } ->
                        ( underlyingTokenAllowance, underlyingTokenWalletBalance )
                    )
                |> Maybe.withDefault ( Decimal.zero, Decimal.zero )
    in
    if isCEtherToken config cToken || Decimal.lt tokenAllowance Decimal.zero then
        True

    else
        Decimal.gt tokenAllowance Decimal.zero


askCTokenMetadata : Int -> Config -> List CTokenConfig -> Cmd CompoundMsg
askCTokenMetadata blockNumber config cTokenConfigs =
    askCTokenGetMetadataAll blockNumber (List.map .address cTokenConfigs) config.compoundLens


askCustomerBalances : Dict String String -> Maybe Network -> Int -> CustomerAddress -> List CTokenConfig -> ContractAddress -> Cmd CompoundMsg
askCustomerBalances apiBaseUrlMap maybeNetwork blockNumber account cTokenConfigs compoundLens =
    let
        maybeAccountsUrl =
            maybeNetwork
                |> Maybe.andThen
                    (\network ->
                        { addresses = [ getCustomerAddressString account ]
                        , min_borrow_value_in_eth = Nothing
                        , max_health = Nothing
                        , block_number = 0
                        , page_size = 1
                        , page_number = 1
                        }
                            |> CompoundApi.Presidio.Accounts.Urls.accountsRequestUrl apiBaseUrlMap network
                    )

        loadAccountRequestCmd =
            case maybeAccountsUrl of
                Just accountsUrl ->
                    let
                        accountsRequestHttpGet =
                            Http.get accountsUrl accountsResponseDecoder
                    in
                    [ Http.send PresidioAccountResponse accountsRequestHttpGet ]

                _ ->
                    [ Cmd.none ]

        cTokenBalancesCmds =
            [ askCTokenGetBalances blockNumber account cTokenConfigs compoundLens ]
    in
    Cmd.batch (cTokenBalancesCmds ++ loadAccountRequestCmd)



-- Get CToken metadata: exchange rate, borrow rate, collateral factor


port askCTokenMetadataAllPort : { blockNumber : Int, cTokens : List String, compoundLens : String } -> Cmd msg


askCTokenGetMetadataAll : Int -> List ContractAddress -> ContractAddress -> Cmd msg
askCTokenGetMetadataAll blockNumber cTokens (Contract compoundLens) =
    askCTokenMetadataAllPort
        { blockNumber = blockNumber
        , cTokens = List.map getContractAddressString cTokens
        , compoundLens = compoundLens
        }


port giveCTokenMetadataPort : (Value -> msg) -> Sub msg


giveCTokenMetadata : (Result Json.Decode.Error (List CTokenMetadataUpdate) -> msg) -> Sub msg
giveCTokenMetadata wrapper =
    let
        stage1 =
            Json.Decode.map8 CTokenMetadataUpdate
                (field "cTokenAddress" decodeContractAddress)
                (field "exchangeRate" decimal)
                (field "supplyRatePerDay" decimal)
                (field "borrowRatePerDay" decimal)
                (field "collateralFactor" decimal)
                (field "reserveFactor" decimal)
                (field "totalBorrows" decimal)
                (field "totalUnderlyingCash" decimal)

        stage2 =
            (Json.Decode.map6
                (<|)
                stage1
                (field "totalReserves" decimal)
                (field "totalSupply" decimal)
                (field "totalSupplyUnderlying" decimal)
                (field "compSupplySpeedPerBlock" decimal)
                (field "compSupplySpeedPerDay" decimal)
            )
        decoder =
            Json.Decode.list
                (Json.Decode.map4
                    (<|)
                    stage2
                    (field "compBorrowSpeedPerBlock" decimal)
                    (field "compBorrowSpeedPerDay" decimal)
                    (field "borrowCap" decimal)
                )
    in
    giveCTokenMetadataPort
        (decodeValue decoder >> wrapper)



-- Get customer balance


type alias CTokenPortData =
    { underlyingAssetAddress : String
    , underlyingDecimals : Int
    , cTokenDecimals : Int
    , cTokenSymbol : String
    }


port askCTokenGetBalancesPort : { blockNumber : Int, customerAddress : String, cTokens : List ( String, CTokenPortData ), compoundLens : String } -> Cmd msg


askCTokenGetBalances : Int -> CustomerAddress -> List CTokenConfig -> ContractAddress -> Cmd msg
askCTokenGetBalances blockNumber (Customer customerAddress) cTokenConfigs (Contract compoundLens) =
    let
        cTokens =
            cTokenConfigs
                |> List.map
                    (\cTokenConfig ->
                        ( getContractAddressString cTokenConfig.address
                        , { underlyingAssetAddress = getContractAddressString cTokenConfig.underlying.address
                          , underlyingDecimals = cTokenConfig.underlying.decimals
                          , cTokenDecimals = cTokenConfig.decimals
                          , cTokenSymbol = cTokenConfig.symbol
                          }
                        )
                    )
    in
    askCTokenGetBalancesPort
        { blockNumber = blockNumber
        , customerAddress = customerAddress
        , cTokens = cTokens
        , compoundLens = compoundLens
        }


port giveCTokenBalancesAllPort : (Value -> msg) -> Sub msg


giveCTokenBalancesAllUpdate : (Result Json.Decode.Error (List CTokenBalanceUpdate) -> msg) -> Sub msg
giveCTokenBalancesAllUpdate wrapper =
    let
        decoder =
            Json.Decode.list
                (Json.Decode.map8 CTokenBalanceUpdate
                    (field "cTokenAddress" decodeContractAddress)
                    (field "customerAddress" decodeCustomerAddress)
                    (field "cTokenWalletBalance" decimal)
                    (field "underlyingAssetAddress" decodeAssetAddress)
                    (field "underlyingBorrowBalance" decimal)
                    (field "underlyingSupplyBalance" decimal)
                    (field "underlyingTokenWalletBalance" decimal)
                    (field "underlyingTokenAllowance" decimal)
                )
    in
    giveCTokenBalancesAllPort
        (decodeValue decoder >> wrapper)



-- Get all accounts limits for the user: liquidity, shortfall, assetsIn, and currentTransactionCount


port askAccountLimitsPort : { blockNumber : Int, comptrollerAddress : String, customerAddress : String, compoundLens : String } -> Cmd msg


askAccountLimits : Int -> ContractAddress -> CustomerAddress -> ContractAddress -> Cmd msg
askAccountLimits blockNumber (Contract contractAddress) (Customer customerAddress) (Contract compoundLens) =
    askAccountLimitsPort
        { blockNumber = blockNumber
        , comptrollerAddress = contractAddress
        , customerAddress = customerAddress
        , compoundLens = compoundLens
        }


port giveAccountLimitsPort : (Value -> msg) -> Sub msg


giveAccountLimits : (Result Json.Decode.Error AccountLimits -> msg) -> Sub msg
giveAccountLimits wrapper =
    let
        decoder =
            Json.Decode.map7 AccountLimits
                (field "customerAddress" decodeCustomerAddress)
                (field "accountLiquidity" decimal)
                (field "accountShortfall" decimal)
                (field "assetsIn" (Json.Decode.list decodeContractAddress))
                (field "trxCount" int)
                (field "closeFactor" decimal)
                (field "liquidationIncentive" decimal)
    in
    giveAccountLimitsPort
        (decodeValue decoder >> wrapper)
