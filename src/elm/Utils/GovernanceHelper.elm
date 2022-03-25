module Utils.GovernanceHelper exposing
    ( AllTransactions
    , GovernanceHelperData
    , Proposal
    , TimelockTransaction
    , TransactionData
    , abiDecoder
    , governanceHelperDataFromConfig
    , governanceProposalDecoder
    , humanReadableTimelockAction
    , proposalThreshold
    , timelockTrxDecoder
    )

import Array
import CompoundApi.Governance.ProposalService.Decoders exposing (proposalStateDecoder)
import CompoundApi.Governance.ProposalService.Models exposing (ProposalState)
import CompoundComponents.DecoderHelper exposing (andMap)
import CompoundComponents.Eth.Decoders exposing (stringDecimal)
import CompoundComponents.Eth.Ethereum exposing (getContractAddressString)
import CompoundComponents.Eth.Network as Network exposing (Network(..))
import CompoundComponents.Utils.NumberFormatter exposing (formatToDecimalPlaces)
import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Eth.Config exposing (Config)
import Eth.FunctionArg exposing (FunctionArg)
import Json.Decode exposing (Decoder, andThen, bool, field, int, list, map2, map3, map7, oneOf, string, succeed)
import Strings.Translations as Translations
import Utils.ABIHelper exposing (ABIInputOutputType, ABIValue)


proposalThreshold : Network -> Decimal
proposalThreshold network =
    if network == Network.MainNet then
        Decimal.fromInt 25000

    else
        Decimal.fromInt 100000


type alias TransactionData =
    { functionName : String
    , functionArgs : List String
    , functionCall : String
    , blockHash : String
    , blockNumber : Int
    , transactionHash : String
    , timelockTrxNumber : Int
    }


type alias TimelockTransaction =
    { txHash : String
    , target : String
    , value : String
    , signature : String
    , data : String
    , eta : String
    , transactionData : TransactionData
    }


type alias ActionData =
    { functionName : String
    , functionArgs : List String
    , functionCall : String
    }


type alias Proposal =
    { id : Int
    , transactionHash : String
    , title : String
    , description : String
    , proposer : String
    , eta : String
    , startBlock : String
    , endBlock : String
    , for_votes : Decimal
    , against_votes : Decimal
    , canceled : Bool
    , executed : Bool
    , states : List ProposalState
    , targets : List String
    , values : List String
    , signatures : List String
    , calldatas : List String
    , actionsData : List ActionData
    }


type alias AllTransactions =
    { queuedTransactions : List TimelockTransaction
    , executedTransactions : List TimelockTransaction
    }


type alias GovernanceHelperData =
    { contractAddressToName : Dict String String
    , cTokenToUnderlyingDecimals : Dict String Int
    , cTokenToUnderlyingSymbol : Dict String String
    , tokensToDecimals : Dict String Int
    }


abiDecoder : Decoder ABIValue
abiDecoder =
    map7 ABIValue
        (oneOf
            [ field "inputs"
                (list
                    (map2 ABIInputOutputType
                        (field "name" string)
                        (field "type" string)
                    )
                )
            , succeed []
            ]
        )
        (oneOf [ field "name" string, succeed "" ])
        (oneOf
            [ field "outputs"
                (list
                    (map2 ABIInputOutputType
                        (field "name" string)
                        (field "type" string)
                    )
                )
            , succeed []
            ]
        )
        (oneOf [ field "payable" bool, succeed False ])
        (oneOf [ field "stateMutability" string, succeed "" ])
        (field "type" string)
        (oneOf [ field "signature" string, succeed "" ])


timelockTrxDecoder : Decoder TimelockTransaction
timelockTrxDecoder =
    map7 TimelockTransaction
        (field "txHash" string)
        (field "target" string)
        (field "value" string)
        (field "signature" string)
        (field "data" string)
        (field "eta" string)
        (field "transactionData"
            (map7 TransactionData
                (field "functionName" string)
                (field "functionArgs" (list string))
                (field "functionCall" string)
                (field "blockHash" string)
                (field "blockNumber" int)
                (field "transactionHash" string)
                (field "timelockTrxNumber" int)
            )
        )


governanceProposalDecoder : Decoder Proposal
governanceProposalDecoder =
    succeed Proposal
        |> andMap (field "id" int)
        |> andMap (field "transactionHash" string)
        |> andMap (field "title" string)
        |> andMap (field "description" string)
        |> andMap (field "proposer" string)
        |> andMap (field "eta" string)
        |> andMap (field "startBlock" string)
        |> andMap (field "endBlock" string)
        |> andMap (field "forVotes" stringDecimal)
        |> andMap (field "againstVotes" stringDecimal)
        |> andMap (field "canceled" bool)
        |> andMap (field "executed" bool)
        |> andMap (field "states" (list proposalStateDecoder))
        |> andMap (field "targets" (list string))
        |> andMap (field "values" (list string))
        |> andMap (field "signatures" (list string))
        |> andMap (field "calldatas" (list string))
        |> andMap
            (field "actionsData"
                (list
                    (map3 ActionData
                        (field "functionName" string)
                        (field "functionArgs" (list string))
                        (field "functionCall" string)
                    )
                )
            )


governanceHelperDataFromConfig : Maybe Config -> GovernanceHelperData
governanceHelperDataFromConfig maybeConfig =
    case maybeConfig of
        Just config ->
            let
                timelock : List ( String, String )
                timelock =
                    case config.maybeTimelock of
                        Just timelockEntry ->
                            [ ( getContractAddressString timelockEntry, "Timelock" ) ]

                        Nothing ->
                            []

                cErc20Delegate : List ( String, String )
                cErc20Delegate =
                    case config.maybeCErc20Delegate of
                        Just cErc20DelegateEntry ->
                            [ ( getContractAddressString cErc20DelegateEntry, "CErc20Delegate" ) ]

                        Nothing ->
                            []

                cDaiDelegate : List ( String, String )
                cDaiDelegate =
                    case config.maybeCDaiDelegate of
                        Just cDaiDelegateEntry ->
                            [ ( getContractAddressString cDaiDelegateEntry, "CDaiDelegate" ) ]

                        Nothing ->
                            []

                contractAddressToName : Dict String String
                contractAddressToName =
                    Dict.fromList
                        ([ ( getContractAddressString config.comptroller, "Comptroller" )
                         , ( getContractAddressString config.priceOracle, "PriceOracle" )
                         ]
                            ++ (config.cTokens
                                    |> Dict.toList
                                    |> List.map
                                        (\( cTokenSymbol, cTokenConfig ) ->
                                            let
                                                addressString =
                                                    getContractAddressString cTokenConfig.address

                                                underlyingAddress =
                                                    getContractAddressString cTokenConfig.underlying.address
                                            in
                                            [ ( addressString, cTokenSymbol ), ( underlyingAddress, cTokenConfig.underlying.symbol ) ]
                                        )
                                    |> List.concat
                               )
                            ++ timelock
                            ++ cErc20Delegate
                            ++ cDaiDelegate
                        )

                cTokenToUnderlyingDecimals : Dict String Int
                cTokenToUnderlyingDecimals =
                    Dict.fromList
                        (config.cTokens
                            |> Dict.toList
                            |> List.map
                                (\( cTokenSymbol, cTokenConfig ) ->
                                    let
                                        cTokenAddressString =
                                            getContractAddressString cTokenConfig.address

                                        underlyingDecimals =
                                            cTokenConfig.underlying.decimals
                                    in
                                    [ ( cTokenSymbol, underlyingDecimals ), ( cTokenAddressString, underlyingDecimals ) ]
                                )
                            |> List.concat
                        )

                cTokenToUnderlyingSymbol : Dict String String
                cTokenToUnderlyingSymbol =
                    Dict.fromList
                        (config.cTokens
                            |> Dict.toList
                            |> List.map
                                (\( cTokenSymbol, cTokenConfig ) ->
                                    let
                                        cTokenAddressString =
                                            getContractAddressString cTokenConfig.address

                                        underlyingSymbol =
                                            cTokenConfig.underlying.symbol
                                    in
                                    [ ( cTokenSymbol, underlyingSymbol ), ( cTokenAddressString, underlyingSymbol ) ]
                                )
                            |> List.concat
                        )

                tokenToDecimals : Dict String Int
                tokenToDecimals =
                    Dict.fromList
                        (config.cTokens
                            |> Dict.toList
                            |> List.map
                                (\( cTokenSymbol, cTokenConfig ) ->
                                    let
                                        cTokenAddressString =
                                            getContractAddressString cTokenConfig.address

                                        underlyingAddressString =
                                            getContractAddressString cTokenConfig.underlying.address
                                    in
                                    [ ( cTokenSymbol, cTokenConfig.decimals )
                                    , ( cTokenAddressString, cTokenConfig.decimals )
                                    , ( cTokenConfig.underlying.symbol, cTokenConfig.underlying.decimals )
                                    , ( underlyingAddressString, cTokenConfig.underlying.decimals )
                                    ]
                                )
                            |> List.concat
                        )
            in
            { contractAddressToName = contractAddressToName
            , cTokenToUnderlyingDecimals = cTokenToUnderlyingDecimals
            , cTokenToUnderlyingSymbol = cTokenToUnderlyingSymbol
            , tokensToDecimals = tokenToDecimals
            }

        Nothing ->
            { contractAddressToName = Dict.empty
            , cTokenToUnderlyingDecimals = Dict.empty
            , cTokenToUnderlyingSymbol = Dict.empty
            , tokensToDecimals = Dict.empty
            }


oneMantissa : Float
oneMantissa =
    1000000000000000000


percentage : Float -> String
percentage value =
    String.fromFloat (value / oneMantissa * 100)


finalAmount : Float -> Maybe Int -> String
finalAmount amount maybeDecimals =
    case maybeDecimals of
        Just decimals ->
            let
                baseUnit =
                    toFloat (10 ^ decimals)
            in
            formatToDecimalPlaces 0 True (Decimal.fromInt (round (amount / baseUnit)))

        Nothing ->
            String.fromFloat amount


humanReadableTimelockAction : Translations.Lang -> GovernanceHelperData -> String -> String -> String -> List String -> ( Bool, String, String )
humanReadableTimelockAction userLanguage governanceHelperData target value functionName functionArgsText =
    let
        contractNameOrAddress lookupAddress =
            let
                lowercaseAddress =
                    String.toLower lookupAddress
            in
            Dict.get lowercaseAddress governanceHelperData.contractAddressToName
                |> Maybe.withDefault lowercaseAddress

        maybeHumanReadableString =
            case functionName of
                "_acceptAdmin" ->
                    Just (Translations.accept_admin userLanguage (contractNameOrAddress target))

                "_acceptImplementation" ->
                    Just (Translations.accept_implementation userLanguage)

                "_reduceReserves" ->
                    let
                        maybeAmount =
                            functionArgsText
                                |> List.head
                                |> Maybe.andThen String.toFloat

                        maybeUnderlyingSymbol =
                            Dict.get (String.toLower target) governanceHelperData.cTokenToUnderlyingSymbol
                    in
                    case ( maybeAmount, maybeUnderlyingSymbol ) of
                        ( Just amount, Just underlyingSymbol ) ->
                            let
                                contractName =
                                    contractNameOrAddress target

                                maybeDecimals =
                                    Dict.get (String.toLower target) governanceHelperData.cTokenToUnderlyingDecimals

                                reduceAmount =
                                    finalAmount amount maybeDecimals ++ " " ++ underlyingSymbol
                            in
                            Just (Translations.reduce_reserves userLanguage contractName reduceAmount)

                        _ ->
                            Just (Translations.reduce_reserves_basic userLanguage (contractNameOrAddress target))

                "_setCloseFactor" ->
                    let
                        maybeValue =
                            functionArgsText
                                |> List.head
                                |> Maybe.andThen String.toFloat
                    in
                    case maybeValue of
                        Just value_ ->
                            Just (Translations.set_close_factor userLanguage (percentage value_))

                        Nothing ->
                            Just (Translations.set_close_factor_basic userLanguage)

                "_setCollateralFactor" ->
                    let
                        maybeContract =
                            functionArgsText
                                |> List.head

                        maybeValue =
                            functionArgsText
                                |> List.reverse
                                |> List.head
                                |> Maybe.andThen String.toFloat
                    in
                    case ( maybeContract, maybeValue ) of
                        ( Just contract, Just value_ ) ->
                            Just (Translations.set_collateral_factor userLanguage (contractNameOrAddress contract) (percentage value_))

                        _ ->
                            Just (Translations.set_collateral_factor_basic userLanguage)

                "_setImplementation" ->
                    Just (Translations.set_implementation userLanguage (contractNameOrAddress target))

                "_setInterestRateModel" ->
                    Just (Translations.set_interest_rate_model userLanguage (contractNameOrAddress target))

                "_setPendingImplementation" ->
                    Just (Translations.set_pending_implementation userLanguage)

                "_setPriceOracle" ->
                    Just (Translations.set_price_oracle userLanguage)

                "_setReserveFactor" ->
                    let
                        maybeValue =
                            functionArgsText
                                |> List.head
                                |> Maybe.andThen String.toFloat
                    in
                    case maybeValue of
                        Just value_ ->
                            Just (Translations.set_reserve_factor userLanguage (contractNameOrAddress target) (percentage value_))

                        Nothing ->
                            Just (Translations.set_reserve_factor_basic userLanguage (contractNameOrAddress target))

                "_supportMarket" ->
                    let
                        assetToSupport =
                            functionArgsText
                                |> List.head
                                |> Maybe.withDefault "a new asset"
                    in
                    Just (Translations.support_asset userLanguage (contractNameOrAddress assetToSupport))

                "transfer" ->
                    let
                        maybeContract =
                            functionArgsText
                                |> List.head

                        maybeAmount =
                            functionArgsText
                                |> List.reverse
                                |> List.head
                                |> Maybe.andThen String.toFloat
                    in
                    case ( maybeContract, maybeAmount ) of
                        ( Just contract, Just amount ) ->
                            let
                                maybeDecimals =
                                    Dict.get (String.toLower target) governanceHelperData.tokensToDecimals
                            in
                            Just (Translations.transfer userLanguage (finalAmount amount maybeDecimals) (contractNameOrAddress target) (contractNameOrAddress contract))

                        _ ->
                            Just (Translations.transfer_basic userLanguage (contractNameOrAddress target))

                "" ->
                    let
                        maybeValueFloat =
                            String.toFloat value
                    in
                    case maybeValueFloat of
                        Just valueFloat ->
                            Just (Translations.transfer userLanguage (finalAmount valueFloat (Just 18)) "ETH" target)

                        _ ->
                            Just (Translations.transfer_basic userLanguage "ETH")

                _ ->
                    Nothing

        functionCall =
            contractNameOrAddress target ++ "." ++ functionName ++ "(" ++ String.toLower (String.join ", " functionArgsText) ++ ")"
    in
    case maybeHumanReadableString of
        Just string ->
            ( True, string, functionCall )

        Nothing ->
            ( False, "", functionCall )
