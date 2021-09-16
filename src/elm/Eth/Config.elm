module Eth.Config exposing (BasicConfig, CTokenConfig, Config, RawCTokenConfig, TokenConfig, basicConfigDecoder, getCTokenAddresses, getUnderlyingTokenAddresses, loadConfig, loadConfigs)

import CompoundComponents.Eth.Decoders exposing (decodeContractAddress)
import CompoundComponents.Eth.Ethereum exposing (AssetAddress(..), ContractAddress(..), areContractsEqual, contractAddressToAssetAddress)
import CompoundComponents.Functions as Functions
import Dict exposing (Dict)
import Json.Decode exposing (Decoder, Error, bool, decodeValue, field, int, string)
import Json.Encode


type alias BasicConfig =
    { contracts : Dict String ContractAddress
    , cTokensRaw : Dict String RawCTokenConfig
    , tokens : Dict String TokenConfig
    , interestModels : Dict String InterestRateModelConfig
    , blocks : Dict String Int
    }


type alias TokenConfig =
    { name : String
    , symbol : String
    , decimals : Int
    , address : ContractAddress
    , supported : Maybe Bool
    , reader : Maybe ContractAddress
    }


type alias CTokenConfig =
    { name : String
    , symbol : String
    , decimals : Int
    , address : ContractAddress
    , underlying : TokenConfig
    }


type alias InterestRateModelConfig =
    { contract : String
    , address : ContractAddress
    }



-- This is the config as defined in the json config file, but we want to coerce to a better form


type alias RawCTokenConfig =
    { name : String
    , symbol : String
    , decimals : Int
    , address : ContractAddress
    , underlying : Maybe ContractAddress
    }


type alias Config =
    { comptroller : ContractAddress
    , priceOracle : ContractAddress
    , maximillion : ContractAddress
    , compoundLens : ContractAddress
    , maybeFauceteer : Maybe ContractAddress
    , cEtherToken : TokenConfig --TODO: This is likey a different config entirely since we don't have an underlying.
    , cTokens : Dict String CTokenConfig
    , maybeInvertedEtherPriceAsset : Maybe ContractAddress
    , maybeGovernor : Maybe ( ContractAddress, Bool )
    , maybeStarport : Maybe ContractAddress
    , maybeTimelock : Maybe ContractAddress
    , maybeCErc20Delegate : Maybe ContractAddress
    , maybeCDaiDelegate : Maybe ContractAddress
    , maybeCompToken : Maybe TokenConfig
    , maybeReservoir : Maybe ContractAddress
    , maybeComptrollerG3 : Maybe ContractAddress
    , maybePriceFeed : Maybe ContractAddress
    , maybeCrowdProposalFactory : Maybe ContractAddress
    , blocks : Dict String Int
    , basicConfig : BasicConfig
    }


getCTokenAddresses : Dict String CTokenConfig -> List ContractAddress
getCTokenAddresses cTokenConfigDict =
    cTokenConfigDict
        |> Dict.values
        |> List.map .address


getUnderlyingTokenAddresses : Dict String CTokenConfig -> List AssetAddress
getUnderlyingTokenAddresses cTokenConfigDict =
    cTokenConfigDict
        |> Dict.values
        |> List.map .underlying
        |> List.map .address
        |> List.map contractAddressToAssetAddress


basicConfigDecoder : Decoder (Dict String (Maybe BasicConfig))
basicConfigDecoder =
    Json.Decode.dict <|
        Json.Decode.maybe
            (Json.Decode.map5 BasicConfig
                (field "Contracts" (Json.Decode.dict decodeContractAddress))
                (field "cTokens" (Json.Decode.dict decodeRawCToken))
                (field "Tokens" (Json.Decode.dict decodeToken))
                (field "InterestRateModel" (Json.Decode.dict decodeInterestRateModel))
                (field "Blocks"
                    (Json.Decode.dict Json.Decode.int)
                )
            )


loadConfigs : Json.Encode.Value -> Result Error (Dict String Config)
loadConfigs json =
    decodeValue basicConfigDecoder json
        |> Result.map
            (Functions.dictFilterMap
                (\basicConfigKey maybeBasicConfig ->
                    maybeBasicConfig
                        |> Maybe.andThen (loadConfig basicConfigKey)
                )
            )


loadConfig : String -> BasicConfig -> Maybe Config
loadConfig networkName ({ contracts, cTokensRaw, tokens, blocks } as basicConfig) =
    let
        maybeComptroller =
            Dict.get "Comptroller" contracts

        maybePriceOracle =
            Dict.get "PriceOracleProxy" contracts

        maybePriceFeed =
            Dict.get "PriceFeed" contracts

        maybeMaximillion =
            Dict.get "Maximillion" contracts

        maybeCEtherTokenRaw =
            Dict.get "cETH" cTokensRaw

        maybeFauceteer =
            Dict.get "Fauceteer" contracts

        maybeGovernor =
            case ( Dict.get "GovernorBravo" contracts, Dict.get "GovernorAlpha" contracts ) of
                ( Just bravoAddress, _ ) ->
                    Just ( bravoAddress, True )

                ( Nothing, Just alphaAddress ) ->
                    Just ( alphaAddress, False )

                _ ->
                    Nothing

        maybeStarport =
            Dict.get "Starport" contracts

        maybeTimelock =
            Dict.get "Timelock" contracts

        maybeCrowdProposalFactory =
            Dict.get "CrowdProposalFactory" contracts

        maybeCompoundLens =
            Dict.get "CompoundLens" contracts

        maybeCErc20Delegate =
            Dict.get "cErc20Delegate" contracts

        maybeCDaiDelegate =
            Dict.get "cDaiDelegate" contracts

        maybeUSDCAssetAddress =
            tokens
                |> Dict.values
                |> List.filter (\tokenConfig -> tokenConfig.symbol == "USDC")
                |> List.head
                |> Maybe.andThen (\tokenConfig -> Just tokenConfig.address)

        maybeCompToken =
            Dict.get "COMP" tokens

        maybeReservoir =
            Dict.get "Reservoir" contracts

        maybeComptrollerG3 =
            Dict.get "StdComptrollerG3" contracts

        cTokensWithoutCEth =
            Functions.dictFilterMap
                (\key rawCTokenConfig ->
                    tokens
                        |> Dict.values
                        |> List.filter (\tokenConfig -> Just tokenConfig.address == rawCTokenConfig.underlying)
                        |> List.head
                        |> Maybe.andThen
                            (\tokenConfig ->
                                let
                                    underlyingRebrandTokenConfig =
                                        case networkName of
                                            "mainnet" ->
                                                if areContractsEqual rawCTokenConfig.address (Contract "0x5d3a536E4D6DbD6114cc1Ead35777bAB948E3643") then
                                                    -- Rename DAI in the underlying
                                                    { tokenConfig | name = "Dai" }

                                                else if areContractsEqual rawCTokenConfig.address (Contract "0xF5DCe57282A584D2746FaF1593d3121Fcac444dC") then
                                                    -- Rename SAI in the underlying
                                                    { tokenConfig | name = "Sai (Deprecated)" }

                                                else if areContractsEqual rawCTokenConfig.address (Contract "0x158079Ee67Fce2f58472A96584A73C7Ab9AC95c1") then
                                                    -- Rename REP in the underlying
                                                    { tokenConfig | name = "Augur v1 (Deprecated)" }

                                                else if areContractsEqual rawCTokenConfig.address (Contract "0xC11b1268C1A384e55C48c2391d8d480264A3A7F4") then
                                                    -- Rename old WBTC in the underlying
                                                    { tokenConfig | name = "WBTC (Legacy)" }

                                                else
                                                    tokenConfig

                                            "kovan" ->
                                                if areContractsEqual rawCTokenConfig.address (Contract "0xe7bc397DBd069fC7d0109C0636d06888bb50668c") then
                                                    -- Rename DAI in the underlying
                                                    { tokenConfig | name = "Dai" }

                                                else if areContractsEqual rawCTokenConfig.address (Contract "0x63c344BF8651222346DD870be254D4347c9359f7") then
                                                    -- Rename SAI in the underlying
                                                    { tokenConfig | name = "Sai (Legacy DAI)" }

                                                else
                                                    tokenConfig

                                            _ ->
                                                tokenConfig

                                    underlyingTetherRebrandTokenConfig =
                                        if underlyingRebrandTokenConfig.symbol == "USDT" && underlyingRebrandTokenConfig.name == "USDT" then
                                            { underlyingRebrandTokenConfig | name = "Tether" }

                                        else
                                            underlyingRebrandTokenConfig
                                in
                                { name = rawCTokenConfig.name
                                , symbol = rawCTokenConfig.symbol
                                , decimals = rawCTokenConfig.decimals
                                , address = rawCTokenConfig.address
                                , underlying = underlyingTetherRebrandTokenConfig
                                }
                                    |> Just
                            )
                )
                cTokensRaw

        ( cTokens, maybeCEtherToken ) =
            case maybeCEtherTokenRaw of
                Just cEtherTokenConfig ->
                    let
                        dummyEthConfig =
                            { name = "Ether"
                            , symbol = "ETH"
                            , decimals = 18
                            , address = cEtherTokenConfig.address
                            , supported = Just True
                            , reader = Nothing
                            }

                        actualCEtherTokenConfig =
                            { name = cEtherTokenConfig.name
                            , symbol = cEtherTokenConfig.symbol
                            , decimals = cEtherTokenConfig.decimals
                            , address = cEtherTokenConfig.address
                            , underlying = dummyEthConfig
                            }
                    in
                    ( Dict.insert cEtherTokenConfig.symbol actualCEtherTokenConfig cTokensWithoutCEth
                    , Just actualCEtherTokenConfig.underlying
                    )

                Nothing ->
                    ( cTokensWithoutCEth
                    , Nothing
                    )
    in
    Functions.map5
        maybeComptroller
        maybePriceOracle
        maybeCEtherToken
        maybeMaximillion
        maybeCompoundLens
        (\comptroller priceOracle cEtherToken maximillion compoundLens ->
            { comptroller = comptroller
            , priceOracle = priceOracle
            , maximillion = maximillion
            , compoundLens = compoundLens
            , maybeFauceteer = maybeFauceteer
            , cEtherToken = cEtherToken
            , cTokens = cTokens
            , maybeInvertedEtherPriceAsset = maybeUSDCAssetAddress
            , maybeGovernor = maybeGovernor
            , maybeStarport = maybeStarport
            , maybeTimelock = maybeTimelock
            , maybeCErc20Delegate = maybeCErc20Delegate
            , maybeCDaiDelegate = maybeCDaiDelegate
            , maybeCompToken = maybeCompToken
            , maybeReservoir = maybeReservoir
            , maybeComptrollerG3 = maybeComptrollerG3
            , maybePriceFeed = maybePriceFeed
            , maybeCrowdProposalFactory = maybeCrowdProposalFactory
            , blocks = blocks
            , basicConfig = basicConfig
            }
        )


decodeRawCToken : Decoder RawCTokenConfig
decodeRawCToken =
    Json.Decode.map5 RawCTokenConfig
        (field "name" string)
        (field "symbol" string)
        (field "decimals" int)
        (field "address" decodeContractAddress)
        (Json.Decode.maybe (field "underlying" decodeContractAddress))


decodeToken : Decoder TokenConfig
decodeToken =
    Json.Decode.map6 TokenConfig
        (field "name" string)
        (field "symbol" string)
        (field "decimals" int)
        (field "address" decodeContractAddress)
        (Json.Decode.maybe (field "supported" bool))
        (Json.Decode.maybe (field "reader" decodeContractAddress))


decodeInterestRateModel : Decoder InterestRateModelConfig
decodeInterestRateModel =
    Json.Decode.map2 InterestRateModelConfig
        (field "contract" string)
        (field "address" decodeContractAddress)
