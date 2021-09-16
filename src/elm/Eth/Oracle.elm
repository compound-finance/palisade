port module Eth.Oracle exposing
    ( OracleMsg(..)
    , OracleState
    , getCompPriceUSD
    , getEtherPrice
    , getOraclePrice
    , oracleInit
    , oracleNewBlockCmd
    , oracleSubscriptions
    , oracleUpdate
    )

import CompoundComponents.Console as Console
import CompoundComponents.Eth.Decoders exposing (decimal, decodeAssetAddress)
import CompoundComponents.Eth.Ethereum as Ethereum exposing (AssetAddress(..), ContractAddress(..))
import CompoundComponents.Functions as Functions
import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Eth.Config exposing (Config)
import Eth.Token exposing (CToken, Token, TokenState, getTokenAddress)
import Json.Decode exposing (Value, decodeValue, field, float)


type alias OracleState =
    { prices : Dict String Decimal
    , isPriceFeedOracle : ( Maybe Bool, Maybe Decimal )
    , errors : List String
    }


oracleInit : ( OracleState, Cmd OracleMsg )
oracleInit =
    ( { prices = Dict.empty
      , isPriceFeedOracle = ( Nothing, Nothing )
      , errors = []
      }
    , Cmd.none
    )


type OracleMsg
    = SetOraclePrices (List ( AssetAddress, Decimal ))
    | Error String


oracleUpdate : Maybe Config -> TokenState -> OracleMsg -> OracleState -> ( OracleState, Cmd OracleMsg )
oracleUpdate maybeConfig tokenState msg state =
    case msg of
        SetOraclePrices allPrices ->
            let
                updatedPrices =
                    allPrices
                        |> List.foldl
                            (\( assetAddress, value ) acc ->
                                let
                                    assetAddressVal =
                                        case assetAddress of
                                            Asset assetAddressVal_ ->
                                                assetAddressVal_
                                in
                                Dict.insert assetAddressVal value acc
                            )
                            state.prices

                stateWithUpdatePrices =
                    { state | prices = updatedPrices }

                etherAddress =
                    maybeConfig
                        |> Maybe.map
                            (\config ->
                                Ethereum.getContractAddressString config.cEtherToken.address
                            )
                        |> Maybe.withDefault "NADA-ADDRESS"

                maybeEtherUsdPrice =
                    maybeConfig
                        |> Maybe.andThen
                            (\config ->
                                getEtherPrice config tokenState state
                            )

                knownPriceFeedOracle =
                    -- If we get a price update for Ether then we can check if it's
                    -- value is 1 which signifies old Oracle otherwise we know this
                    -- is the new Price Feed Oracle.
                    case Dict.get etherAddress updatedPrices of
                        Just etherPriceUpdate ->
                            let
                                decimalAdjustedPrice =
                                    let
                                        decimalDelta =
                                            0

                                        deScaleAmount =
                                            Decimal.fromIntWithExponent 1 -decimalDelta
                                    in
                                    -- price / 1e12 == price * (1e-12)
                                    Decimal.mul etherPriceUpdate deScaleAmount
                            in
                            if Decimal.eq decimalAdjustedPrice Decimal.one then
                                let
                                    -- The very first time we have a weird circular, chicken and egg problem
                                    -- so let's manually set the eth price if we don't have a current
                                    -- eth price set for old oracle here.
                                    firstTimeEtherUsdPrice =
                                        case ( maybeEtherUsdPrice, maybeConfig ) of
                                            ( Nothing, Just config ) ->
                                                getEtherPriceFromInvertedUSDC config tokenState stateWithUpdatePrices

                                            _ ->
                                                maybeEtherUsdPrice
                                in
                                ( Just False, firstTimeEtherUsdPrice )

                            else
                                ( Just True, Nothing )

                        Nothing ->
                            if state.isPriceFeedOracle == ( Just False, Nothing ) then
                                Tuple.mapSecond (\_ -> maybeEtherUsdPrice) state.isPriceFeedOracle

                            else
                                state.isPriceFeedOracle
            in
            ( { stateWithUpdatePrices
                | isPriceFeedOracle = knownPriceFeedOracle
              }
            , Cmd.none
            )

        Error error ->
            ( { state | errors = error :: state.errors }, Console.error error )


getDescaledPriceFromState : OracleState -> Token -> Maybe Decimal
getDescaledPriceFromState { prices } token =
    let
        tokenAddress =
            getTokenAddress token
    in
    Dict.get tokenAddress prices
        |> Maybe.map
            (\actualPrice ->
                let
                    decimalDelta =
                        Eth.Token.ethDecimals - token.decimals

                    deScaleAmount =
                        Decimal.fromIntWithExponent 1 -decimalDelta
                in
                -- price / 1e12 == price * (1e-12)
                Decimal.mul actualPrice deScaleAmount
            )


getOraclePrice : OracleState -> Token -> Maybe Decimal
getOraclePrice ({ prices, isPriceFeedOracle } as oracleState) token =
    let
        actualOraclePrice =
            getDescaledPriceFromState oracleState token
    in
    if Tuple.first isPriceFeedOracle == Just False then
        let
            maybeEtherUsdPrice =
                Tuple.second isPriceFeedOracle
        in
        Functions.map2
            actualOraclePrice
            maybeEtherUsdPrice
            (\price etherUSD ->
                Decimal.mul price etherUSD
            )

    else
        actualOraclePrice


getEtherPrice : Config -> TokenState -> OracleState -> Maybe Decimal
getEtherPrice config tokenState ({ prices, isPriceFeedOracle } as oracleState) =
    let
        maybeCEtherToken =
            tokenState.cTokens
                |> Dict.values
                |> List.filter (\cToken -> Eth.Token.isCEtherToken config cToken)
                |> List.head

        maybeCEtherPrice =
            maybeCEtherToken
                |> Maybe.andThen (\cEtherToken -> getOraclePrice oracleState cEtherToken.underlying)
    in
    if Tuple.first isPriceFeedOracle == Just False then
        getEtherPriceFromInvertedUSDC config tokenState oracleState

    else
        maybeCEtherPrice


getEtherPriceFromInvertedUSDC : Config -> TokenState -> OracleState -> Maybe Decimal
getEtherPriceFromInvertedUSDC config tokenState oracleState =
    case config.maybeInvertedEtherPriceAsset of
        Just (Contract invertedEtherPriceAssetString) ->
            let
                usdcToken =
                    Eth.Token.getUnderlyingTokenByAddress tokenState.cTokens invertedEtherPriceAssetString

                maybeEtherUsdPrice =
                    usdcToken
                        |> Maybe.andThen (getDescaledPriceFromState oracleState)
                        |> Maybe.andThen (Decimal.fastdiv Decimal.one)
            in
            maybeEtherUsdPrice

        Nothing ->
            tokenState.infuraEtherPrice


getCompPriceUSD : Config -> OracleState -> Maybe Decimal
getCompPriceUSD config { prices } =
    config.maybeCompToken
        |> Maybe.andThen
            (\compToken ->
                Dict.get (Ethereum.getContractAddressString compToken.address) prices
            )


oracleSubscriptions : OracleState -> Sub OracleMsg
oracleSubscriptions state =
    Sub.batch
        [ giveAllOraclePrices (Functions.handleError (Json.Decode.errorToString >> Error) SetOraclePrices) ]


oracleNewBlockCmd : OracleState -> Int -> ContractAddress -> TokenState -> ContractAddress -> Cmd OracleMsg
oracleNewBlockCmd state blockNumber priceOracle tokenState compoundLens =
    Cmd.batch
        [ askTokenOraclePrices blockNumber priceOracle compoundLens (Dict.values tokenState.cTokens)
        ]


askTokenOraclePrices : Int -> ContractAddress -> ContractAddress -> List CToken -> Cmd OracleMsg
askTokenOraclePrices blockNumber priceOracleAddress compoundLens cTokens =
    cTokens
        |> List.map (\cToken -> ( cToken.contractAddress, cToken.underlying.assetAddress ))
        |> askOraclePricesAll blockNumber compoundLens



-- Ports


port askOraclePricesAllPort : { blockNumber : Int, cTokens : List ( String, String ), compoundLens : String } -> Cmd msg


askOraclePricesAll : Int -> ContractAddress -> List ( ContractAddress, AssetAddress ) -> Cmd msg
askOraclePricesAll blockNumber (Contract compoundLens) cTokenPairs =
    let
        cTokens =
            List.map
                (\cTokenPair ->
                    case cTokenPair of
                        ( Contract token, Asset asset ) ->
                            ( token, asset )
                )
                cTokenPairs
    in
    askOraclePricesAllPort
        { blockNumber = blockNumber
        , cTokens = cTokens
        , compoundLens = compoundLens
        }


port giveOraclePricesAllPort : (Value -> msg) -> Sub msg


giveAllOraclePrices : (Result Json.Decode.Error (List ( AssetAddress, Decimal )) -> msg) -> Sub msg
giveAllOraclePrices wrapper =
    let
        decoder =
            Json.Decode.list
                (Json.Decode.map2 Tuple.pair
                    (field "underlyingAssetAddress" decodeAssetAddress)
                    (field "value" decimal)
                )
    in
    giveOraclePricesAllPort
        (decodeValue decoder >> wrapper)
