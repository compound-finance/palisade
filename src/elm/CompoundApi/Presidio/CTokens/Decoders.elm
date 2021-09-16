module CompoundApi.Presidio.CTokens.Decoders exposing (cTokenResponseDecoder)

import CompoundApi.Common.Decoders exposing (apiErrorDecoder, apiPrecise)
import CompoundApi.Common.Models exposing (API_Error)
import CompoundApi.Presidio.CTokens.Models exposing (CToken, CTokenRequest, CTokenResponse, Metadata)
import CompoundComponents.DecoderHelper exposing (andMap)
import CompoundComponents.Eth.Ethereum as Ethereum exposing (ContractAddress(..))
import Decimal exposing (Decimal)
import Json.Decode exposing (Decoder, andThen, field, int, list, map2, map3, map4, maybe, null, nullable, oneOf, string, succeed)


cTokenResponseDecoder : Decoder CTokenResponse
cTokenResponseDecoder =
    map4 CTokenResponse
        (field "request"
            (map3 CTokenRequest
                (field "addresses"
                    (list string)
                )
                (field "block_number" (maybe int))
                (field "block_timestamp" (maybe int))
            )
        )
        (field "cToken"
            (list cTokenDecoder)
        )
        (field "error" apiErrorDecoder)
        (field "meta"
            (nullable
                (map2 Metadata
                    (field "unique_borrowers" int)
                    (field "unique_suppliers" int)
                )
            )
        )


cTokenDecoder : Json.Decode.Decoder CToken
cTokenDecoder =
    let
        addEthValues apiToken =
            let
                supplyValue =
                    calculateTotalSupply apiToken

                borrowValue =
                    calculateTotalBorrow apiToken

                borrowCapValue =
                    calculateBorrowCap apiToken
            in
            succeed
                { apiToken
                    | total_supply_value_in_eth = supplyValue
                    , total_borrow_value_in_eth = borrowValue
                    , underlying_borrow_cap_in_eth = borrowCapValue
                }
    in
    succeed CToken
        |> andMap (field "borrow_rate" apiPrecise)
        |> andMap (field "borrow_cap" apiPrecise)
        |> andMap (field "cash" apiPrecise)
        |> andMap (field "collateral_factor" apiPrecise)
        |> andMap (field "exchange_rate" apiPrecise)
        |> andMap (field "interest_rate_model_address" string)
        |> andMap (field "name" string)
        |> andMap (field "number_of_borrowers" int)
        |> andMap (field "number_of_suppliers" int)
        |> andMap (field "reserves" apiPrecise)
        |> andMap (field "reserve_factor" apiPrecise)
        |> andMap (field "supply_rate" apiPrecise)
        |> andMap (field "symbol" string)
        |> andMap (field "token_address" string)
        |> andMap (field "total_borrows" apiPrecise)
        |> andMap (field "total_supply" apiPrecise)
        |> andMap (field "underlying_address" (maybe string))
        |> andMap (field "underlying_name" string)
        |> andMap (field "underlying_price" apiPrecise)
        |> andMap (field "underlying_symbol" string)
        |> andMap (field "comp_supply_apy" <| oneOf [ apiPrecise, null Decimal.zero ])
        |> andMap (field "comp_borrow_apy" <| oneOf [ apiPrecise, null Decimal.zero ])
        -- set total supply, total borrow and underlying borrow cap value to zero to create valid record, can calculate in "andThen"
        |> andMap (succeed Decimal.zero)
        |> andMap (succeed Decimal.zero)
        |> andMap (succeed Decimal.zero)
        |> andThen addEthValues
        |> andThen renameDaiNameToSaiLegacyDecoder
        |> andThen renameUsdtToTetherDecoder
        |> andThen renameAugurToDeprecatedDecoder
        |> andThen renameOldWbtcToLegacyDecoder


calculateTotalBorrow : CToken -> Decimal
calculateTotalBorrow { total_borrows, underlying_price } =
    total_borrows
        |> Decimal.mul underlying_price


calculateTotalSupply : CToken -> Decimal
calculateTotalSupply { total_supply, exchange_rate, underlying_price } =
    total_supply
        |> Decimal.mul exchange_rate
        |> Decimal.mul underlying_price


calculateBorrowCap : CToken -> Decimal
calculateBorrowCap { borrow_cap, underlying_price } =
    borrow_cap
        |> Decimal.mul underlying_price


renameDaiNameToSaiLegacyDecoder : CToken -> Json.Decode.Decoder CToken
renameDaiNameToSaiLegacyDecoder apiToken =
    let
        -- Doing a rename based on the address combo of both CToken and Underlying since
        -- we don't actually know what network we are running on here.
        ( underlyingName, underlyingSymbol ) =
            if
                Ethereum.areContractsEqual (Contract apiToken.token_address) (Contract "0x5d3a536E4D6DbD6114cc1Ead35777bAB948E3643")
                    && Ethereum.areContractsEqual (Contract (Maybe.withDefault "" apiToken.underlying_address)) (Contract "0x6B175474E89094C44Da98b954EedeAC495271d0F")
            then
                ( "Dai", apiToken.underlying_symbol )

            else if
                Ethereum.areContractsEqual (Contract apiToken.token_address) (Contract "0xF5DCe57282A584D2746FaF1593d3121Fcac444dC")
                    && Ethereum.areContractsEqual (Contract (Maybe.withDefault "" apiToken.underlying_address)) (Contract "0x89d24a6b4ccb1b6faa2625fe562bdd9a23260359")
            then
                ( "Sai (Deprecated)", "SAI" )

            else
                ( apiToken.underlying_name, apiToken.underlying_symbol )
    in
    succeed { apiToken | underlying_name = underlyingName, underlying_symbol = underlyingSymbol }


renameUsdtToTetherDecoder : CToken -> Json.Decode.Decoder CToken
renameUsdtToTetherDecoder apiToken =
    let
        -- Doing a rename based on the symbol and name combo of USDT in the Underlying since
        -- we want to say Tether here.
        ( underlyingName, underlyingSymbol ) =
            if apiToken.underlying_symbol == "USDT" && apiToken.underlying_name == "USDT" then
                ( "Tether", apiToken.underlying_symbol )

            else
                ( apiToken.underlying_name, apiToken.underlying_symbol )
    in
    succeed { apiToken | underlying_name = underlyingName, underlying_symbol = underlyingSymbol }


renameAugurToDeprecatedDecoder : CToken -> Json.Decode.Decoder CToken
renameAugurToDeprecatedDecoder apiToken =
    let
        -- Doing a rename based on the symbol and name combo of USDT in the Underlying since
        -- we want to say Tether here.
        ( underlyingName, underlyingSymbol ) =
            if apiToken.underlying_symbol == "REP" && apiToken.underlying_name == "Augur" then
                ( "Augur v1 (Deprecated)", apiToken.underlying_symbol )

            else
                ( apiToken.underlying_name, apiToken.underlying_symbol )
    in
    succeed { apiToken | underlying_name = underlyingName, underlying_symbol = underlyingSymbol }


renameOldWbtcToLegacyDecoder : CToken -> Json.Decode.Decoder CToken
renameOldWbtcToLegacyDecoder apiToken =
    let
        underlyingName =
            if apiToken.symbol == "cWBTC" then
                "Wrapped BTC (Legacy)"

            else
                apiToken.underlying_name
    in
    succeed { apiToken | underlying_name = underlyingName }


apiErrorDecoder : Json.Decode.Decoder (Maybe API_Error)
apiErrorDecoder =
    Json.Decode.nullable
        (Json.Decode.map2 API_Error
            (Json.Decode.field "error_code" Json.Decode.int)
            (Json.Decode.field "message" Json.Decode.string)
        )
