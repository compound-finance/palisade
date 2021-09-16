module CompoundApi.Governance.CompService.Decoders exposing (accountCompDistributionResponseDecoder, marketCompDistributionResponseDecoder)

import CompoundApi.Governance.CompService.Models
    exposing
        ( AccountCompDistibution
        , AccountCompDistributionRequest
        , AccountCompDistributionResponse
        , MarketCompDistibution
        , MarketCompDistributionRequest
        , MarketCompDistributionResponse
        )
import CompoundComponents.Eth.Decoders exposing (stringDecimal)
import Json.Decode exposing (Decoder, field, list, map, map2, map6, maybe, string, succeed)
import Json.Decode.Pipeline exposing (required)


acccountCompDistributionDecoder : Decoder AccountCompDistibution
acccountCompDistributionDecoder =
    succeed AccountCompDistibution
        |> required "address" string
        |> required "comp_allocated" stringDecimal
        |> required "comp_borrow_index" stringDecimal
        |> required "comp_distributed" stringDecimal
        |> required "comp_supply_index" stringDecimal
        |> required "daily_comp" stringDecimal
        |> required "name" string
        |> required "symbol" string
        |> required "underlying_address" (maybe string)
        |> required "underlying_name" string
        |> required "underlying_symbol" string


marketCompDistributionDecoder : Decoder MarketCompDistibution
marketCompDistributionDecoder =
    succeed MarketCompDistibution
        |> required "address" string
        |> required "borrower_daily_comp" stringDecimal
        |> required "comp_allocated" stringDecimal
        |> required "comp_borrow_index" stringDecimal
        |> required "comp_distributed" stringDecimal
        |> required "comp_speed" stringDecimal
        |> required "comp_supply_index" stringDecimal
        |> required "name" string
        |> required "supplier_daily_comp" stringDecimal
        |> required "symbol" string
        |> required "underlying_address" (maybe string)
        |> required "underlying_name" string
        |> required "underlying_symbol" string


accountCompDistributionResponseDecoder : Decoder AccountCompDistributionResponse
accountCompDistributionResponseDecoder =
    map2 AccountCompDistributionResponse
        (field "markets"
            (list acccountCompDistributionDecoder)
        )
        (field "request"
            (map AccountCompDistributionRequest
                (field "address" string)
            )
        )


marketCompDistributionResponseDecoder : Decoder MarketCompDistributionResponse
marketCompDistributionResponseDecoder =
    map6 MarketCompDistributionResponse
        (field "comp_rate" stringDecimal)
        (field "daily_comp" stringDecimal)
        (field "markets"
            (list marketCompDistributionDecoder)
        )
        (field "request"
            (map MarketCompDistributionRequest
                (field "addresses" (list string))
            )
        )
        (field "total_comp_allocated" stringDecimal)
        (field "total_comp_distributed" stringDecimal)
