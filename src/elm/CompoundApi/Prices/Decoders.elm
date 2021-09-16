module CompoundApi.Prices.Decoders exposing (pricesResponseDecoder)

import CompoundApi.Common.Decoders exposing (apiErrorDecoder)
import CompoundApi.Prices.Models
    exposing
        ( PricesRequest
        , PricesResponse
        , Token
        )
import CompoundComponents.Eth.Decoders exposing (stringDecimal)
import Json.Decode exposing (Decoder, field, int, list, map3, map6, maybe, string)


tokenDecoder : Decoder Token
tokenDecoder =
    map6 Token
        (field "address" string)
        (field "name" string)
        (field "symbol" string)
        (field "price" stringDecimal)
        (field "price_block_number" (maybe int))
        (field "price_timestamp" (maybe int))


pricesResponseDecoder : Decoder PricesResponse
pricesResponseDecoder =
    map3 PricesResponse
        (field "request"
            (map3 PricesRequest
                (field "addresses"
                    (list string)
                )
                (field "block_number" (maybe int))
                (field "block_timestamp" (maybe int))
            )
        )
        (field "tokens"
            (list tokenDecoder)
        )
        (field "error" apiErrorDecoder)
