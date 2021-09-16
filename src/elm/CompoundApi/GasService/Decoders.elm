module CompoundApi.GasService.Decoders exposing (gasPriceResponseDecoder)

import CompoundApi.Common.Decoders
import CompoundApi.Common.Models exposing (API_Error)
import CompoundApi.GasService.Models exposing (API_GasPriceResponse)
import CompoundComponents.Eth.Decoders
import Json.Decode exposing (field)


gasPriceResponseDecoder : Json.Decode.Decoder API_GasPriceResponse
gasPriceResponseDecoder =
    Json.Decode.map4 API_GasPriceResponse
        (Json.Decode.field "average" CompoundApi.Common.Decoders.apiPrecise)
        (Json.Decode.field "fast" CompoundApi.Common.Decoders.apiPrecise)
        (Json.Decode.field "fastest" CompoundApi.Common.Decoders.apiPrecise)
        (Json.Decode.field "safe_low" CompoundApi.Common.Decoders.apiPrecise)
