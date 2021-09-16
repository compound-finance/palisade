module CompoundApi.Common.Encoders exposing (encodeAPIPrecise)

import Decimal exposing (Decimal)
import Json.Encode


encodeAPIPrecise : Decimal -> Json.Encode.Value
encodeAPIPrecise value =
    Json.Encode.object
        [ ( "value", Json.Encode.string (Decimal.toString value) )
        ]
