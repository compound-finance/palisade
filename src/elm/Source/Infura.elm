module Source.Infura exposing (ethereumApiUrl, loadEtherPrice)

import CompoundComponents.Eth.Decoders exposing (decimal)
import Decimal exposing (Decimal)
import Http
import Json.Decode exposing (Decoder, field)


ethereumApiUrl : String
ethereumApiUrl =
    "https://api.infura.io/v1/ticker/ethusd"


loadEtherPrice : (Result Http.Error Decimal -> msg) -> Cmd msg
loadEtherPrice handler =
    Http.get ethereumApiUrl (field "bid" decimal)
        |> Http.send handler
