module Source.CoinMarketCap exposing (ethereumApiUrl, loadEtherPrice)

import CompoundComponents.Eth.Decoders exposing (forceMaybe, forceOk)
import Functions exposing (first)
import Http
import Json.Decode exposing (Decoder, andThen, field, list, map, string)


ethereumApiUrl : String
ethereumApiUrl =
    "https://api.coinmarketcap.com/v1/ticker/ethereum/"


loadEtherPrice : (Result Http.Error Float -> msg) -> Cmd msg
loadEtherPrice handler =
    let
        decoder : Decoder Float
        decoder =
            field "price_usd" string
                |> list
                |> map first
                |> andThen forceMaybe
                |> map String.toFloat
                |> andThen forceOk
    in
    Http.get ethereumApiUrl decoder
        |> Http.send handler
