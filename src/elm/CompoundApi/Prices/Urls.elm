module CompoundApi.Prices.Urls exposing (pricesRequestUrl)

import CompoundApi.Common.Url
import CompoundComponents.Eth.Network exposing (Network)
import Dict exposing (Dict)
import Url.Builder as UrlBuilder


type alias PricesRequestOptions =
    { block_timestamp : Maybe Int }


pricesRequestUrl : Dict String String -> Network -> PricesRequestOptions -> Maybe String
pricesRequestUrl apiBaseUrlMap network requestOptions =
    let
        queryFinal =
            case requestOptions.block_timestamp of
                Just timestamp ->
                    [ UrlBuilder.string "block_timestamp" (String.fromInt timestamp) ]

                _ ->
                    []
    in
    CompoundApi.Common.Url.buildApiUrl apiBaseUrlMap network "v2/prices" queryFinal
