module CompoundApi.Presidio.CTokens.Urls exposing (cTokenRequestUrl)

import CompoundApi.Common.Url
import CompoundComponents.Eth.Network exposing (Network)
import Dict exposing (Dict)
import Json.Encode exposing (bool, encode)
import Url.Builder as UrlBuilder


type alias CTokenRequestOptions =
    { includeMetadata : Bool, block_timestamp : Maybe Int }


cTokenRequestUrl : Dict String String -> Network -> CTokenRequestOptions -> Maybe String
cTokenRequestUrl apiBaseUrlMap network requestOptions =
    let
        queryMeta =
            [ UrlBuilder.string "meta" (encode 0 (bool requestOptions.includeMetadata)) ]

        queryFinal =
            case requestOptions.block_timestamp of
                Just timestamp ->
                    queryMeta ++ [ UrlBuilder.string "block_timestamp" (String.fromInt timestamp) ]

                _ ->
                    queryMeta
    in
    CompoundApi.Common.Url.buildApiUrl apiBaseUrlMap network "" queryFinal
