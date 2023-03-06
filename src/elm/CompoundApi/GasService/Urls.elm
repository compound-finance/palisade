module CompoundApi.GasService.Urls exposing (getGasPriceUrl)
import CompoundComponents.Eth.Network exposing (Network, networkName)
import Url.Builder as UrlBuilder
import String exposing (toLower)
import Dict exposing (Dict)


getGasPriceUrl : Dict String String -> Network -> Maybe String
getGasPriceUrl apiBaseUrlMap network =
    Dict.get "v3_api" apiBaseUrlMap
        |> Maybe.map (\apiBaseUrl -> apiBaseUrl ++ UrlBuilder.relative [ "legacy", toLower (networkName network), "gas-price" ] [])
