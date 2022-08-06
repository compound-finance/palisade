module CompoundApi.GasService.Urls exposing (getGasPriceUrl)

import CompoundApi.Common.Url
import CompoundComponents.Eth.Network exposing (Network)
import Dict exposing (Dict)


getGasPriceUrl : Dict String String -> Network -> Maybe String
getGasPriceUrl apiBaseUrlMap network =
    CompoundApi.Common.Url.buildApiUrl apiBaseUrlMap network "getGasPrices" []
