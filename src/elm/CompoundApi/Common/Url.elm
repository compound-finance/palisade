module CompoundApi.Common.Url exposing (buildApiUrl)

import CompoundComponents.Eth.Network exposing (Network(..))
import Dict exposing (Dict)
import Url.Builder as UrlBuilder exposing (QueryParameter)


buildApiUrl : Dict String String -> Network -> String -> List QueryParameter -> Maybe String
buildApiUrl apiBaseUrlMap network apiEndpoint endpointQueryParams =
    let
        lowercaseNetworkName =
            String.toLower (CompoundComponents.Eth.Network.networkName network)

        shouldAddNetworkQueryParam =
            case network of
                MainNet ->
                    True

                Ropsten ->
                    True

                Rinkeby ->
                    True

                Goerli ->
                    True

                Kovan ->
                    True

                _ ->
                    False

        finalQueryParams =
            if shouldAddNetworkQueryParam then
                endpointQueryParams ++ [ UrlBuilder.string "network" lowercaseNetworkName ]

            else
                endpointQueryParams
    in
    Dict.get lowercaseNetworkName apiBaseUrlMap
        |> Maybe.map (\apiBaseUrl -> apiBaseUrl ++ UrlBuilder.relative [ apiEndpoint ] finalQueryParams)
