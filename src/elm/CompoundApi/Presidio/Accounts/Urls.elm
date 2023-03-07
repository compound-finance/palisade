module CompoundApi.Presidio.Accounts.Urls exposing (accountsRequestUrl)

import CompoundApi.Common.Url
import CompoundApi.Presidio.Accounts.Models exposing (AccountRequest)
import CompoundComponents.Eth.Network exposing (Network)
import Decimal
import Dict exposing (Dict)
import Url.Builder as UrlBuilder


accountsRequestUrl : Dict String String -> Network -> AccountRequest -> Maybe String
accountsRequestUrl apiBaseUrlMap network accountsRequest = Nothing
