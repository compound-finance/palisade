module CompoundApi.Governance.HistoryService.Urls exposing (governanceHistoryRequestUrl)

import CompoundApi.Common.Url
import CompoundApi.Governance.HistoryService.Models exposing (GovernanceHistoryRequest)
import CompoundComponents.Eth.Network exposing (Network)
import Dict exposing (Dict)


governanceHistoryRequestUrl : Dict String String -> Network -> GovernanceHistoryRequest -> Maybe String
governanceHistoryRequestUrl apiBaseUrlMap network _ =
    CompoundApi.Common.Url.buildApiUrl apiBaseUrlMap network "v2/governance/history" []
