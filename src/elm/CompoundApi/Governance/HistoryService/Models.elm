module CompoundApi.Governance.HistoryService.Models exposing
    ( GovernanceHistoryRequest
    , GovernanceHistoryResponse
    )

import CompoundApi.Common.Models exposing (API_Error)
import Decimal exposing (Decimal)


type alias GovernanceHistoryRequest =
    {}


type alias GovernanceHistoryResponse =
    { error : Maybe API_Error
    , proposals_created : Int
    , token_holders : Int
    , total_comp_allocated : Decimal
    , votes_delegated : Decimal
    , voting_addresses : Int
    }
