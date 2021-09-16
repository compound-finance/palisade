module CompoundApi.Governance.AccountService.Models exposing
    ( AccountFilterByEnum(..)
    , GovernanceAccountRequest
    , GovernanceAccountResponse
    , GovernanceAccountSearchRequest
    , GovernanceAccountSearchResponse
    )

import CompoundApi.Common.Models exposing (API_Error, PaginationSummary)
import CompoundApi.Governance.Common.Models exposing (CompAccount)


type AccountFilterByEnum
    = Votes
    | Balance
    | ProposalsCreated


type alias GovernanceAccountRequest =
    { addresses : List String
    , order_by : Maybe AccountFilterByEnum
    , page_size : Int
    , page_number : Int -- This starts at page 1
    , with_history : Bool
    }


type alias GovernanceAccountSearchRequest =
    { term : String
    , page_size : Int
    , page_number : Int -- This starts at page 1
    }


type alias GovernanceAccountResponse =
    { request : GovernanceAccountRequest
    , pagination_summary : PaginationSummary
    , accounts : List CompAccount
    , error : Maybe API_Error
    }


type alias GovernanceAccountSearchResponse =
    { request : GovernanceAccountSearchRequest
    , pagination_summary : PaginationSummary
    , accounts : List CompAccount
    , error : Maybe API_Error
    }
