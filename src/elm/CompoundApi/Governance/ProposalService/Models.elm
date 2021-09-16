module CompoundApi.Governance.ProposalService.Models exposing
    ( Proposal
    , ProposalState
    , ProposalStateEnum(..)
    , ProposalWithDetail
    , ProposalsRequest
    , ProposalsResponse
    , ProposalsWithDetailResponse
    , createProposal
    , createProposalWithDetail
    )

import CompoundApi.Common.Models exposing (API_Error, PaginationSummary)
import CompoundApi.Governance.Common.Models exposing (DisplayAccount, ProposalAction)
import Decimal exposing (Decimal)
import Time


type ProposalStateEnum
    = Pending
    | Active
    | Succeeded
    | Failed
    | Queued
    | Executed
    | Canceled
    | Expired


type alias ProposalsRequest =
    { proposal_ids : List Int
    , state : Maybe ProposalStateEnum
    , page_size : Int
    , page_number : Int -- This starts at page 1
    , with_detail : Maybe Bool
    }


type alias ProposalsResponse =
    { request : ProposalsRequest
    , pagination_summary : PaginationSummary
    , proposals : List (Proposal {})
    , error : Maybe API_Error
    }


type alias ProposalsWithDetailResponse =
    { request : ProposalsRequest
    , pagination_summary : PaginationSummary
    , proposals : List ProposalWithDetail
    , error : Maybe API_Error
    }


type alias Proposal a =
    { a
        | id : Int
        , title : String
        , description : String
        , states : List ProposalState
        , for_votes : Decimal
        , against_votes : Decimal
        , abstain_votes : Decimal
    }


type alias ProposalWithDetail =
    Proposal
        { proposer : DisplayAccount {}
        , actions : List ProposalAction
        }


createProposal : Int -> String -> String -> List ProposalState -> Decimal -> Decimal -> Decimal -> Proposal {}
createProposal id title description states for_votes against_votes abstain_votes =
    { id = id
    , title = title
    , description = description
    , states = states
    , for_votes = for_votes
    , against_votes = against_votes
    , abstain_votes = abstain_votes
    }


createProposalWithDetail : Int -> String -> String -> DisplayAccount {} -> List ProposalAction -> List ProposalState -> Decimal -> Decimal -> Decimal -> ProposalWithDetail
createProposalWithDetail id title description proposer actions states for_votes against_votes abstain_votes =
    { id = id
    , title = title
    , description = description
    , proposer = proposer
    , actions = actions
    , states = states
    , for_votes = for_votes
    , against_votes = against_votes
    , abstain_votes = abstain_votes
    }


type alias ProposalState =
    { state : ProposalStateEnum
    , start_time : Time.Posix
    , end_time : Maybe Time.Posix
    , trx_hash : Maybe String
    }
