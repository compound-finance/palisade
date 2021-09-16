module CompoundApi.Governance.ProposalVoteReceiptService.Models exposing
    ( ProposalVoteReceipt
    , ProposalVoteReceiptRequest
    , ProposalVoteReceiptResponse
    , createProposalVoteReceipt
    )

import CompoundApi.Common.Models exposing (API_Error, PaginationSummary)
import CompoundApi.Governance.Common.Models exposing (DisplayAccount)
import CompoundApi.Governance.ProposalService.Models exposing (Proposal)
import Decimal exposing (Decimal)


type alias ProposalVoteReceiptRequest =
    { proposal_id : Maybe Int
    , account : Maybe String
    , support : Maybe Bool
    , with_proposal_data : Bool
    , page_size : Int
    , page_number : Int -- This starts at page 1
    }


type alias ProposalVoteReceiptResponse =
    { request : ProposalVoteReceiptRequest
    , pagination_summary : PaginationSummary
    , proposal_vote_receipts : List ProposalVoteReceipt
    , error : Maybe API_Error
    }


type alias ProposalVoteReceipt =
    { proposal_id : Int
    , proposal : Maybe (Proposal {})
    , voter : DisplayAccount {}
    , support : Bool
    , votes : Decimal
    }


createProposalVoteReceipt : Int -> DisplayAccount {} -> Bool -> Decimal -> Maybe (Proposal {}) -> ProposalVoteReceipt
createProposalVoteReceipt id displayAccount support votes proposal =
    { proposal_id = id
    , proposal = proposal
    , voter = displayAccount
    , support = support
    , votes = votes
    }
