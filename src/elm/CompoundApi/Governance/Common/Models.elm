module CompoundApi.Governance.Common.Models exposing
    ( CompAccount
    , CompAccountTransaction
    , CrowdProposal
    , CrowdProposalStateEnum(..)
    , DisplayAccount
    , ProposalAction
    , createCompAccount
    , createDisplayAccount
    )

import Decimal exposing (Decimal)
import Time


type alias CompAccount =
    DisplayAccount
        { balance : Decimal
        , votes : Decimal
        , vote_weight : Decimal
        , proposals_created : Int
        , proposals_voted : Int
        , delegate : DisplayAccount {}
        , rank : Maybe Int
        , transactions : Maybe (List CompAccountTransaction)
        , total_delegates : Int
        , crowd_proposal : Maybe CrowdProposal
        }


type alias DisplayAccount a =
    { a
        | address : String
        , display_name : Maybe String
        , image_url : Maybe String
        , account_url : Maybe String
    }


createDisplayAccount : String -> Maybe String -> Maybe String -> Maybe String -> DisplayAccount {}
createDisplayAccount address display_name image_url account_url =
    { address = address
    , display_name = display_name
    , image_url = image_url
    , account_url = account_url
    }


createCompAccount : String -> Maybe String -> Maybe String -> Maybe String -> Decimal -> Decimal -> Decimal -> Int -> Int -> DisplayAccount {} -> Maybe Int -> Maybe (List CompAccountTransaction) -> Int -> Maybe CrowdProposal -> CompAccount
createCompAccount address display_name image_url account_url balance votes vote_weight proposals_created proposals_voted delegate rank transactions total_delegates crowd_proposal =
    { address = address
    , display_name = display_name
    , image_url = image_url
    , account_url = account_url
    , balance = balance
    , votes = votes
    , vote_weight = vote_weight
    , proposals_created = proposals_created
    , proposals_voted = proposals_voted
    , delegate = delegate
    , rank = rank
    , transactions = transactions
    , total_delegates = total_delegates
    , crowd_proposal = crowd_proposal
    }


type alias CompAccountTransaction =
    { title : String
    , timestamp : Time.Posix
    , trx_hash : String
    , delta : Decimal
    }


type alias ProposalAction =
    { title : String
    , target : String
    , value : Decimal
    , signature : String
    , data : String
    }


type CrowdProposalStateEnum
    = GatheringVotes
    | Proposed
    | Canceled


type alias CrowdProposal =
    { proposal_address : String
    , title : String
    , author : DisplayAccount {}
    , description : String
    , state : CrowdProposalStateEnum
    , actions : List ProposalAction
    , create_time : Time.Posix
    , propose_time : Maybe Time.Posix
    }
