module CompoundApi.Governance.ProposalService.Encoders exposing (encodeProposalStateEnum)

import CompoundApi.Governance.ProposalService.Models exposing (ProposalStateEnum(..))


encodeProposalStateEnum : ProposalStateEnum -> String
encodeProposalStateEnum stateEnum =
    case stateEnum of
        Pending ->
            "pending"

        Active ->
            "active"

        Succeeded ->
            "succeeded"

        Failed ->
            "defeated"

        Queued ->
            "queued"

        Executed ->
            "executed"

        Canceled ->
            "canceled"

        Expired ->
            "expired"
