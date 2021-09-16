module CompoundApi.Governance.AccountService.Encoders exposing (encodeAccountFilterEnum)

import CompoundApi.Governance.AccountService.Models exposing (AccountFilterByEnum(..))


encodeAccountFilterEnum : AccountFilterByEnum -> String
encodeAccountFilterEnum accountFilterEnum =
    case accountFilterEnum of
        Votes ->
            "votes"

        Balance ->
            "balance"

        ProposalsCreated ->
            "proposals_created"
