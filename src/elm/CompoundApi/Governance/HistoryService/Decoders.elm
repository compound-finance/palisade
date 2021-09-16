module CompoundApi.Governance.HistoryService.Decoders exposing (governanceHistoryResponseDecoder)

import CompoundApi.Common.Decoders exposing (apiErrorDecoder)
import CompoundApi.Governance.HistoryService.Models exposing (GovernanceHistoryResponse)
import CompoundComponents.Eth.Decoders exposing (stringDecimal)
import Json.Decode exposing (Decoder, field, int, map6)


governanceHistoryResponseDecoder : Decoder GovernanceHistoryResponse
governanceHistoryResponseDecoder =
    map6 GovernanceHistoryResponse
        (field "error" apiErrorDecoder)
        (field "proposals_created" int)
        (field "token_holders" int)
        (field "total_comp_allocated" stringDecimal)
        (field "votes_delegated" stringDecimal)
        (field "voting_addresses" int)
