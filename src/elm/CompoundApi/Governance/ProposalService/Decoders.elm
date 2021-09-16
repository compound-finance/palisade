module CompoundApi.Governance.ProposalService.Decoders exposing (proposalDecoder, proposalStateDecoder, proposalWithDetailDecoder, proposalsResponseDecoder, proposalsWithDetailResponseDecoder)

import CompoundApi.Common.Decoders exposing (apiErrorDecoder, paginationSummaryDecoder)
import CompoundApi.Governance.Common.Decoders exposing (displayAccountDecoder, proposalActionDecoder)
import CompoundApi.Governance.ProposalService.Models
    exposing
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
import CompoundComponents.Eth.Decoders exposing (stringDecimal)
import Decimal
import Json.Decode exposing (Decoder, andThen, bool, fail, field, int, list, map, map4, map5, maybe, nullable, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Time


proposalStateEnumDecoder : Json.Decode.Decoder ProposalStateEnum
proposalStateEnumDecoder =
    let
        proposalStateStringDecoder : String -> Decoder ProposalStateEnum
        proposalStateStringDecoder stateString =
            case stateString of
                "pending" ->
                    succeed Pending

                "active" ->
                    succeed Active

                "succeeded" ->
                    succeed Succeeded

                "defeated" ->
                    succeed Failed

                "failed" ->
                    succeed Failed

                "queued" ->
                    succeed Queued

                "executed" ->
                    succeed Executed

                "canceled" ->
                    succeed Canceled

                "expired" ->
                    succeed Expired

                _ ->
                    fail ("Unrecognized proposal state: " ++ stateString)
    in
    string
        |> andThen proposalStateStringDecoder


proposalsResponseDecoder : Decoder ProposalsResponse
proposalsResponseDecoder =
    map4 ProposalsResponse
        (field "request"
            (map5 ProposalsRequest
                (field "proposal_ids"
                    (list int)
                )
                (field "state" (maybe proposalStateEnumDecoder))
                (field "page_size" int)
                (field "page_number" int)
                (field "with_detail" (maybe bool))
            )
        )
        (field "pagination_summary" paginationSummaryDecoder)
        (field "proposals"
            (list proposalDecoder)
        )
        (field "error" apiErrorDecoder)


proposalsWithDetailResponseDecoder : Decoder ProposalsWithDetailResponse
proposalsWithDetailResponseDecoder =
    map4 ProposalsWithDetailResponse
        (field "request"
            (map5 ProposalsRequest
                (field "proposal_ids"
                    (list int)
                )
                (field "state" (maybe proposalStateEnumDecoder))
                (field "page_size" int)
                (field "page_number" int)
                (field "with_detail" (maybe bool))
            )
        )
        (field "pagination_summary" paginationSummaryDecoder)
        (field "proposals"
            (list proposalWithDetailDecoder)
        )
        (field "error" apiErrorDecoder)


proposalDecoder : Json.Decode.Decoder (Proposal {})
proposalDecoder =
    succeed createProposal
        |> required "id" int
        |> required "title" string
        |> required "description" string
        |> required "states" (list proposalStateDecoder)
        |> required "for_votes" stringDecimal
        |> required "against_votes" stringDecimal
        |> optional "abstain_votes" stringDecimal Decimal.zero


proposalWithDetailDecoder : Json.Decode.Decoder ProposalWithDetail
proposalWithDetailDecoder =
    succeed createProposalWithDetail
        |> required "id" int
        |> required "title" string
        |> required "description" string
        |> required "proposer" displayAccountDecoder
        |> required "actions" (list proposalActionDecoder)
        |> required "states" (list proposalStateDecoder)
        |> required "for_votes" stringDecimal
        |> required "against_votes" stringDecimal
        |> optional "abstain_votes" stringDecimal Decimal.zero


proposalStateDecoder : Json.Decode.Decoder ProposalState
proposalStateDecoder =
    succeed ProposalState
        |> required "state" proposalStateEnumDecoder
        |> required "start_time" (int |> map ((*) 1000) |> map Time.millisToPosix)
        |> optional "end_time" (nullable (int |> map ((*) 1000) |> map Time.millisToPosix)) Nothing
        |> optional "trx_hash" (nullable string) Nothing
