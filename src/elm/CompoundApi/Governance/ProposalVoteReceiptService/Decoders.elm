module CompoundApi.Governance.ProposalVoteReceiptService.Decoders exposing (proposalVoteReceiptResponseDecoder)

import CompoundApi.Common.Decoders exposing (apiErrorDecoder, paginationSummaryDecoder)
import CompoundApi.Governance.Common.Decoders exposing (displayAccountDecoder)
import CompoundApi.Governance.ProposalService.Decoders exposing (proposalDecoder)
import CompoundApi.Governance.ProposalVoteReceiptService.Models
    exposing
        ( ProposalVoteReceipt
        , ProposalVoteReceiptRequest
        , ProposalVoteReceiptResponse
        , createProposalVoteReceipt
        )
import CompoundComponents.Eth.Decoders exposing (stringDecimal)
import Decimal exposing (Decimal)
import Json.Decode exposing (Decoder, andThen, bool, fail, field, int, list, map, map4, maybe, nullable, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Time


proposalVoteReceiptResponseDecoder : Decoder ProposalVoteReceiptResponse
proposalVoteReceiptResponseDecoder =
    map4 ProposalVoteReceiptResponse
        (field "request"
            (succeed ProposalVoteReceiptRequest
                |> optional "proposal_id" (nullable int) Nothing
                |> optional "account" (nullable string) Nothing
                |> optional "support" (nullable bool) Nothing
                |> required "with_proposal_data" bool
                |> required "page_size" int
                |> required "page_number" int
            )
        )
        (field "pagination_summary" paginationSummaryDecoder)
        (field "proposal_vote_receipts"
            (list proposalVoteReceiptDecoder)
        )
        (field "error" apiErrorDecoder)


proposalVoteReceiptDecoder : Json.Decode.Decoder ProposalVoteReceipt
proposalVoteReceiptDecoder =
    succeed createProposalVoteReceipt
        |> required "proposal_id" int
        |> required "voter" displayAccountDecoder
        |> required "support" bool
        |> required "votes" stringDecimal
        |> optional "proposal" (nullable proposalDecoder) Nothing
