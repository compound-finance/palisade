module CompoundApi.Governance.AccountService.Decoders exposing (governanceAccountResponseDecoder, governanceAccountSearchResponseDecoder)

import CompoundApi.Common.Decoders exposing (apiErrorDecoder, paginationSummaryDecoder)
import CompoundApi.Governance.AccountService.Models
    exposing
        ( AccountFilterByEnum(..)
        , GovernanceAccountRequest
        , GovernanceAccountResponse
        , GovernanceAccountSearchRequest
        , GovernanceAccountSearchResponse
        )
import CompoundApi.Governance.Common.Decoders exposing (compAccountDecoder)
import Json.Decode exposing (Decoder, andThen, bool, fail, field, int, list, map3, map4, map5, maybe, string, succeed)


accountFilterByDecoder : Json.Decode.Decoder AccountFilterByEnum
accountFilterByDecoder =
    let
        accountFilterStringDecoder : String -> Decoder AccountFilterByEnum
        accountFilterStringDecoder accountFilterString =
            case accountFilterString of
                "votes" ->
                    succeed Votes

                "balance" ->
                    succeed Balance

                "proposals_created" ->
                    succeed ProposalsCreated

                _ ->
                    fail ("Unrecognized account filter option: " ++ accountFilterString)
    in
    string
        |> andThen accountFilterStringDecoder


governanceAccountResponseDecoder : Decoder GovernanceAccountResponse
governanceAccountResponseDecoder =
    map4 GovernanceAccountResponse
        (field "request"
            (map5 GovernanceAccountRequest
                (field "addresses"
                    (list string)
                )
                (field "order_by" (maybe accountFilterByDecoder))
                (field "page_size" int)
                (field "page_number" int)
                (field "with_history" bool)
            )
        )
        (field "pagination_summary" paginationSummaryDecoder)
        (field "accounts"
            (list compAccountDecoder)
        )
        (field "error" apiErrorDecoder)


governanceAccountSearchResponseDecoder : Decoder GovernanceAccountSearchResponse
governanceAccountSearchResponseDecoder =
    map4 GovernanceAccountSearchResponse
        (field "request"
            (map3 GovernanceAccountSearchRequest
                (field "term" string)
                (field "page_size" int)
                (field "page_number" int)
            )
        )
        (field "pagination_summary" paginationSummaryDecoder)
        (field "accounts"
            (list compAccountDecoder)
        )
        (field "error" apiErrorDecoder)
