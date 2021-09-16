module CompoundApi.Governance.ProposalVoteReceiptService.Urls exposing (voteReceiptsRequestUrl)

import CompoundApi.Common.Url
import CompoundApi.Governance.ProposalVoteReceiptService.Models exposing (ProposalVoteReceiptRequest)
import CompoundComponents.Eth.Network exposing (Network)
import Dict exposing (Dict)
import Url.Builder as UrlBuilder


voteReceiptsRequestUrl : Dict String String -> Network -> ProposalVoteReceiptRequest -> Maybe String
voteReceiptsRequestUrl apiBaseUrlMap network request =
    let
        requiredQueryParams =
            [ UrlBuilder.string "page_size" (String.fromInt request.page_size)
            , UrlBuilder.string "page_number" (String.fromInt request.page_number)
            ]

        finalQueryParams =
            requiredQueryParams
                ++ (case request.support of
                        Just support ->
                            if support then
                                [ UrlBuilder.string "support" "true" ]

                            else
                                [ UrlBuilder.string "support" "false" ]

                        Nothing ->
                            []
                   )
                ++ (case request.proposal_id of
                        Just id ->
                            [ UrlBuilder.string "proposal_id" (String.fromInt id) ]

                        Nothing ->
                            []
                   )
                ++ (case request.account of
                        Just accountAddress ->
                            [ UrlBuilder.string "account" accountAddress ]

                        Nothing ->
                            []
                   )
                ++ (if request.with_proposal_data then
                        [ UrlBuilder.string "with_proposal_data" "true" ]

                    else
                        [ UrlBuilder.string "with_proposal_data" "false" ]
                   )
    in
    CompoundApi.Common.Url.buildApiUrl apiBaseUrlMap network "v2/governance/proposal_vote_receipts" finalQueryParams
