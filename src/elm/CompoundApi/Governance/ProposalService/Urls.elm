module CompoundApi.Governance.ProposalService.Urls exposing (proposalsRequestUrl)

import CompoundApi.Common.Url
import CompoundApi.Governance.ProposalService.Encoders exposing (encodeProposalStateEnum)
import CompoundApi.Governance.ProposalService.Models exposing (ProposalsRequest)
import CompoundComponents.Eth.Network exposing (Network)
import Dict exposing (Dict)
import Url.Builder as UrlBuilder


proposalsRequestUrl : Dict String String -> Network -> ProposalsRequest -> Maybe String
proposalsRequestUrl apiBaseUrlMap network proposalRequest =
    let
        requiredQueryParams =
            [ UrlBuilder.string "page_size" (String.fromInt proposalRequest.page_size)
            , UrlBuilder.string "page_number" (String.fromInt proposalRequest.page_number)
            ]

        intermediateQueryParams =
            requiredQueryParams
                ++ (case proposalRequest.proposal_ids of
                        [] ->
                            []

                        proposalIdsList ->
                            let
                                proposalsQueryArg =
                                    proposalIdsList
                                        |> List.map String.fromInt
                                        |> String.join ","
                            in
                            [ UrlBuilder.string "proposal_ids" proposalsQueryArg ]
                   )
                ++ (case proposalRequest.with_detail of
                        Just True ->
                            [ UrlBuilder.string "with_detail" "true" ]

                        Just False ->
                            [ UrlBuilder.string "with_detail" "false" ]

                        Nothing ->
                            []
                   )

        finalQueryParams =
            case proposalRequest.state of
                Just actualStateFilter ->
                    intermediateQueryParams
                        ++ [ UrlBuilder.string "state" (encodeProposalStateEnum actualStateFilter) ]

                Nothing ->
                    intermediateQueryParams
    in
    CompoundApi.Common.Url.buildApiUrl apiBaseUrlMap network "v2/governance/proposals" finalQueryParams
