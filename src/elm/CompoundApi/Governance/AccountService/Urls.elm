module CompoundApi.Governance.AccountService.Urls exposing (governanceAccountRequestUrl, governanceAccountSearchRequestUrl)

import CompoundApi.Common.Url
import CompoundApi.Governance.AccountService.Encoders exposing (encodeAccountFilterEnum)
import CompoundApi.Governance.AccountService.Models exposing (GovernanceAccountRequest, GovernanceAccountSearchRequest)
import CompoundComponents.Eth.Network exposing (Network)
import Dict exposing (Dict)
import Url.Builder as UrlBuilder


governanceAccountRequestUrl : Dict String String -> Network -> GovernanceAccountRequest -> Maybe String
governanceAccountRequestUrl apiBaseUrlMap network accountRequest =
    let
        requiredQueryParams =
            [ UrlBuilder.string "page_size" (String.fromInt accountRequest.page_size)
            , UrlBuilder.string "page_number" (String.fromInt accountRequest.page_number)
            ]
                ++ (if accountRequest.with_history then
                        [ UrlBuilder.string "with_history" "true" ]

                    else
                        [ UrlBuilder.string "with_history" "false" ]
                   )

        intermediateQueryParams =
            requiredQueryParams
                ++ (case accountRequest.addresses of
                        [] ->
                            []

                        addressList ->
                            let
                                addressesQueryArg =
                                    addressList
                                        |> String.join ","
                            in
                            [ UrlBuilder.string "addresses" addressesQueryArg ]
                   )

        finalQueryParams =
            case accountRequest.order_by of
                Just actualOrderBy ->
                    intermediateQueryParams
                        ++ [ UrlBuilder.string "order_by" (encodeAccountFilterEnum actualOrderBy) ]

                Nothing ->
                    intermediateQueryParams
    in
    CompoundApi.Common.Url.buildApiUrl apiBaseUrlMap network "v2/governance/accounts" finalQueryParams


governanceAccountSearchRequestUrl : Dict String String -> Network -> GovernanceAccountSearchRequest -> Maybe String
governanceAccountSearchRequestUrl apiBaseUrlMap network accountRequest =
    let
        queryParams =
            [ UrlBuilder.string "page_size" (String.fromInt accountRequest.page_size)
            , UrlBuilder.string "page_number" (String.fromInt accountRequest.page_number)
            , UrlBuilder.string "term" accountRequest.term
            ]
    in
    CompoundApi.Common.Url.buildApiUrl apiBaseUrlMap network "v2/governance/accounts/search" queryParams
