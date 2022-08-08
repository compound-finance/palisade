module CompoundApi.Presidio.Accounts.Urls exposing (accountsRequestUrl)

import CompoundApi.Common.Url
import CompoundApi.Presidio.Accounts.Models exposing (AccountRequest)
import CompoundComponents.Eth.Network exposing (Network)
import Decimal
import Dict exposing (Dict)
import Url.Builder as UrlBuilder


accountsRequestUrl : Dict String String -> Network -> AccountRequest -> Maybe String
accountsRequestUrl apiBaseUrlMap network accountsRequest =
    let
        requiredQueryParams =
            [ UrlBuilder.string "page_size" (String.fromInt accountsRequest.page_size)
            , UrlBuilder.string "page_number" (String.fromInt accountsRequest.page_number)
            , UrlBuilder.string "block_number" (String.fromInt accountsRequest.block_number)
            ]

        intermediateQueryParams =
            requiredQueryParams
                ++ (case accountsRequest.addresses of
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
            intermediateQueryParams
                ++ (case accountsRequest.min_borrow_value_in_eth of
                        Just minBorrowValueEth ->
                            [ UrlBuilder.string "min_borrow_value_in_eth[value]" (Decimal.toString minBorrowValueEth) ]

                        Nothing ->
                            []
                   )
                ++ (case accountsRequest.max_health of
                        Just maxHealth ->
                            [ UrlBuilder.string "max_health[value]" (Decimal.toString maxHealth) ]

                        Nothing ->
                            []
                   )
    in
    CompoundApi.Common.Url.buildApiUrl apiBaseUrlMap network "account" finalQueryParams
