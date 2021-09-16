module CompoundApi.Presidio.Accounts.Encoders exposing (accountsRequestEncoder)

import CompoundApi.Common.Encoders exposing (encodeAPIPrecise)
import CompoundApi.Presidio.Accounts.Models exposing (AccountRequest)
import Json.Encode


accountsRequestEncoder : AccountRequest -> Json.Encode.Value
accountsRequestEncoder request =
    let
        requiredParams =
            [ ( "addresses", Json.Encode.list Json.Encode.string request.addresses )
            , ( "block_number", Json.Encode.int request.block_number )
            , ( "page_size", Json.Encode.int request.page_size )
            , ( "page_number", Json.Encode.int request.page_number )
            ]

        minBorrowParam =
            case request.min_borrow_value_in_eth of
                Just actualMinBorrowValue ->
                    [ ( "min_borrow_value_in_eth", encodeAPIPrecise actualMinBorrowValue ) ]

                Nothing ->
                    []

        maxHealthParam =
            case request.max_health of
                Just actualMaxHealth ->
                    [ ( "max_health", encodeAPIPrecise actualMaxHealth ) ]

                Nothing ->
                    []
    in
    Json.Encode.object
        ( requiredParams
            ++ minBorrowParam
            ++ maxHealthParam
        )
