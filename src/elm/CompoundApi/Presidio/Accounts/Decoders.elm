module CompoundApi.Presidio.Accounts.Decoders exposing (accountsResponseDecoder)

import CompoundApi.Common.Decoders
import CompoundApi.Common.Models exposing (API_Error)
import CompoundApi.Presidio.Accounts.Models exposing (Account, AccountCToken, AccountRequest, AccountResponse)
import CompoundComponents.Eth.Decoders
import Decimal exposing (Decimal)
import Json.Decode


accountsResponseDecoder : Json.Decode.Decoder AccountResponse
accountsResponseDecoder =
    Json.Decode.map4 AccountResponse
        (Json.Decode.field "request"
            (Json.Decode.map6 AccountRequest
                (Json.Decode.field "addresses"
                    (Json.Decode.list Json.Decode.string)
                )
                (Json.Decode.field "min_borrow_value_in_eth" (Json.Decode.nullable CompoundApi.Common.Decoders.apiPrecise))
                (Json.Decode.field "max_health" (Json.Decode.nullable CompoundApi.Common.Decoders.apiPrecise))
                (Json.Decode.field "block_number" Json.Decode.int)
                (Json.Decode.field "page_size" Json.Decode.int)
                (Json.Decode.field "page_number" Json.Decode.int)
            )
        )
        (Json.Decode.field "pagination_summary" CompoundApi.Common.Decoders.paginationSummaryDecoder)
        (Json.Decode.field "accounts"
            (Json.Decode.list accountDecoder)
        )
        (Json.Decode.field "error" CompoundApi.Common.Decoders.apiErrorDecoder)


accountDecoder : Json.Decode.Decoder Account
accountDecoder =
    Json.Decode.map6 Account
        (Json.Decode.field "address" Json.Decode.string)
        (Json.Decode.field "total_collateral_value_in_eth" CompoundApi.Common.Decoders.apiPrecise)
        (Json.Decode.field "total_borrow_value_in_eth" CompoundApi.Common.Decoders.apiPrecise)
        (Json.Decode.field "health" (Json.Decode.succeed Decimal.zero))
        --TODO: Fix me
        (Json.Decode.field "block_updated" (Json.Decode.succeed 0))
        --TODO: Fix me
        (Json.Decode.field "tokens"
            (Json.Decode.list accountCTokenDecoder)
        )


accountCTokenDecoder : Json.Decode.Decoder AccountCToken
accountCTokenDecoder =
    Json.Decode.map6 AccountCToken
        (Json.Decode.field "address" Json.Decode.string)
        (Json.Decode.field "symbol" (Json.Decode.succeed "NOPE"))
        --TODO: Fix me
        (Json.Decode.field "supply_balance_underlying" CompoundApi.Common.Decoders.apiPrecise)
        (Json.Decode.field "borrow_balance_underlying" CompoundApi.Common.Decoders.apiPrecise)
        (Json.Decode.field "lifetime_supply_interest_accrued" (Json.Decode.nullable CompoundApi.Common.Decoders.apiPrecise))
        (Json.Decode.field "lifetime_borrow_interest_accrued" (Json.Decode.nullable CompoundApi.Common.Decoders.apiPrecise))
