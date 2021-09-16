module CompoundApi.Presidio.Accounts.Models exposing
    ( Account
    , AccountCToken
    , AccountRequest
    , AccountResponse
    )

import CompoundApi.Common.Models exposing (API_Error, PaginationSummary)
import Decimal exposing (Decimal)


type alias AccountRequest =
    { addresses : List String
    , min_borrow_value_in_eth : Maybe Decimal
    , max_health : Maybe Decimal -- This is => TotalCollateral / TotalBorrow
    , block_number : Int -- Send 0 to always get latest
    , page_size : Int
    , page_number : Int -- This starts at page 1
    }


type alias AccountResponse =
    { request : AccountRequest
    , pagination_summary : PaginationSummary
    , accounts : List Account
    , error : Maybe API_Error
    }


type alias Account =
    { address : String
    , total_collateral_value_in_eth : Decimal
    , total_borrow_value_in_eth : Decimal
    , health : Decimal
    , block_updated : Int
    , tokens : List AccountCToken
    }


type alias AccountCToken =
    { address : String
    , symbol : String
    , supply_balance_underlying : Decimal
    , borrow_balance_underlying : Decimal
    , lifetime_supply_interest_accrued : Maybe Decimal
    , lifetime_borrow_interest_accrued : Maybe Decimal
    }
