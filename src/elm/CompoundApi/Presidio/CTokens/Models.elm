module CompoundApi.Presidio.CTokens.Models exposing
    ( CToken
    , CTokenRequest
    , CTokenResponse
    , Metadata
    )

import CompoundApi.Common.Models exposing (API_Error)
import Decimal exposing (Decimal)


type alias CTokenRequest =
    { addresses : List String
    , block_number : Maybe Int
    , block_timestamp : Maybe Int
    }


type alias CTokenResponse =
    { request : CTokenRequest
    , cToken : List CToken
    , error : Maybe API_Error
    , meta : Maybe Metadata
    }


type alias CToken =
    { borrow_rate : Decimal
    , borrow_cap : Decimal
    , cash : Decimal
    , collateral_factor : Decimal
    , exchange_rate : Decimal
    , interest_rate_model_address : String
    , name : String
    , number_of_borrowers : Int
    , number_of_suppliers : Int
    , reserves : Decimal
    , reserve_factor : Decimal
    , supply_rate : Decimal
    , symbol : String
    , token_address : String
    , total_borrows : Decimal
    , total_supply : Decimal
    , underlying_address : Maybe String --cether
    , underlying_name : String
    , underlying_price : Decimal
    , underlying_symbol : String
    , comp_supply_apy : Decimal
    , comp_borrow_apy : Decimal
    , total_supply_value_in_eth : Decimal
    , total_borrow_value_in_eth : Decimal
    , underlying_borrow_cap_in_eth : Decimal
    }


type alias Metadata =
    { unique_borrowers : Int
    , unique_suppliers : Int
    }
