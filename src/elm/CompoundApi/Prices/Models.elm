module CompoundApi.Prices.Models exposing
    ( PricesRequest
    , PricesResponse
    , Token
    )

import CompoundApi.Common.Models exposing (API_Error)
import Decimal exposing (Decimal)


type alias PricesRequest =
    { addresses : List String
    , block_number : Maybe Int
    , block_timestamp : Maybe Int
    }


type alias PricesResponse =
    { request : PricesRequest
    , tokens : List Token
    , error : Maybe API_Error
    }


type alias Token =
    { address : String
    , name : String
    , symbol : String
    , price : Decimal
    , price_block_number : Maybe Int
    , price_timestamp : Maybe Int
    }
