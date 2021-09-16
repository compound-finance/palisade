module CompoundApi.Governance.CompService.Models exposing
    ( AccountCompDistibution
    , AccountCompDistributionRequest
    , AccountCompDistributionResponse
    , MarketCompDistibution
    , MarketCompDistributionRequest
    , MarketCompDistributionResponse
    )

import CompoundApi.Common.Models exposing (API_Error, PaginationSummary)
import CompoundApi.Governance.Common.Models exposing (CompAccount)
import Decimal exposing (Decimal)


type AccountFilterByEnum
    = Votes
    | Balance
    | ProposalsCreated


type alias MarketCompDistibution =
    { address : String
    , borrower_daily_comp : Decimal
    , comp_allocated : Decimal
    , comp_borrow_index : Decimal
    , comp_distributed : Decimal
    , comp_speed : Decimal
    , comp_supply_index : Decimal
    , name : String
    , supplier_daily_comp : Decimal
    , symbol : String
    , underlying_address : Maybe String
    , underlying_name : String
    , underlying_symbol : String
    }


type alias AccountCompDistibution =
    { address : String
    , comp_allocated : Decimal
    , comp_borrow_index : Decimal
    , comp_distributed : Decimal
    , comp_supply_index : Decimal
    , daily_comp : Decimal
    , name : String
    , symbol : String
    , underlying_address : Maybe String
    , underlying_name : String
    , underlying_symbol : String
    }


type alias MarketCompDistributionRequest =
    { addresses : List String
    }


type alias MarketCompDistributionResponse =
    { comp_rate : Decimal
    , daily_comp : Decimal
    , markets : List MarketCompDistibution
    , request : MarketCompDistributionRequest
    , total_comp_allocated : Decimal
    , total_comp_distributed : Decimal
    }


type alias AccountCompDistributionRequest =
    { address : String
    }


type alias AccountCompDistributionResponse =
    { markets : List AccountCompDistibution
    , request : AccountCompDistributionRequest
    }
