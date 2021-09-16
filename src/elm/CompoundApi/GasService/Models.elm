module CompoundApi.GasService.Models exposing
    ( API_GasPriceRequest
    , API_GasPriceResponse
    )

import CompoundApi.Common.Models exposing (API_Error)
import Decimal exposing (Decimal)


type alias API_GasPriceRequest =
    {}


type alias API_GasPriceResponse =
    { average : Decimal
    , fast : Decimal
    , fastest : Decimal
    , safe_low : Decimal
    }
