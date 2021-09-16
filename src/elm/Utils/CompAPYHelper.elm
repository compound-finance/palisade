module Utils.CompAPYHelper exposing (..)

import Decimal exposing (Decimal)


pow365 : Decimal -> Decimal
pow365 num =
    (Decimal.toFloat num ^ 365)
        |> Decimal.fromFloat
        |> Maybe.withDefault Decimal.zero


compRate : Decimal -> Decimal -> Decimal -> Maybe Decimal
compRate compUSDPrice compSpeedPerDay marketTotalUSDValue =
    Decimal.fastdiv (Decimal.mul compUSDPrice compSpeedPerDay) marketTotalUSDValue
        |> Maybe.map
            (\compOverMarket ->
                Decimal.sub (pow365 (Decimal.add Decimal.one compOverMarket)) Decimal.one
            )
