module Utils.CTokenHelper exposing (getAllSupportedCTokens)

import Balances
import Decimal exposing (Decimal)
import Dict
import Eth.Compound exposing (CompoundState)
import Eth.Token exposing (CToken, TokenState)


getAllSupportedCTokens : CompoundState -> TokenState -> List CToken
getAllSupportedCTokens compoundState tokenState =
    tokenState.cTokens
        |> Dict.values
        |> List.filterMap
            (\cToken ->
                if cToken.symbol == "cSAI" || cToken.symbol == "cREP" || cToken.symbol == "cWBTC" then
                    let
                        underlyingBalances =
                            Balances.getUnderlyingBalances compoundState cToken.contractAddress

                        ( supplyBalance, borrowBalance ) =
                            ( underlyingBalances
                                |> Maybe.map .underlyingSupplyBalance
                                |> Maybe.withDefault Decimal.zero
                            , underlyingBalances
                                |> Maybe.map .underlyingBorrowBalance
                                |> Maybe.withDefault Decimal.zero
                            )
                    in
                    if Decimal.eq supplyBalance Decimal.zero && Decimal.eq borrowBalance Decimal.zero then
                        Nothing

                    else
                        Just cToken

                else
                    Just cToken
            )
