module Utils.CTokenHelper exposing (CTokenType(..), getAllSupportedCTokens)

import Balances
import Decimal
import Dict
import Eth.Compound exposing (CompoundState)
import Eth.Token exposing (CToken, TokenState)


type CTokenType
    = ForCollateral
    | ForBorrow


getAllSupportedCTokens : CompoundState -> TokenState -> CTokenType -> List CToken
getAllSupportedCTokens compoundState tokenState cTokenType =
    tokenState.cTokens
        |> Dict.values
        |> List.filterMap
            (\cToken ->
                let
                    mintPaused =
                        Balances.getMintGuardianPaused compoundState.cTokensMetadata cToken.contractAddress && cTokenType == ForCollateral
                in
                if cToken.symbol == "cSAI" || cToken.symbol == "cREP" || cToken.symbol == "cWBTC" || cToken.symbol == "cFEI" || mintPaused then
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
