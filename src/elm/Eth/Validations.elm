module Eth.Validations exposing (hasSufficientBalanceForSupply, hasSufficientCollateralForBorrow)

import Balances
import CompoundComponents.Eth.Ethereum exposing (Account(..), AssetAddress(..), CustomerAddress(..))
import CompoundComponents.Eth.Ledger exposing (LedgerAccount(..))
import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Eth.Compound exposing (CompoundState)
import Eth.Config exposing (Config)
import Eth.Oracle exposing (OracleState)
import Eth.Token exposing (CToken, Token, TokenState)



{-
   These becomes difficult since we need to check a few things, such as:

       1. (For supply) Do you have a sufficient balance of the asset to supply it?
       2. (For supply) Has the asset been turned "on"?
       3. (For withdrawal), does the Money Market have enough of the asset such that you can withdraw
       etc...

    The goal for now is to slowly build out the validations we think we need so that
    the user doesn't get error messages.
-}


hasSufficientBalanceForSupply : Config -> Account -> CompoundState -> CToken -> Decimal -> Bool
hasSufficientBalanceForSupply config account compoundState cToken amount =
    case Balances.getWalletBalanceSafeEther config account compoundState cToken of
        Just balance ->
            Decimal.gte balance amount

        Nothing ->
            False


hasSufficientCollateralForBorrow : Maybe Decimal -> OracleState -> Token -> Decimal -> Bool
hasSufficientCollateralForBorrow maybeAccountLiquidityUsd oracleState token amount =
    case maybeAccountLiquidityUsd of
        Nothing ->
            False

        Just maxBorrowAvailableUsd ->
            case Eth.Oracle.getOraclePrice oracleState token of
                Nothing ->
                    False

                Just tokenPriceUsd ->
                    let
                        totalPrice =
                            Decimal.mul tokenPriceUsd amount
                    in
                    Decimal.lte totalPrice maxBorrowAvailableUsd
