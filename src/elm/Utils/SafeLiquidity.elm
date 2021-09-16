module Utils.SafeLiquidity exposing
    ( UserLiquidityStatus(..)
    , getAbsoluteMaxBorrowForToken
    , getAbsoluteMaxBorrowInUsd
    , getAbsoluteMaxWithdrawForToken
    , getCurrentBorrowLimitUsd
    , getSafeMaxBorrowForToken
    , getSafeMaxBorrowInUsd
    , getSafeMaxWithdrawForToken
    , getUserCollateralBorrowedRate
    , getUserCollateralizationStatus
    )

import Balances
import CompoundComponents.Eth.Ethereum as Ethereum
import CompoundComponents.Functions as Functions
import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Eth.Compound exposing (CompoundMsg, CompoundState)
import Eth.Config exposing (Config)
import Eth.Oracle exposing (OracleState)
import Eth.Token exposing (CToken, Token, TokenState)


safeMaxCollateralValueBorrowPercentage : Decimal
safeMaxCollateralValueBorrowPercentage =
    Decimal.fromString "0.8"
        |> Maybe.withDefault Decimal.one


safeMaxCollateralValueWithdrawPercentage : Decimal
safeMaxCollateralValueWithdrawPercentage =
    Decimal.fromString "1.25"
        |> Maybe.withDefault Decimal.one


type UserLiquidityStatus
    = Safe
    | Caution
    | AtRisk


getUserCollateralBorrowedRate : Eth.Compound.CompoundState -> List CToken -> Eth.Oracle.OracleState -> Maybe Decimal
getUserCollateralBorrowedRate compoundState cTokens oracleState =
    let
        balanceTotalsUsd =
            Balances.getUnderlyingTotalsInUsd compoundState cTokens oracleState

        collateralValueUsd =
            Balances.getCollateralValueInUsd compoundState cTokens oracleState
    in
    Decimal.fastdiv balanceTotalsUsd.totalBorrow collateralValueUsd


getUserCollateralizationStatus : Eth.Compound.CompoundState -> List CToken -> Eth.Oracle.OracleState -> Maybe UserLiquidityStatus
getUserCollateralizationStatus compoundState cTokens oracleState =
    let
        balanceTotalsUsd =
            Balances.getUnderlyingTotalsInUsd compoundState cTokens oracleState

        atRiskDecimalPercent =
            Decimal.fromString "0.9"
                |> Maybe.withDefault Decimal.one

        cautionDecimalPercent =
            Decimal.fromString "0.75"
                |> Maybe.withDefault Decimal.zero
    in
    if Decimal.eq balanceTotalsUsd.totalBorrow Decimal.zero then
        Just Safe

    else
        case getUserCollateralBorrowedRate compoundState cTokens oracleState of
            Just collateralBorredRate ->
                if Decimal.gt collateralBorredRate atRiskDecimalPercent then
                    Just AtRisk

                else if Decimal.gt collateralBorredRate cautionDecimalPercent then
                    Just Caution

                else
                    Just Safe

            Nothing ->
                Nothing



-- Maximum borrow allowed by the user is just their account liquidity


getAbsoluteMaxBorrowInUsd : CompoundState -> OracleState -> Decimal
getAbsoluteMaxBorrowInUsd compoundState oracleState =
    case compoundState.maybeAccountLiquidityUsd of
        Just accountLiquidityUsd ->
            accountLiquidityUsd

        _ ->
            Decimal.zero


getAbsoluteMaxBorrowForToken : CompoundState -> OracleState -> Token -> Decimal
getAbsoluteMaxBorrowForToken compoundState oracleState token =
    let
        absoluteMaxBorrowUsd =
            getAbsoluteMaxBorrowInUsd compoundState oracleState
    in
    calculateMaxBorrowForToken absoluteMaxBorrowUsd oracleState token



-- Safe max borrow allows the user up to 80% of their collateral value which is
-- the greater of:
-- 1. 0
-- or
-- 2. accountLiquidity - (CollateralValue * (1-0.8))


getSafeMaxBorrowInUsd : CompoundState -> TokenState -> OracleState -> Decimal
getSafeMaxBorrowInUsd compoundState tokenState oracleState =
    let
        cTokens =
            Dict.values tokenState.cTokens

        collateralValueUsd =
            Balances.getCollateralValueInUsd compoundState cTokens oracleState

        safeMinLiquidityPercent =
            Decimal.sub Decimal.one safeMaxCollateralValueBorrowPercentage

        safeMinLiquidityUsd =
            Decimal.mul collateralValueUsd safeMinLiquidityPercent

        maxLiquidityCanBorrow =
            Decimal.sub
                (getAbsoluteMaxBorrowInUsd compoundState oracleState)
                safeMinLiquidityUsd
    in
    Functions.decimalMax maxLiquidityCanBorrow Decimal.zero


getSafeMaxBorrowForToken : CompoundState -> TokenState -> OracleState -> Token -> Decimal
getSafeMaxBorrowForToken compoundState tokenState oracleState token =
    let
        maxSafeBorrowUsd =
            getSafeMaxBorrowInUsd compoundState tokenState oracleState
    in
    calculateMaxBorrowForToken maxSafeBorrowUsd oracleState token


calculateMaxBorrowForToken : Decimal -> OracleState -> Token -> Decimal
calculateMaxBorrowForToken maxBorrowInUsd oracleState token =
    case Eth.Oracle.getOraclePrice oracleState token of
        Just tokenPriceUsd ->
            Decimal.fastdiv maxBorrowInUsd tokenPriceUsd
                |> Maybe.withDefault Decimal.zero

        _ ->
            Decimal.zero


getAbsoluteMaxWithdrawForToken : Config -> CompoundState -> TokenState -> OracleState -> CToken -> Decimal -> Decimal
getAbsoluteMaxWithdrawForToken config compoundState tokenState oracleState cToken tokenSupplyBalance =
    calculateMaxWithdrawWithSafeFactor Decimal.one config compoundState tokenState oracleState cToken tokenSupplyBalance



-- Max withdrawable is token balance if the user has 0 borrows or is not entered in the specific asset,
-- else we can only withdraw a Collateral Value that does not take them below a health of
-- 1.25 (ie: account_total_collateral/account_total_borrow) which is the same as
-- saying the max withdrawable is (account_total_collateral - account_total_borrow*1.25).
-- Then we can subtract the target tokens collateral_value from the max withdrawable to get the max
-- percentage of underlying that a user can withdraw to keep at 1.25.


getSafeMaxWithdrawForToken : Config -> CompoundState -> TokenState -> OracleState -> CToken -> Decimal -> Decimal
getSafeMaxWithdrawForToken config compoundState tokenState oracleState cToken tokenSupplyBalance =
    calculateMaxWithdrawWithSafeFactor safeMaxCollateralValueWithdrawPercentage config compoundState tokenState oracleState cToken tokenSupplyBalance


calculateMaxWithdrawWithSafeFactor : Decimal -> Config -> CompoundState -> TokenState -> OracleState -> CToken -> Decimal -> Decimal
calculateMaxWithdrawWithSafeFactor collateralValuePercentage config compoundState tokenState oracleState cToken tokenSupplyBalance =
    let
        allCTokensList =
            tokenState.cTokens
                |> Dict.values

        balanceTotalsUsd =
            Balances.getUnderlyingTotalsInUsd compoundState allCTokensList oracleState

        maybeCTokenMetadata =
            compoundState.cTokensMetadata
                |> Dict.get (Ethereum.getContractAddressString cToken.contractAddress)

        maybeTokenPriceUsd =
            Eth.Oracle.getOraclePrice oracleState cToken.underlying
    in
    if Decimal.eq balanceTotalsUsd.totalBorrow Decimal.zero || not (Balances.hasEnteredAsset config compoundState cToken) then
        tokenSupplyBalance

    else
        case ( compoundState.maybeAccountLiquidityUsd, maybeCTokenMetadata, maybeTokenPriceUsd ) of
            ( Just maybeAccountLiquidityUsd, Just cTokenMetadata, Just tokenPriceUsd ) ->
                let
                    accountTotalCollateralUsd =
                        Balances.getCollateralValueInUsd compoundState allCTokensList oracleState

                    safeTotalCollateralUsd =
                        Decimal.mul balanceTotalsUsd.totalBorrow collateralValuePercentage

                    maxWithdrawableLiquidityUsd =
                        Decimal.sub accountTotalCollateralUsd safeTotalCollateralUsd

                    tokenUnderlyingBalances =
                        Balances.getUnderlyingTotalsInUsd compoundState [ cToken ] oracleState

                    tokenCollateralValueUsd =
                        tokenUnderlyingBalances.totalSupply
                            |> Decimal.mul cTokenMetadata.collateralFactor
                in
                if Decimal.lte maxWithdrawableLiquidityUsd Decimal.zero then
                    Decimal.zero

                else if Decimal.lt tokenCollateralValueUsd maxWithdrawableLiquidityUsd then
                    tokenSupplyBalance

                else
                    let
                        ratio =
                            Decimal.fastdiv maxWithdrawableLiquidityUsd tokenCollateralValueUsd
                                |> Maybe.withDefault Decimal.zero
                    in
                    ratio
                        |> Decimal.mul tokenSupplyBalance

            _ ->
                Decimal.zero


getCurrentBorrowLimitUsd : CompoundState -> TokenState -> OracleState -> Decimal
getCurrentBorrowLimitUsd compoundState tokenState oracleState =
    let
        cTokens =
            Dict.values tokenState.cTokens

        balanceTotalsUsd =
            Balances.getUnderlyingTotalsInUsd compoundState cTokens oracleState

        accountLiquidityUsd =
            compoundState.maybeAccountLiquidityUsd
                |> Maybe.withDefault Decimal.zero

        --Total Borrow Limit is AccountLiquidity + TotalBorrowBalance
        totalBorrowLimitUsd =
            Decimal.add balanceTotalsUsd.totalBorrow accountLiquidityUsd
    in
    totalBorrowLimitUsd
