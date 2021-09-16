module Balances exposing
    ( getAssetsNotYetEntered
    , getCollateralValueInUsd
    , getHasAnyAssetEnabledForBorrowing
    , getInterestRate
    , getUnderlyingBalances
    , getUnderlyingInterestBalances
    , getUnderlyingTotalsInUsd
    , getWalletBalanceNonSafeEther
    , getWalletBalanceSafeEther
    , hasEnteredAsset
    )

import CompoundComponents.Eth.Ethereum as Ethereum exposing (Account(..), AssetAddress(..), ContractAddress(..), CustomerAddress(..), getContractAddressString)
import CompoundComponents.Eth.Ledger exposing (LedgerAccount, ledgerAccountToInt)
import CompoundComponents.Functions as Functions
import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Eth.Compound exposing (CTokenBalances, CTokenBalancesDict, CTokenInterestBalances, CTokenMetadata, CTokenMetadataDict, CompoundState)
import Eth.Config exposing (Config)
import Eth.Oracle exposing (OracleState)
import Eth.Token exposing (CToken, TokenState)


getUnderlyingBalances : CompoundState -> ContractAddress -> Maybe CTokenBalances
getUnderlyingBalances compoundState (Contract cTokenAddress) =
    compoundState.balances
        |> Dict.get cTokenAddress


getUnderlyingInterestBalances : CompoundState -> ContractAddress -> Maybe CTokenInterestBalances
getUnderlyingInterestBalances compoundState (Contract cTokenAddress) =
    compoundState.interestBalances
        |> Dict.get cTokenAddress


type alias UnderlyingBalancesInUsd =
    { underlyingBorrowBalanceUsd : Decimal
    , underlyingBorrowInterestUsd : Decimal
    , underlyingSupplyBalanceUsd : Decimal
    , underlyingSupplyInterestUsd : Decimal
    }


getUnderlyingBalancesInUsd : CompoundState -> CToken -> OracleState -> Maybe UnderlyingBalancesInUsd
getUnderlyingBalancesInUsd compoundState cToken oracleState =
    Functions.map2 (Eth.Oracle.getOraclePrice oracleState cToken.underlying)
        (getUnderlyingBalances compoundState cToken.contractAddress)
        (\priceInUsd underlyingBalances ->
            let
                maybeInterestBalances =
                    compoundState.interestBalances
                        |> Dict.get (Ethereum.getContractAddressString cToken.contractAddress)

                ( actualBorrowInterest, actualSupplyInterest ) =
                    case maybeInterestBalances of
                        Just interestBalances ->
                            ( Maybe.withDefault Decimal.zero interestBalances.underlyingBorrowInterestPaid
                            , Maybe.withDefault Decimal.zero interestBalances.underlyingSupplyInterestEarned
                            )

                        Nothing ->
                            ( Decimal.zero
                            , Decimal.zero
                            )
            in
            { underlyingBorrowBalanceUsd = Decimal.mul priceInUsd underlyingBalances.underlyingBorrowBalance
            , underlyingBorrowInterestUsd = Decimal.mul priceInUsd actualBorrowInterest
            , underlyingSupplyBalanceUsd = Decimal.mul priceInUsd underlyingBalances.underlyingSupplyBalance
            , underlyingSupplyInterestUsd = Decimal.mul priceInUsd actualSupplyInterest
            }
        )


type alias InterestRates =
    { borrowRate : Decimal
    , supplyRate : Decimal
    }


getInterestRate : CTokenMetadataDict -> ContractAddress -> Maybe InterestRates
getInterestRate cTokenMetadataDict (Contract cTokenAddress) =
    cTokenMetadataDict
        |> Dict.get cTokenAddress
        |> Maybe.map (\metaData -> { borrowRate = metaData.borrowRate, supplyRate = metaData.supplyRate })


type alias UnderlyingBalanceTotals =
    { totalBorrow : Decimal
    , totalBorrowInterest : Decimal
    , totalSupply : Decimal
    , totalSupplyInterest : Decimal
    }


getUnderlyingTotalsInUsd : CompoundState -> List CToken -> OracleState -> UnderlyingBalanceTotals
getUnderlyingTotalsInUsd compoundState cTokens oracleState =
    let
        emptyTotals =
            { totalBorrow = Decimal.zero
            , totalBorrowInterest = Decimal.zero
            , totalSupply = Decimal.zero
            , totalSupplyInterest = Decimal.zero
            }
    in
    List.foldl
        (\cToken runningBalanceTotals ->
            let
                maybeUnderlyingBalancesUsd =
                    getUnderlyingBalancesInUsd compoundState cToken oracleState
            in
            case maybeUnderlyingBalancesUsd of
                Just underlyingBalancesUsd ->
                    { totalBorrow = Decimal.add underlyingBalancesUsd.underlyingBorrowBalanceUsd runningBalanceTotals.totalBorrow
                    , totalBorrowInterest = Decimal.add underlyingBalancesUsd.underlyingBorrowInterestUsd runningBalanceTotals.totalBorrowInterest
                    , totalSupply = Decimal.add underlyingBalancesUsd.underlyingSupplyBalanceUsd runningBalanceTotals.totalSupply
                    , totalSupplyInterest = Decimal.add underlyingBalancesUsd.underlyingSupplyInterestUsd runningBalanceTotals.totalSupplyInterest
                    }

                Nothing ->
                    runningBalanceTotals
        )
        emptyTotals
        cTokens


getWalletBalanceNonSafeEther : Config -> Account -> CompoundState -> CToken -> Maybe Decimal
getWalletBalanceNonSafeEther config account compoundState cToken =
    case ( Eth.Token.isCEtherToken config cToken, account ) of
        ( True, Acct customerAddress etherBalance ) ->
            etherBalance

        _ ->
            Dict.get (getContractAddressString cToken.contractAddress) compoundState.balances
                |> Maybe.map .underlyingTokenWalletBalance


getWalletBalanceSafeEther : Config -> Account -> CompoundState -> CToken -> Maybe Decimal
getWalletBalanceSafeEther config account compoundState cToken =
    case ( Eth.Token.isCEtherToken config cToken, account ) of
        ( True, Acct customerAddress etherBalance ) ->
            case ( Decimal.fromFloat 0.005, etherBalance ) of
                ( Just etherMaxAdjustment, Just actualEtherBalance ) ->
                    Decimal.sub actualEtherBalance etherMaxAdjustment
                        |> Functions.decimalMax Decimal.zero
                        |> Just

                _ ->
                    etherBalance

        _ ->
            Dict.get (getContractAddressString cToken.contractAddress) compoundState.balances
                |> Maybe.map .underlyingTokenWalletBalance


getCollateralValueInUsd : Eth.Compound.CompoundState -> List CToken -> Eth.Oracle.OracleState -> Decimal
getCollateralValueInUsd compoundState cTokens oracleState =
    let
        balanceTotalsUsd =
            getUnderlyingTotalsInUsd compoundState cTokens oracleState

        accountLiquidityUsd =
            compoundState.maybeAccountLiquidityUsd
                |> Maybe.withDefault Decimal.zero

        accountShortfallUsd =
            compoundState.maybeAccountShortfallUsd
                |> Maybe.withDefault Decimal.zero

        -- Actual account liquidity is the summation of both accountLiquidity and accountShortfall
        -- as only one of the them can be non-zero at a time.
        actualAccountLiquidityUsd =
            Decimal.add accountLiquidityUsd (Decimal.mul Decimal.minusOne accountShortfallUsd)
    in
    Decimal.add actualAccountLiquidityUsd balanceTotalsUsd.totalBorrow


hasEnteredAsset : Config -> CompoundState -> CToken -> Bool
hasEnteredAsset config compoundState cToken =
    getAssetsNotYetEntered config compoundState
        |> Maybe.withDefault []
        |> List.member cToken.contractAddress
        |> not


getAssetsNotYetEntered : Config -> CompoundState -> Maybe (List ContractAddress)
getAssetsNotYetEntered config compoundState =
    case compoundState.maybeAssetsIn of
        Just assetsIn ->
            let
                cTokensAddressList =
                    Dict.values config.cTokens
                        |> List.map .address

                assetsToAdd =
                    cTokensAddressList
                        |> List.filterMap
                            (\cTokenAddress ->
                                if List.member cTokenAddress assetsIn then
                                    Nothing

                                else
                                    Just cTokenAddress
                            )
            in
            Just assetsToAdd

        _ ->
            Nothing


getHasAnyAssetEnabledForBorrowing : Config -> CompoundState -> Maybe Bool
getHasAnyAssetEnabledForBorrowing config compoundState =
    case compoundState.maybeAssetsIn of
        Just assetsIn ->
            not (List.isEmpty assetsIn)
                |> Just

        _ ->
            Nothing
