module Eth.TrxDescriptions exposing
    ( describeGracefulFailure
    , describeTransaction
    )

import CompoundComponents.Eth.Ethereum exposing (ContractAddress(..))
import CompoundComponents.Eth.TokenMath as TokenMath
import CompoundComponents.Functions exposing (default)
import CompoundComponents.Utils.NumberFormatter exposing (formatTokenBalance)
import Decimal exposing (Decimal)
import Eth.Token exposing (CTokenSet, getUnderlyingTokenDecimals, getUnderlyingTokenSymbol)
import Strings.Translations as Translations


describeGracefulFailure : String -> String
describeGracefulFailure error =
    --TODO: This should differentiate between comptroller errors and ctoken errors
    case error of
        "1" ->
            "Account unathorized"

        "2" ->
            "Input error - check your math"

        "3" ->
            "Protocol declined transaction"

        "4" ->
            "Protocol - temporary error"

        "5" ->
            "Protocol - temporary error"

        "6" ->
            "Account unathorized"

        "7" ->
            "Input error - check your math"

        "8" ->
            "Input error - check your math"

        "9" ->
            "Protocol - temporary error"

        "10" ->
            "Asset not supported"

        "11" ->
            "Asset not supported"

        "12" ->
            "Input error - token allowance insufficient"

        "13" ->
            "Input error - token balance insufficient"

        "14" ->
            "Input error - protocol has insufficient liquidity"

        "15" ->
            "Protocol unable to accept token transfer"

        "16" ->
            "Protocol unable to transfer token out"

        _ ->
            "Failed with error code: " ++ error


describeTransaction : Translations.Lang -> CTokenSet -> ContractAddress -> String -> List String -> Maybe String
describeTransaction userLanguage cTokens contract function args =
    case contract of
        Contract contractAddress ->
            case function of
                "allocate" ->
                    case args of
                        [ cTokenAddress, amountWei ] ->
                            let
                                ( tokenSymbol, amount ) =
                                    tokenDetails cTokens cTokenAddress amountWei
                            in
                            Just (Translations.faucet userLanguage ++ " " ++ amount ++ " " ++ tokenSymbol)

                        _ ->
                            Just (Translations.faucet_tokens userLanguage)

                "approve" ->
                    case args of
                        [ cTokenAddress ] ->
                            let
                                ( tokenSymbol, amount ) =
                                    tokenDetails cTokens cTokenAddress "0"
                            in
                            Just (Translations.enable userLanguage ++ " " ++ tokenSymbol)

                        _ ->
                            Just (Translations.enable userLanguage)

                "mint" ->
                    case args of
                        [ amountWei ] ->
                            let
                                ( tokenSymbol, amount ) =
                                    tokenDetails cTokens contractAddress amountWei
                            in
                            Just (Translations.supply userLanguage ++ " " ++ amount ++ " " ++ tokenSymbol)

                        _ ->
                            Just (Translations.supply_tokens userLanguage)

                "redeem" ->
                    case args of
                        [ amountWei ] ->
                            let
                                ( tokenSymbol, amount ) =
                                    tokenDetails cTokens contractAddress amountWei
                            in
                            Just (Translations.withdraw_max userLanguage ++ " " ++ tokenSymbol)

                        _ ->
                            Just (Translations.withdraw_tokens userLanguage)

                "redeemUnderlying" ->
                    case args of
                        [ amountWei ] ->
                            let
                                ( tokenSymbol, amount ) =
                                    tokenDetails cTokens contractAddress amountWei
                            in
                            Just (Translations.withdraw userLanguage ++ " " ++ amount ++ " " ++ tokenSymbol)

                        _ ->
                            Just (Translations.withdraw_tokens userLanguage)

                "borrow" ->
                    case args of
                        [ amountWei ] ->
                            let
                                ( tokenSymbol, amount ) =
                                    tokenDetails cTokens contractAddress amountWei
                            in
                            Just (Translations.borrow userLanguage ++ " " ++ amount ++ " " ++ tokenSymbol)

                        _ ->
                            Just (Translations.borrow_tokens userLanguage)

                "repayBorrow" ->
                    case args of
                        [ amountWei ] ->
                            let
                                ( tokenSymbol, amount ) =
                                    tokenDetails cTokens contractAddress amountWei
                            in
                            Just (Translations.repay userLanguage ++ " " ++ amount ++ " " ++ tokenSymbol)

                        _ ->
                            Just (Translations.pay_back_borrowed_tokens userLanguage)

                "repayBehalf" ->
                    case args of
                        [ cTokenAddress, amountWei ] ->
                            let
                                ( tokenSymbol, amount ) =
                                    tokenDetails cTokens cTokenAddress amountWei
                            in
                            Just (Translations.repay userLanguage ++ " " ++ amount ++ " " ++ tokenSymbol)

                        _ ->
                            Just (Translations.pay_back_borrowed_tokens userLanguage)

                "liquidateBorrow" ->
                    case args of
                        [ cTokenBorrowAddress, amountToCloseWei, cTokenCollateralAddress, amountSeizedArg ] ->
                            let
                                ( borrowTokenSymbol, borrowAmountString ) =
                                    tokenDetails cTokens cTokenBorrowAddress amountToCloseWei

                                ( collateralTokenSymbol, _ ) =
                                    tokenDetails cTokens cTokenCollateralAddress "0"
                            in
                            Just (Translations.liquidated_for userLanguage (borrowAmountString ++ " " ++ borrowTokenSymbol) collateralTokenSymbol)

                        _ ->
                            Just (Translations.liquidate_target_user_borrow userLanguage)

                "exitMarket" ->
                    Just (Translations.disable_for_borrowing userLanguage)

                "enterMarkets" ->
                    Just (Translations.enable_for_borrowing userLanguage)

                _ ->
                    Just function


tokenDetails : CTokenSet -> String -> String -> ( String, String )
tokenDetails cTokens cTokenAddressString amountWei =
    let
        maybeCToken =
            Eth.Token.getCTokenByAddress cTokens cTokenAddressString
    in
    case maybeCToken of
        Just cToken ->
            ( cToken.underlying.symbol, tokenAmountFromWei amountWei cToken.underlying.decimals )

        Nothing ->
            ( "<Unknown>", tokenAmountFromWei amountWei 1 )


tokenAmountFromWei : String -> Int -> String
tokenAmountFromWei amountWei tokenDecimals =
    case Decimal.fromString amountWei of
        Just amountWeiDec ->
            if Decimal.eq amountWeiDec Decimal.minusOne then
                "Max"

            else
                TokenMath.getTokenAmount amountWeiDec tokenDecimals
                    |> formatTokenBalance

        Nothing ->
            "unknown"
