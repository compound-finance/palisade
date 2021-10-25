module DappInterface.OverviewHeader exposing (ParentMsg(..), view)

import Balances
import Bootstrap.Progress as Progress
import Charty.PieChart as PieChart exposing (view)
import CompoundComponents.DisplayCurrency as DisplayCurrency
import CompoundComponents.Eth.Ethereum exposing (getContractAddressString)
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (class, id, style)
import CompoundComponents.Utils.DigitAnimatorHelper exposing (valueFormattedStringToDigits)
import CompoundComponents.Utils.NumberFormatter exposing (formatPercentageToNearestWhole, formatPercentageWithDots)
import DappInterface.CollateralPane as CollateralPane
import DappInterface.MainModel
    exposing
        ( BorrowingRisk(..)
        , Model
        , MouseEvent
        , getBorrowingRisk
        , mouseEventDecoder
        )
import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Eth.Config exposing (Config)
import Eth.Oracle
import Html exposing (Html, div, label, p, section, span, text)
import Html.Attributes
import Html.Events
import Json.Decode
import Strings.Translations as Translations
import Utils.CompAPYHelper


type ParentMsg
    = BorrowingLimitMouseEnter MouseEvent
    | BorrowingLimitMouseLeave MouseEvent
    | NetAPYMouseEnter MouseEvent
    | NetAPYMouseLeave MouseEvent


view : Maybe Config -> Maybe Decimal -> Model -> Html ParentMsg
view maybeConfig maybeEtherUsdPrice ({ borrowingContainerState, userLanguage } as mainModel) =
    let
        cTokens =
            Dict.values mainModel.tokenState.cTokens

        balanceTotalsUsd =
            Balances.getUnderlyingTotalsInUsd mainModel.compoundState cTokens mainModel.oracleState

        accountLiquidityUsd =
            mainModel.compoundState.maybeAccountLiquidityUsd
                |> Maybe.withDefault Decimal.zero

        --Total Borrow Limit is AccountLiquidity + TotalBorrowBalance
        totalBorrowLimitUsd =
            Decimal.add balanceTotalsUsd.totalBorrow accountLiquidityUsd

        ( borrowPercentOfLimit, borrowPercentAsWhole ) =
            let
                borrowPercentage =
                    case Decimal.fastdiv balanceTotalsUsd.totalBorrow totalBorrowLimitUsd of
                        Just percent ->
                            percent

                        Nothing ->
                            Decimal.zero
            in
            ( borrowPercentage
                |> Decimal.mul (Decimal.fromInt 100)
            , borrowPercentage
            )

        barClass =
            case getBorrowingRisk borrowPercentOfLimit of
                HighRisk ->
                    "red"

                MediumRisk ->
                    "yellow"

                LowRisk ->
                    "green"

                NoRisk ->
                    "green"

        formatCurrencyExtraDecimalsFunc =
            DisplayCurrency.formatDisplayCurrencyManyDecimals mainModel.preferences.displayCurrency maybeEtherUsdPrice
                >> Maybe.withDefault "―"

        formatCurrencyShortFunc =
            DisplayCurrency.formatDisplayCurrencyInNumberSpec mainModel.preferences.displayCurrency maybeEtherUsdPrice

        ( oldSupplyBalanceString, oldBorrowBalanceString ) =
            case borrowingContainerState.maybePreviousAnimatedBalances of
                Nothing ->
                    ( Nothing, Nothing )

                Just ( oldSupplyBalanceUsd, oldBorrowBalanceUsd ) ->
                    ( formatCurrencyExtraDecimalsFunc (DisplayCurrency.UsdValue oldSupplyBalanceUsd)
                        |> Just
                    , formatCurrencyExtraDecimalsFunc (DisplayCurrency.UsdValue oldBorrowBalanceUsd)
                        |> Just
                    )

        ( currentSupplyBalanceString, currentBorrowBalanceString ) =
            case borrowingContainerState.maybeCurrentAnimatedBalances of
                Nothing ->
                    ( "―", "―" )

                Just ( currentSupplyBalanceUsd, currentBorrowBalanceUsd ) ->
                    ( formatCurrencyExtraDecimalsFunc (DisplayCurrency.UsdValue currentSupplyBalanceUsd)
                    , formatCurrencyExtraDecimalsFunc (DisplayCurrency.UsdValue currentBorrowBalanceUsd)
                    )

        ( supplyTotal, borrowTotal ) =
            if maybeConfig /= Nothing && CollateralPane.areAllAssetsLoaded mainModel then
                ( div [ class "headline" ]
                    (valueFormattedStringToDigits oldSupplyBalanceString currentSupplyBalanceString mainModel.preferences.displayCurrency borrowingContainerState.shouldRemoveSlotMachineActiveClass)
                , div [ class "headline" ]
                    (valueFormattedStringToDigits oldBorrowBalanceString currentBorrowBalanceString mainModel.preferences.displayCurrency borrowingContainerState.shouldRemoveSlotMachineActiveClass)
                )

            else
                ( div [ class "headline headline--loading" ] []
                , div [ class "headline headline--loading" ] []
                )
    in
    section [ id "borrow-overview", class "hero" ]
        [ div [ class "balance-totals" ]
            [ div [ class "content" ]
                [ div [ class "row align-middle mobile-hide" ]
                    [ div [ class "col-xs-5 text-center" ]
                        [ label [ class "supply" ] [ text (Translations.supply_balance mainModel.userLanguage) ]
                        , supplyTotal
                        ]
                    , div [ class "col-xs-2" ]
                        [ netAPYView maybeConfig maybeEtherUsdPrice mainModel
                        ]
                    , div [ class "col-xs-5 text-center" ]
                        [ label [ class "borrow" ] [ text (Translations.borrow_balance mainModel.userLanguage) ]
                        , borrowTotal
                        ]
                    ]
                , div [ class "row align-middle mobile-only" ]
                    [ div [ class "col-xs-4" ]
                        [ netAPYView maybeConfig maybeEtherUsdPrice mainModel
                        ]
                    , div [ class "col-xs-8 text-left" ]
                        [ div [ class "balance" ]
                            [ label [ class "supply" ] [ text (Translations.collateral_balance mainModel.userLanguage) ]
                            , supplyTotal
                            , label [ class "borrow" ] [ text (Translations.borrow_balance mainModel.userLanguage) ]
                            , borrowTotal
                            ]
                        ]
                    ]
                ]
            , div [ class "limit-bar align-between" ]
                [ label [] [ text (Translations.borrow_limit mainModel.userLanguage) ]
                , div [ class "progress-bar-wrapper" ]
                    [ Progress.progress
                        [ Progress.wrapperAttrs
                            [ Html.Attributes.class "dark-clear thin"
                            ]
                        , Progress.attrs
                            [ Html.Attributes.class barClass
                            ]
                        , Progress.value (Decimal.toFloat borrowPercentOfLimit)
                        ]
                    , div [ class "progress-bar-percent-label" ]
                        [ div [ style "width" (formatPercentageToNearestWhole borrowPercentAsWhole) ]
                            [ p [ class "small" ] [ text (formatPercentageToNearestWhole borrowPercentAsWhole) ] ]
                        ]
                    , div
                        [ class "progress-bar-hover-space"
                        , Html.Events.on "mouseenter" (Json.Decode.map BorrowingLimitMouseEnter mouseEventDecoder)
                        , Html.Events.on "mouseleave" (Json.Decode.map BorrowingLimitMouseLeave mouseEventDecoder)
                        ]
                        []
                    ]
                , label [] [ text (formatCurrencyShortFunc (DisplayCurrency.UsdValue totalBorrowLimitUsd)) ]
                ]
            ]
        ]


buildDataset : Float -> PieChart.Dataset
buildDataset borrowPercentage =
    [ { value = borrowPercentage }
    , { value = 100.0 - borrowPercentage }
    ]


netAPYView : Maybe Config -> Maybe Decimal -> Model -> Html ParentMsg
netAPYView maybeConfig maybeEtherUsdPrice ({ userLanguage } as mainModel) =
    let
        cTokens =
            Dict.values mainModel.tokenState.cTokens

        balanceTotalsUsd =
            Balances.getUnderlyingTotalsInUsd mainModel.compoundState cTokens mainModel.oracleState

        ( balanceWithRatesSum, supplyWithRatesSum, borrowWithRatesSum ) =
            cTokens
                |> List.map
                    (\cToken ->
                        let
                            ( supplyBalance, borrowBalance ) =
                                Balances.getUnderlyingBalances mainModel.compoundState cToken.contractAddress
                                    |> Maybe.map
                                        (\balances ->
                                            ( balances.underlyingSupplyBalance
                                            , balances.underlyingBorrowBalance
                                            )
                                        )
                                    |> Maybe.withDefault ( Decimal.zero, Decimal.zero )

                            tokenValueUsd =
                                Eth.Oracle.getOraclePrice mainModel.oracleState cToken.underlying
                                    |> Maybe.withDefault Decimal.zero

                            maybeCompUSDPrice =
                                maybeConfig
                                    |> Maybe.andThen
                                        (\config ->
                                            Eth.Oracle.getCompPriceUSD config mainModel.oracleState
                                        )

                            cTokenAddressString =
                                getContractAddressString cToken.contractAddress

                            maybeCTokenMetadata =
                                mainModel.compoundState.cTokensMetadata
                                    |> Dict.get cTokenAddressString

                            ( compRateForSupply, compRateForBorrow ) =
                                case ( maybeCTokenMetadata, maybeCompUSDPrice ) of
                                    ( Just cTokenMetadata, Just compUSDPrice ) ->
                                        let
                                            totalSupplyUsd =
                                                cTokenMetadata.totalSupplyUnderlying
                                                    |> Decimal.mul tokenValueUsd

                                            totalBorrowUsd =
                                                cTokenMetadata.totalBorrows
                                                    |> Decimal.mul tokenValueUsd

                                            compRate marketTotalUSDValue compSpeedPerDayValue =
                                                Utils.CompAPYHelper.compRate compUSDPrice compSpeedPerDayValue marketTotalUSDValue
                                                    |> Maybe.withDefault Decimal.zero
                                        in
                                        ( compRate totalSupplyUsd cTokenMetadata.compSupplySpeedPerDay, compRate totalBorrowUsd cTokenMetadata.compBorrowSpeedPerDay )

                                    _ ->
                                        ( Decimal.zero, Decimal.zero )

                            ( supplyInterestRate, borrowInterestRate ) =
                                Balances.getInterestRate mainModel.compoundState.cTokensMetadata cToken.contractAddress
                                    |> Maybe.map
                                        (\rates ->
                                            ( rates.supplyRate
                                            , rates.borrowRate
                                            )
                                        )
                                    |> Maybe.withDefault ( Decimal.zero, Decimal.zero )

                            supplyBalMulRateUsd =
                                Decimal.mul supplyBalance tokenValueUsd
                                    |> Decimal.mul (Decimal.add supplyInterestRate compRateForSupply)

                            borrowBalMulRateUsd =
                                Decimal.mul borrowBalance tokenValueUsd
                                    |> Decimal.mul (Decimal.sub borrowInterestRate compRateForBorrow)
                        in
                        ( Decimal.sub supplyBalMulRateUsd borrowBalMulRateUsd
                        , supplyBalMulRateUsd
                        , borrowBalMulRateUsd
                        )
                    )
                |> List.foldl
                    (\( supplyMinusBorrow, supplyOnly, borrowOnly ) ( supplyMinusBorrowSum, supplyOnlySum, borrowOnlySum ) ->
                        ( Decimal.add supplyMinusBorrow supplyMinusBorrowSum
                        , Decimal.add supplyOnly supplyOnlySum
                        , Decimal.add borrowOnly borrowOnlySum
                        )
                    )
                    ( Decimal.zero
                    , Decimal.zero
                    , Decimal.zero
                    )

        earningWeightedAverage =
            Decimal.fastdiv balanceWithRatesSum balanceTotalsUsd.totalSupply

        borrowEffectPercentage =
            Decimal.fastdiv borrowWithRatesSum (Decimal.add supplyWithRatesSum borrowWithRatesSum)
                |> Maybe.withDefault Decimal.zero
                |> Decimal.mul (Decimal.fromInt 100)
                |> Decimal.toFloat

        ( netAPYDescriptionExtraClass, newApyDescriptionLabel, newApyDescription ) =
            if mainModel.borrowingContainerState.showNetAPYHoverView then
                ( " active"
                , formatPercentageWithDots earningWeightedAverage
                , Translations.interest_earned_and_paid userLanguage
                )

            else
                ( ""
                , Translations.net_apy userLanguage
                , formatPercentageWithDots earningWeightedAverage
                )

        apyContent =
            if maybeConfig /= Nothing && CollateralPane.areAllAssetsLoaded mainModel then
                div [ class "headline" ] [ text newApyDescription ]

            else
                div [ class "headline headline--loading" ] []
    in
    div
        [ class "net-apy-wrapper"
        , Html.Events.on "mouseenter" (Json.Decode.map NetAPYMouseEnter mouseEventDecoder)
        , Html.Events.on "mouseleave" (Json.Decode.map NetAPYMouseLeave mouseEventDecoder)
        ]
        [ div [ class "net-apy" ]
            [ PieChart.view PieChart.defaults (buildDataset borrowEffectPercentage) ]
        , div [ class ("net-apy-description" ++ netAPYDescriptionExtraClass) ]
            [ label [] [ text newApyDescriptionLabel ]
            , apyContent
            ]
        ]
