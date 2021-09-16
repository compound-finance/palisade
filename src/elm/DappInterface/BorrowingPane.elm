module DappInterface.BorrowingPane exposing
    ( Msg(..)
    , ParentMsg(..)
    , ReadyBorrowedAsset
    , getReadyBorrowedAssets
    , view
    )

import Balances
import Bootstrap.Progress as Progress
import CompoundComponents.DisplayCurrency as DisplayCurrency
import CompoundComponents.Eth.Ethereum as Ethereum exposing (Account(..), CustomerAddress, getContractAddressString)
import CompoundComponents.Functions as Functions
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (HrefLinkType(..), class, id, onClickStopPropagation)
import CompoundComponents.Utils.NumberFormatter as NumberFormatter exposing (formatPercentageToNearestWhole, formatPercentageWithDots)
import DappInterface.MainModel exposing (BorrowingRisk(..), Model)
import Decimal exposing (Decimal)
import Dict
import Eth.Config exposing (Config)
import Eth.Oracle
import Eth.Token exposing (CToken)
import Eth.Transaction
import Html exposing (Html, a, div, h4, label, section, span, text)
import Html.Events exposing (onClick)
import Preferences exposing (PreferencesMsg(..))
import Strings.Translations as Translations
import Utils.CTokenHelper as CTokenHelper


type ParentMsg
    = BorrowedAssetClicked CToken


type Msg
    = ForPreferences PreferencesMsg
    | ForParent ParentMsg


view : Maybe Config -> Maybe Decimal -> Model -> Html Msg
view maybeConfig maybeEtherUsdPrice ({ compoundState, oracleState, tokenState, userLanguage } as model) =
    let
        allCTokensList =
            CTokenHelper.getAllSupportedCTokens compoundState tokenState

        balanceTotalsUsd =
            Balances.getUnderlyingTotalsInUsd compoundState allCTokensList oracleState

        headerTitleText =
            if Decimal.gt balanceTotalsUsd.totalBorrow Decimal.zero then
                Translations.borrowing userLanguage

            else
                Translations.borrow_markets userLanguage
    in
    div [ id "borrowing-pane", class "col-sm-6" ]
        [ div [ class "panel" ]
            [ div [ class "panel__header align-between" ]
                [ h4 []
                    [ text headerTitleText ]
                ]
            , borrowedListOrAllMarketsPanel maybeConfig maybeEtherUsdPrice model
            ]
        , pendingTransactionsPanel maybeConfig model
        , allMarketsListPanel maybeConfig maybeEtherUsdPrice model
        ]


loadingBorrowedAssetRow : Html Msg
loadingBorrowedAssetRow =
    a [ class "asset asset--loading" ]
        [ div [ class "col-xs-4 col-sm-4 identity" ]
            [ span [ class "icon icon--loading" ] []
            , div [ class "name name--loading" ] []
            ]
        , div [ class "col-xs-0 col-sm-3 text-right mobile-hide" ]
            [ div [ class "balance balance--loading" ] []
            ]
        , div [ class "col-xs-4 col-sm-3 text-right" ]
            [ div [ class "balance balance--loading" ] []
            ]
        , div [ class "col-xs-4 col-sm-2 text-right" ]
            [ div [ class "progress-holder progress-holder--loading" ] []
            ]
        ]


borrowedListOrAllMarketsPanel : Maybe Config -> Maybe Decimal -> Model -> Html Msg
borrowedListOrAllMarketsPanel maybeConfig maybeEtherUsdPrice ({ account, userLanguage } as model) =
    let
        columnLabels =
            div [ class "panel__labels" ]
                [ div [ class "col-xs-4 col-sm-4" ] [ label [] [ text (Translations.asset userLanguage) ] ]
                , div [ class "col-xs-0 col-sm-3 text-right mobile-hide" ] [ label [] [ text (Translations.apy_slash_accrued userLanguage) ] ]
                , div [ class "col-xs-4 col-sm-3 text-right" ] [ label [] [ text (Translations.balance userLanguage) ] ]
                , div [ class "col-xs-4 col-sm-2 text-right" ] [ label [] [ text (Translations.percent_of_limit userLanguage) ] ]
                ]

        borrowedAssets =
            getReadyBorrowedAssets model True

        allReadyBorrowableAssets =
            getReadyBorrowedAssetsForNoAccount model
    in
    if areAllAssetsLoaded model then
        case ( maybeConfig, account ) of
            ( Just config, Acct customerAddress maybeEtherBalance ) ->
                if List.length borrowedAssets > 0 then
                    section [ class "asset-list" ]
                        [ columnLabels
                        , div [ class "assets" ]
                            (borrowedAssets
                                |> List.map (borrowedAssetRow False config ( Just customerAddress, maybeEtherBalance ) maybeEtherUsdPrice model)
                            )
                        ]

                else
                    getAllMarketsPanelContent config maybeEtherUsdPrice False allReadyBorrowableAssets model

            ( Just config, NoAccount ) ->
                getAllMarketsPanelContent config maybeEtherUsdPrice False allReadyBorrowableAssets model

            _ ->
                text ""

    else
        section [ class "asset-list" ]
            [ columnLabels
            , div [ class "assets" ]
                (List.repeat 5 loadingBorrowedAssetRow)
            ]


pendingTransactionsPanel : Maybe Config -> Model -> Html Msg
pendingTransactionsPanel _ { currentTime, currentTimeZone, network, account, tokenState, transactionState, bnTransactionState, userLanguage } =
    let
        -- Transactions that affect borrowing are Borrow, Repay & Repay Behalf.
        pendingBorrowingAssetTransactions =
            transactionState.transactions
                |> Eth.Transaction.getPendingTransactionsForAccount network account (Eth.Transaction.getDefaultOldestPendingTrxTime currentTime) (Just bnTransactionState)
                |> List.filter
                    (\transaction ->
                        (transaction.function == "borrow")
                            || (transaction.function == "repayBorrow")
                            || (transaction.function == "repayBehalf")
                    )
    in
    if List.length pendingBorrowingAssetTransactions > 0 then
        div [ class "panel" ]
            [ div [ class "panel__header" ]
                [ label []
                    [ text (Translations.pending_transactions userLanguage) ]
                ]
            , div [ class "transactions" ]
                [ div [ class "" ] (List.map (Eth.Transaction.transactionEl userLanguage currentTimeZone network tokenState.cTokens) pendingBorrowingAssetTransactions)
                ]
            ]

    else
        text ""


allMarketsListPanel : Maybe Config -> Maybe Decimal -> Model -> Html Msg
allMarketsListPanel maybeConfig maybeEtherUsdPrice ({ account, compoundState, preferences, userLanguage } as model) =
    let
        allBorrowableAssets =
            getReadyBorrowedAssets model False
                |> List.filterMap
                    (\readyAsset ->
                        let
                            supplyBalance =
                                Balances.getUnderlyingBalances compoundState readyAsset.cToken.contractAddress
                                    |> Maybe.map .underlyingSupplyBalance
                                    |> Maybe.withDefault Decimal.zero
                        in
                        if Decimal.gt readyAsset.borrowBalance Decimal.zero then
                            Nothing

                        else if Decimal.gt supplyBalance Decimal.zero then
                            Nothing

                        else
                            Just readyAsset
                    )

        borrowedAndReadyAssets =
            getReadyBorrowedAssets model True
    in
    case ( maybeConfig, account ) of
        ( Just config, Acct _ _ ) ->
            if List.length borrowedAndReadyAssets == 0 then
                text ""

            else if List.length allBorrowableAssets > 0 then
                let
                    ( dropdownActiveClass, panelContent ) =
                        if preferences.borrowPaneOpen then
                            ( " active"
                            , getAllMarketsPanelContent config maybeEtherUsdPrice True allBorrowableAssets model
                            )

                        else
                            ( ""
                            , text ""
                            )
                in
                section [ class "asset-list" ]
                    [ span [ class "panel-label row", onClickStopPropagation (ForPreferences (Preferences.SetBorrowPaneOpen (not preferences.borrowPaneOpen))) ]
                        [ label [] [ text (Translations.all_markets userLanguage) ]
                        , span [ class ("dropdown-icon" ++ dropdownActiveClass) ] []
                        ]
                    , panelContent
                    ]

            else
                text ""

        _ ->
            text ""


getAllMarketsPanelContent : Config -> Maybe Decimal -> Bool -> List ReadyBorrowedAsset -> Model -> Html Msg
getAllMarketsPanelContent config maybeEtherUsdPrice addPanelClass allBorrowableAssets ({ account, userLanguage } as model) =
    let
        columnLabels =
            div [ class "panel__labels" ]
                [ div [ class "col-xs-4 col-sm-4" ] [ label [] [ text (Translations.asset userLanguage) ] ]
                , div [ class "col-xs-0 col-sm-3 text-right mobile-hide" ] [ label [] [ text (Translations.apy userLanguage) ] ]
                , div [ class "col-xs-4 col-sm-3 text-right" ] [ label [] [ text (Translations.wallet userLanguage) ] ]
                , div [ class "col-xs-4 col-sm-2 text-right" ] [ label [] [ text (Translations.liquidity userLanguage) ] ]
                ]

        parentDivAttributes =
            if addPanelClass then
                [ class "panel hideable" ]

            else
                [ class "asset-list" ]
    in
    case account of
        Acct customerAddress maybeEtherBalance ->
            div parentDivAttributes
                [ columnLabels
                , div [ class "assets" ]
                    (allBorrowableAssets
                        |> List.map (borrowedAssetRow True config ( Just customerAddress, maybeEtherBalance ) maybeEtherUsdPrice model)
                    )
                ]

        NoAccount ->
            div parentDivAttributes
                [ columnLabels
                , div [ class "assets" ]
                    (allBorrowableAssets
                        |> List.map (borrowedAssetRow True config ( Nothing, Nothing ) maybeEtherUsdPrice model)
                    )
                ]

        _ ->
            text ""


borrowedAssetRow : Bool -> Config -> ( Maybe CustomerAddress, Maybe Decimal ) -> Maybe Decimal -> Model -> ReadyBorrowedAsset -> Html Msg
borrowedAssetRow isAllMarketsRow config ( maybeCustomerAddress, maybeEtherBalance ) maybeEtherUsdPrice { compoundState, tokenState, oracleState, preferences, userLanguage } { cToken, borrowBalance, tokenValueUsd, maybeBorrowInterestAccrued, borrowInterestRate, underlyingCash } =
    let
        allCTokensList =
            CTokenHelper.getAllSupportedCTokens compoundState tokenState

        balanceTotalsUsd =
            Balances.getUnderlyingTotalsInUsd compoundState allCTokensList oracleState

        supplyLiquidity =
            Decimal.mul underlyingCash tokenValueUsd

        maybeCTokenMetadata =
            Dict.get (getContractAddressString cToken.contractAddress) compoundState.cTokensMetadata

        liquidityUsd =
            case maybeCTokenMetadata of
                Just { totalBorrows, borrowCap } ->
                    if Decimal.eq borrowCap Decimal.zero || Decimal.gt borrowCap underlyingCash then
                        supplyLiquidity

                    else if Decimal.gt borrowCap totalBorrows then
                        Decimal.mul (Decimal.sub borrowCap totalBorrows) tokenValueUsd

                    else
                        Decimal.zero

                _ ->
                    supplyLiquidity

        walletBalance =
            maybeCustomerAddress
                |> Maybe.map
                    (\customerAddress ->
                        Balances.getWalletBalanceNonSafeEther config (Acct customerAddress maybeEtherBalance) compoundState cToken
                    )
                |> Functions.demaybeify
                |> Maybe.withDefault Decimal.zero

        accountLiquidityUsd =
            compoundState.maybeAccountLiquidityUsd
                |> Maybe.withDefault Decimal.zero

        totalBorrowLimitUsd =
            Decimal.add balanceTotalsUsd.totalBorrow accountLiquidityUsd

        borrowBalanceUsd =
            Decimal.mul borrowBalance tokenValueUsd

        balanceAsPercentOfLimit =
            Decimal.fastdiv borrowBalanceUsd totalBorrowLimitUsd
                |> Maybe.withDefault Decimal.zero

        tokenFormatter tokenBalance =
            NumberFormatter.formatTokenBalanceInNumberSpecWithSymbol tokenBalance cToken.underlying.symbol

        formatCurrencyFunc =
            DisplayCurrency.formatDisplayCurrencyInNumberSpec preferences.displayCurrency maybeEtherUsdPrice

        formatMarketSizeFunc =
            DisplayCurrency.formatMarketSize preferences.displayCurrency maybeEtherUsdPrice
                >> Maybe.withDefault "―"

        borrowInterestAccruedSubtitle =
            case maybeBorrowInterestAccrued of
                Just borrowInterestAccrued ->
                    borrowInterestAccrued
                        |> tokenFormatter

                Nothing ->
                    if Decimal.eq borrowBalanceUsd Decimal.zero then
                        "–"

                    else
                        Translations.tbd userLanguage

        ( balanceColumnContents, percentOfBorrowColumnContents ) =
            if isAllMarketsRow then
                ( div [ class "balance" ]
                    [ div []
                        [ text (tokenFormatter walletBalance) ]
                    ]
                , div [ class "balance" ]
                    [ div []
                        [ text (formatMarketSizeFunc (DisplayCurrency.UsdValue liquidityUsd)) ]
                    ]
                )

            else
                ( div [ class "balance" ]
                    [ div []
                        [ text (formatCurrencyFunc (DisplayCurrency.UsdValue borrowBalanceUsd)) ]
                    , span [ class "subtitle" ]
                        [ text (tokenFormatter borrowBalance) ]
                    ]
                , div [ class "progress-holder" ]
                    [ Progress.progress
                        [ Progress.value (Decimal.toFloat <| Decimal.mul (Decimal.fromInt 100) balanceAsPercentOfLimit) ]
                    , span []
                        [ text (formatPercentageToNearestWhole balanceAsPercentOfLimit) ]
                    ]
                )
    in
    a [ class "asset", onClick (ForParent <| BorrowedAssetClicked cToken) ]
        [ div [ class "col-xs-4 col-sm-4 identity" ]
            [ span [ class ("icon icon--" ++ cToken.underlying.symbol) ] []
            , div [ class "balance" ]
                [ div []
                    [ text cToken.underlying.name ]
                , span [ class "subtitle mobile-only" ]
                    [ text (formatPercentageWithDots (Just borrowInterestRate)) ]
                ]
            ]
        , div [ class "col-xs-0 col-sm-3 text-right mobile-hide" ]
            [ div [ class "balance" ]
                (div []
                    [ text (formatPercentageWithDots (Just borrowInterestRate)) ]
                    :: (if not isAllMarketsRow then
                            [ span [ class "subtitle" ]
                                [ text borrowInterestAccruedSubtitle ]
                            ]

                        else
                            [ text "" ]
                       )
                )
            ]
        , div [ class "col-xs-4 col-sm-3 text-right" ]
            [ balanceColumnContents
            ]
        , div [ class "col-xs-4 col-sm-2 text-right" ]
            [ percentOfBorrowColumnContents
            ]
        ]



-- HELPERS


type alias ReadyBorrowedAsset =
    { cToken : CToken
    , borrowBalance : Decimal
    , tokenValueUsd : Decimal
    , maybeBorrowInterestAccrued : Maybe Decimal
    , borrowInterestRate : Decimal
    , underlyingCash : Decimal
    }


getReadyBorrowedAssets : Model -> Bool -> List ReadyBorrowedAsset
getReadyBorrowedAssets { compoundState, oracleState, tokenState } applyNonZeroBalanceFilter =
    let
        allCTokensList =
            CTokenHelper.getAllSupportedCTokens compoundState tokenState

        borrowedCTokensList =
            if applyNonZeroBalanceFilter then
                allCTokensList
                    |> List.filterMap
                        (\cToken ->
                            let
                                borrowBalance =
                                    Balances.getUnderlyingBalances compoundState cToken.contractAddress
                                        |> Maybe.map .underlyingBorrowBalance
                                        |> Maybe.withDefault Decimal.zero
                            in
                            if Decimal.gt borrowBalance Decimal.zero then
                                Just cToken

                            else
                                Nothing
                        )

            else
                allCTokensList
    in
    borrowedCTokensList
        |> List.filterMap
            (\cToken_ ->
                let
                    underlyingBalances =
                        Balances.getUnderlyingBalances compoundState cToken_.contractAddress

                    maybeBorrowBalance =
                        underlyingBalances
                            |> Maybe.map .underlyingBorrowBalance

                    maybeTokenValueUsd =
                        Eth.Oracle.getOraclePrice oracleState cToken_.underlying

                    underlyingInterestBalances =
                        Balances.getUnderlyingInterestBalances compoundState cToken_.contractAddress

                    maybeBorrowInterestAccrued_ =
                        underlyingInterestBalances
                            |> Maybe.map .underlyingBorrowInterestPaid
                            |> Functions.demaybeify

                    maybeBorrowInterestRate =
                        Balances.getInterestRate compoundState.cTokensMetadata cToken_.contractAddress
                            |> Maybe.map .borrowRate

                    maybeUnderlyingCash =
                        compoundState.cTokensMetadata
                            |> Dict.get (Ethereum.getContractAddressString cToken_.contractAddress)
                            |> Maybe.map .totalUnderlyingCash
                in
                Functions.map4
                    maybeBorrowBalance
                    maybeTokenValueUsd
                    maybeBorrowInterestRate
                    maybeUnderlyingCash
                    (\borrowBalance_ tokenValueUsd_ borrowInterestRate_ underlyingCash_ ->
                        { cToken = cToken_
                        , borrowBalance = borrowBalance_
                        , tokenValueUsd = tokenValueUsd_
                        , maybeBorrowInterestAccrued = maybeBorrowInterestAccrued_
                        , borrowInterestRate = borrowInterestRate_
                        , underlyingCash = underlyingCash_
                        }
                    )
            )


getReadyBorrowedAssetsForNoAccount : Model -> List ReadyBorrowedAsset
getReadyBorrowedAssetsForNoAccount { compoundState, oracleState, tokenState } =
    let
        allCTokensList =
            CTokenHelper.getAllSupportedCTokens compoundState tokenState
    in
    allCTokensList
        |> List.filterMap
            (\cToken_ ->
                let
                    maybeTokenValueUsd =
                        Eth.Oracle.getOraclePrice oracleState cToken_.underlying

                    maybeBorrowInterestRate =
                        Balances.getInterestRate compoundState.cTokensMetadata cToken_.contractAddress
                            |> Maybe.map .borrowRate

                    maybeUnderlyingCash =
                        compoundState.cTokensMetadata
                            |> Dict.get (Ethereum.getContractAddressString cToken_.contractAddress)
                            |> Maybe.map .totalUnderlyingCash
                in
                Functions.map3
                    maybeTokenValueUsd
                    maybeBorrowInterestRate
                    maybeUnderlyingCash
                    (\tokenValueUsd_ borrowInterestRate_ underlyingCash_ ->
                        { cToken = cToken_
                        , borrowBalance = Decimal.zero
                        , tokenValueUsd = tokenValueUsd_
                        , maybeBorrowInterestAccrued = Nothing
                        , borrowInterestRate = borrowInterestRate_
                        , underlyingCash = underlyingCash_
                        }
                    )
            )


areAllAssetsLoaded : Model -> Bool
areAllAssetsLoaded ({ account, compoundState, tokenState } as model) =
    let
        allCTokensList =
            CTokenHelper.getAllSupportedCTokens compoundState tokenState

        readyBorrowedAssets =
            case account of
                NoAccount ->
                    getReadyBorrowedAssetsForNoAccount model

                _ ->
                    getReadyBorrowedAssets model False
    in
    List.length allCTokensList == List.length readyBorrowedAssets
