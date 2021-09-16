module DappInterface.CollateralPane exposing (Msg(..), ParentMsg(..), ReadyCollateralAsset, areAllAssetsLoaded, getReadyCollateralAssets, view)

import Balances
import CompoundComponents.DisplayCurrency as DisplayCurrency
import CompoundComponents.Eth.Ethereum as Ethereum exposing (Account(..), CustomerAddress(..))
import CompoundComponents.Functions as Functions
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (HrefLinkType(..), class, id, onClickStopPropagation, type_)
import CompoundComponents.Utils.Markup
import CompoundComponents.Utils.NumberFormatter as NumberFormatter exposing (formatPercentageWithDots)
import DappInterface.MainModel exposing (Model)
import Decimal exposing (Decimal)
import Dict
import Eth.Compound exposing (CompoundMsg)
import Eth.Config exposing (Config)
import Eth.Oracle
import Eth.Token exposing (CToken)
import Eth.Transaction
import Html exposing (Html, a, div, h4, input, label, section, span, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Preferences exposing (PreferencesMsg(..))
import Strings.Translations as Translations
import Utils.CTokenHelper as CTokenHelper


type ParentMsg
    = CollateralAssetClicked CToken
    | UseAsCollateralToggleClicked CToken


type Msg
    = ForCompoundController CompoundMsg
    | ForPreferences PreferencesMsg
    | ForParent ParentMsg


view : Maybe Config -> Maybe Decimal -> Model -> Html Msg
view maybeConfig maybeEtherUsdPrice ({ compoundState, oracleState, tokenState, userLanguage } as model) =
    let
        allCTokensList =
            CTokenHelper.getAllSupportedCTokens compoundState tokenState

        balanceTotalsUsd =
            Balances.getUnderlyingTotalsInUsd compoundState allCTokensList oracleState

        headerTitleText =
            if Decimal.gt balanceTotalsUsd.totalSupply Decimal.zero then
                Translations.supply userLanguage

            else
                Translations.supply_markets userLanguage
    in
    div [ id "collateral-pane", class "col-sm-6" ]
        [ div [ class "panel" ]
            [ div [ class "panel__header align-between" ]
                [ h4 []
                    [ text headerTitleText ]
                ]
            , collateralListOrAllMarketsPanel maybeConfig maybeEtherUsdPrice model
            ]
        , pendingTransactionsPanel model
        , allMarketsListPanel maybeConfig maybeEtherUsdPrice model
        ]


loadingCollateralAssetRow : Html Msg
loadingCollateralAssetRow =
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


collateralListOrAllMarketsPanel : Maybe Config -> Maybe Decimal -> Model -> Html Msg
collateralListOrAllMarketsPanel maybeConfig maybeEtherUsdPrice ({ account, userLanguage } as model) =
    let
        columnLabels =
            div [ class "panel__labels" ]
                [ div [ class "col-xs-4 col-sm-4" ] [ label [] [ text (Translations.asset userLanguage) ] ]
                , div [ class "col-xs-0 col-sm-3 text-right mobile-hide" ] [ label [] [ text (Translations.apy_slash_earned userLanguage) ] ]
                , div [ class "col-xs-4 col-sm-3 text-right" ] [ label [] [ text (Translations.balance userLanguage) ] ]
                , div [ class "col-xs-4 col-sm-2 text-right" ] [ label [] [ text (Translations.borrow_against userLanguage) ] ]
                ]

        suppliedAndReadyCollateralAssets =
            getReadyCollateralAssets model True

        allReadyCollateralAssets =
            getReadyCollateralAssetsForNoAccount model
    in
    if areAllAssetsLoaded model then
        case ( maybeConfig, account ) of
            ( Just config, Acct customerAddress maybeEtherBalance ) ->
                if List.length suppliedAndReadyCollateralAssets > 0 then
                    section [ class "asset-list" ]
                        [ columnLabels
                        , div [ class "assets" ]
                            (suppliedAndReadyCollateralAssets
                                |> List.map (collateralAssetRow False config ( Just customerAddress, maybeEtherBalance ) maybeEtherUsdPrice model)
                            )
                        ]

                else
                    getAllMarketsPanelContent config maybeEtherUsdPrice False allReadyCollateralAssets model

            ( Just config, NoAccount ) ->
                getAllMarketsPanelContent config maybeEtherUsdPrice False allReadyCollateralAssets model

            _ ->
                text ""

    else
        section [ class "asset-list" ]
            [ columnLabels
            , div [ class "assets" ]
                (List.repeat 5 loadingCollateralAssetRow)
            ]


pendingTransactionsPanel : Model -> Html Msg
pendingTransactionsPanel { currentTime, currentTimeZone, network, account, tokenState, transactionState, bnTransactionState, userLanguage } =
    let
        -- Transactions that affect collateral are Enter, Exit, Supply and Withdraw.
        pendingCollateralAssetTransactions =
            transactionState.transactions
                |> Eth.Transaction.getPendingTransactionsForAccount network account (Eth.Transaction.getDefaultOldestPendingTrxTime currentTime) (Just bnTransactionState)
                |> List.filter
                    (\transaction ->
                        (transaction.function == "enterMarkets")
                            || (transaction.function == "exitMarket")
                            || (transaction.function == "mint")
                            || (transaction.function == "redeem")
                            || (transaction.function == "redeemUnderlying")
                            || (transaction.function == "approve")
                    )
    in
    if List.length pendingCollateralAssetTransactions > 0 then
        div [ class "panel" ]
            [ div [ class "panel__header" ]
                [ label []
                    [ text (Translations.pending_transactions userLanguage) ]
                ]
            , div [ class "transactions" ]
                [ div [ class "" ] (List.map (Eth.Transaction.transactionEl userLanguage currentTimeZone network tokenState.cTokens) pendingCollateralAssetTransactions)
                ]
            ]

    else
        text ""


allMarketsListPanel : Maybe Config -> Maybe Decimal -> Model -> Html Msg
allMarketsListPanel maybeConfig maybeEtherUsdPrice ({ account, compoundState, preferences, userLanguage } as model) =
    let
        allReadyCollateralAssets =
            getReadyCollateralAssets model False
                |> List.filterMap
                    (\readyAsset ->
                        let
                            borrowBalance =
                                Balances.getUnderlyingBalances compoundState readyAsset.cToken.contractAddress
                                    |> Maybe.map .underlyingBorrowBalance
                                    |> Maybe.withDefault Decimal.zero
                        in
                        if Decimal.gt readyAsset.supplyBalance Decimal.zero then
                            Nothing

                        else if Decimal.gt borrowBalance Decimal.zero then
                            Nothing

                        else
                            Just readyAsset
                    )

        suppliedAndReadyCollateralAssets =
            getReadyCollateralAssets model True
    in
    case ( maybeConfig, account ) of
        ( Just config, Acct _ _ ) ->
            if List.length suppliedAndReadyCollateralAssets == 0 then
                text ""

            else if List.length allReadyCollateralAssets > 0 then
                let
                    ( dropdownActiveClass, panelContent ) =
                        if preferences.supplyPaneOpen then
                            ( " active"
                            , getAllMarketsPanelContent config maybeEtherUsdPrice True allReadyCollateralAssets model
                            )

                        else
                            ( ""
                            , text ""
                            )
                in
                section [ class "asset-list" ]
                    [ span [ class "panel-label row", onClickStopPropagation (ForPreferences (Preferences.SetSupplyPaneOpen (not preferences.supplyPaneOpen))) ]
                        [ label [] [ text (Translations.all_markets userLanguage) ]
                        , span [ class ("dropdown-icon" ++ dropdownActiveClass) ] []
                        ]
                    , panelContent
                    ]

            else
                text ""

        _ ->
            text ""


getAllMarketsPanelContent : Config -> Maybe Decimal -> Bool -> List ReadyCollateralAsset -> Model -> Html Msg
getAllMarketsPanelContent config maybeEtherUsdPrice addPanelClass allReadyCollateralAssets ({ account, userLanguage } as model) =
    let
        columnLabels =
            div [ class "panel__labels" ]
                [ div [ class "col-xs-4 col-sm-4" ] [ label [] [ text (Translations.asset userLanguage) ] ]
                , div [ class "col-xs-0 col-sm-3 text-right mobile-hide" ] [ label [] [ text (Translations.apy userLanguage) ] ]
                , div [ class "col-xs-4 col-sm-3 text-right" ] [ label [] [ text (Translations.wallet userLanguage) ] ]
                , div [ class "col-xs-4 col-sm-2 text-right" ] [ label [] [ text (Translations.borrow_against userLanguage) ] ]
                ]

        parentDivAttributes =
            if addPanelClass then
                [ class "panel hideable" ]

            else
                [ class "asset-list" ]
    in
    case account of
        Acct customerAddress maybeEthBalance ->
            div parentDivAttributes
                [ columnLabels
                , div [ class "assets" ]
                    (allReadyCollateralAssets
                        |> List.map (collateralAssetRow True config ( Just customerAddress, maybeEthBalance ) maybeEtherUsdPrice model)
                    )
                ]

        NoAccount ->
            div parentDivAttributes
                [ columnLabels
                , div [ class "assets" ]
                    (allReadyCollateralAssets
                        |> List.map (collateralAssetRow True config ( Nothing, Nothing ) maybeEtherUsdPrice model)
                    )
                ]

        _ ->
            text ""


collateralAssetRow : Bool -> Config -> ( Maybe CustomerAddress, Maybe Decimal ) -> Maybe Decimal -> Model -> ReadyCollateralAsset -> Html Msg
collateralAssetRow isAllMarketsRow config ( maybeCustomerAddress, maybeEtherBalance ) maybeEtherUsdPrice { compoundState, tokenState, preferences, userLanguage } { cToken, supplyBalance, tokenValueUsd, maybeSupplyInterestEarned, supplyInterestRate } =
    let
        supplyBalanceUsd =
            Decimal.mul supplyBalance tokenValueUsd

        walletBalance =
            maybeCustomerAddress
                |> Maybe.map
                    (\customerAddress ->
                        Balances.getWalletBalanceNonSafeEther config (Acct customerAddress maybeEtherBalance) compoundState cToken
                    )
                |> Functions.demaybeify
                |> Maybe.withDefault Decimal.zero

        tokenFormatter tokenBalance =
            NumberFormatter.formatTokenBalanceInNumberSpecWithSymbol tokenBalance cToken.underlying.symbol

        formatCurrencyFunc =
            DisplayCurrency.formatDisplayCurrencyInNumberSpec preferences.displayCurrency maybeEtherUsdPrice

        supplyInterestEarnedSubtitle =
            case maybeSupplyInterestEarned of
                Just supplyInterestEarned ->
                    supplyInterestEarned
                        |> tokenFormatter

                Nothing ->
                    if Decimal.eq supplyBalanceUsd Decimal.zero then
                        "–"

                    else
                        Translations.tbd userLanguage

        ( onClickMsg, switchClass, inputExtraMarkup ) =
            case maybeCustomerAddress of
                Just _ ->
                    if Balances.hasEnteredAsset config compoundState cToken then
                        -- Already in asset, we should queue up exit.
                        ( UseAsCollateralToggleClicked cToken
                            |> ForParent
                        , "mdc-switch mdc-switch--checked"
                        , [ CompoundComponents.Utils.Markup.checked ]
                        )

                    else
                        -- Not in asset so we should queue up enterAsset.
                        ( UseAsCollateralToggleClicked cToken
                            |> ForParent
                        , "mdc-switch"
                        , []
                        )

                Nothing ->
                    ( UseAsCollateralToggleClicked cToken
                        |> ForParent
                    , "mdc-switch disabled"
                    , []
                    )

        assetIconClass =
            if Decimal.gt supplyBalanceUsd Decimal.zero then
                "ctoken ctoken--" ++ cToken.symbol ++ " ctoken--" ++ cToken.symbol

            else
                "icon icon--" ++ cToken.underlying.symbol

        balanceColumnContents =
            if isAllMarketsRow then
                div [ class "balance" ]
                    [ div []
                        [ text (tokenFormatter walletBalance) ]
                    ]

            else
                div [ class "balance" ]
                    [ div []
                        [ text (formatCurrencyFunc (DisplayCurrency.UsdValue supplyBalanceUsd)) ]
                    , span [ class "subtitle" ]
                        [ text (tokenFormatter supplyBalance) ]
                    ]

        collateralFactor =
            compoundState.cTokensMetadata
                |> Dict.get (Ethereum.getContractAddressString cToken.contractAddress)
                |> Maybe.map .collateralFactor
                |> Maybe.withDefault Decimal.zero

        collateralToggleComponent =
            if Decimal.eq collateralFactor Decimal.zero then
                text ""

            else
                div [ class switchClass ]
                    [ div [ class "mdc-switch__track" ] []
                    , div [ class "mdc-switch__thumb-underlay" ]
                        [ div [ class "mdc-switch__thumb" ]
                            [ input
                                ([ id "basic-switch"
                                 , class "mdc-switch__native-control"
                                 , type_ "checkbox"
                                 , Html.Attributes.attribute "role" "checkbox"
                                 , onClickStopPropagation onClickMsg
                                 ]
                                    ++ inputExtraMarkup
                                )
                                []
                            ]
                        ]
                    ]
    in
    a [ class "asset", onClick (ForParent (CollateralAssetClicked cToken)) ]
        [ div [ class "col-xs-4 col-sm-4 identity" ]
            [ span [ class assetIconClass ] []
            , div [ class "balance" ]
                [ div []
                    [ text cToken.underlying.name ]
                , span [ class "subtitle mobile-only" ]
                    [ text (formatPercentageWithDots (Just supplyInterestRate)) ]
                ]
            ]
        , div [ class "col-xs-0 col-sm-3 text-right mobile-hide" ]
            [ div [ class "balance" ]
                (div []
                    [ text (formatPercentageWithDots (Just supplyInterestRate)) ]
                    :: (if not isAllMarketsRow then
                            [ span [ class "subtitle" ]
                                [ text supplyInterestEarnedSubtitle ]
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
            [ collateralToggleComponent
            ]
        ]



-- HELPERS


type alias ReadyCollateralAsset =
    { cToken : CToken
    , supplyBalance : Decimal
    , tokenValueUsd : Decimal
    , maybeSupplyInterestEarned : Maybe Decimal
    , supplyInterestRate : Decimal
    }


getReadyCollateralAssets : Model -> Bool -> List ReadyCollateralAsset
getReadyCollateralAssets { compoundState, oracleState, tokenState } applyNonZeroBalanceFilter =
    let
        allCTokensList =
            CTokenHelper.getAllSupportedCTokens compoundState tokenState

        suppliedCTokensList =
            if applyNonZeroBalanceFilter then
                allCTokensList
                    |> List.filterMap
                        (\cToken ->
                            let
                                supplyBalance =
                                    Balances.getUnderlyingBalances compoundState cToken.contractAddress
                                        |> Maybe.map .underlyingSupplyBalance
                                        |> Maybe.withDefault Decimal.zero
                            in
                            if Decimal.gt supplyBalance Decimal.zero then
                                Just cToken

                            else
                                Nothing
                        )

            else
                allCTokensList
    in
    suppliedCTokensList
        |> List.filterMap
            (\cToken_ ->
                let
                    underlyingBalances =
                        Balances.getUnderlyingBalances compoundState cToken_.contractAddress

                    maybeSupplyBalance =
                        underlyingBalances
                            |> Maybe.map .underlyingSupplyBalance

                    maybeTokenValueUsd =
                        Eth.Oracle.getOraclePrice oracleState cToken_.underlying

                    underlyingInterestBalances =
                        Balances.getUnderlyingInterestBalances compoundState cToken_.contractAddress

                    maybeSupplyInterestEarned_ =
                        underlyingInterestBalances
                            |> Maybe.map .underlyingSupplyInterestEarned
                            |> Functions.demaybeify

                    maybeSupplyInterestRate =
                        Balances.getInterestRate compoundState.cTokensMetadata cToken_.contractAddress
                            |> Maybe.map .supplyRate
                in
                Functions.map3
                    maybeSupplyBalance
                    maybeTokenValueUsd
                    maybeSupplyInterestRate
                    (\supplyBalance_ tokenValueUsd_ supplyInterestRate_ ->
                        { cToken = cToken_
                        , supplyBalance = supplyBalance_
                        , tokenValueUsd = tokenValueUsd_
                        , maybeSupplyInterestEarned = maybeSupplyInterestEarned_
                        , supplyInterestRate = supplyInterestRate_
                        }
                    )
            )


getReadyCollateralAssetsForNoAccount : Model -> List ReadyCollateralAsset
getReadyCollateralAssetsForNoAccount { compoundState, oracleState, tokenState } =
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

                    maybeSupplyInterestRate =
                        Balances.getInterestRate compoundState.cTokensMetadata cToken_.contractAddress
                            |> Maybe.map .supplyRate
                in
                Functions.map2
                    maybeTokenValueUsd
                    maybeSupplyInterestRate
                    (\tokenValueUsd_ supplyInterestRate_ ->
                        { cToken = cToken_
                        , supplyBalance = Decimal.zero
                        , tokenValueUsd = tokenValueUsd_
                        , maybeSupplyInterestEarned = Nothing
                        , supplyInterestRate = supplyInterestRate_
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
                    getReadyCollateralAssetsForNoAccount model

                _ ->
                    getReadyCollateralAssets model False
    in
    List.length allCTokensList == List.length readyBorrowedAssets
