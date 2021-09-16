module DappInterface.CollateralToggleModal exposing
    ( ParentMsg(..)
    , TranslationDictionary
    , Translator
    , handleBNTransactionUpdate
    , handleCompoundUpdate
    , handleNonBNTransactionUpdate
    , prepareToShowModal
    , translator
    , view
    )

import Balances
import Browser.Dom
import CompoundComponents.DisplayCurrency as DisplayCurrency
import CompoundComponents.Eth.ConnectedEthWallet as ConnectedEthWallet
import CompoundComponents.Eth.Ethereum as Ethereum exposing (Account(..), CustomerAddress)
import CompoundComponents.Eth.Network exposing (Network)
import CompoundComponents.Ether.BNTransaction as BNTransaction exposing (BNTransactionMsg)
import CompoundComponents.Functions as Functions
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (HrefLinkType(..), class, href, id, onClickStopPropagation, placeholder, style, target, type_, value)
import CompoundComponents.Utils.Markup exposing (disabled)
import CompoundComponents.Utils.NumberFormatter as NumberFormatter exposing (formatCollateralFactor, formatPercentageToNearestWhole, formatRate)
import DappInterface.MainModel exposing (BorrowingRisk(..), CollateralToggleModalState, Model, getBorrowingRisk, getCurrentConfig)
import Date
import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Eth.Compound exposing (CompoundMsg, CompoundState)
import Eth.Config exposing (Config)
import Eth.Oracle exposing (OracleState)
import Eth.Token exposing (CToken, TokenMsg, TokenState, getCTokenAddress)
import Eth.Transaction exposing (Transaction, TransactionMsg)
import Eth.Validations exposing (hasSufficientBalanceForSupply, hasSufficientCollateralForBorrow)
import Html exposing (Html, a, button, div, h4, input, label, p, span, text)
import Html.Events exposing (onClick, onInput)
import Strings.Translations as Translations
import Task
import Time
import Utils.CTokenHelper as CTokenHelper
import Utils.SafeLiquidity


type ParentMsg
    = DismissAndResetCollateralModal


type Msg
    = ForParent ParentMsg
    | WrappedCompoundMsg CompoundMsg


type alias TranslationDictionary msg =
    { onParentMsg : ParentMsg -> msg
    , onWrappedCompoundMsg : CompoundMsg -> msg
    }


type alias Translator msg =
    Msg -> msg


translator : TranslationDictionary msg -> Translator msg
translator { onParentMsg, onWrappedCompoundMsg } msg =
    case msg of
        ForParent parentMsg ->
            onParentMsg parentMsg

        WrappedCompoundMsg tokenMsg ->
            onWrappedCompoundMsg tokenMsg


prepareToShowModal : Config -> Model -> CToken -> CollateralToggleModalState
prepareToShowModal config mainModel selectedToken =
    let
        isInMarket =
            Balances.hasEnteredAsset config mainModel.compoundState selectedToken

        newCollateralModalState =
            { chosenAsset = selectedToken
            , entering = not isInMarket
            , actionState = DappInterface.MainModel.InitialState
            }
    in
    newCollateralModalState


handleCompoundUpdate : CompoundMsg -> CollateralToggleModalState -> CollateralToggleModalState
handleCompoundUpdate compoundMsg ({ chosenAsset, actionState } as state) =
    let
        updateStateIfMatchingAsset : List Ethereum.ContractAddress -> CollateralToggleModalState
        updateStateIfMatchingAsset targetCTokenAddresses =
            if actionState == DappInterface.MainModel.InitialState then
                let
                    matches =
                        targetCTokenAddresses
                            |> List.filter
                                (\cTokenAddress ->
                                    cTokenAddress == chosenAsset.contractAddress
                                )
                in
                if not (List.isEmpty matches) then
                    { state | actionState = DappInterface.MainModel.AwaitingEnterExitConfirmTransaction }

                else
                    state

            else
                state
    in
    case compoundMsg of
        Eth.Compound.Web3TransactionMsg (Eth.Compound.EnterMarkets _ _ cTokenContractAddressList _) ->
            updateStateIfMatchingAsset cTokenContractAddressList

        Eth.Compound.Web3TransactionMsg (Eth.Compound.ExitMarket _ _ cTokenContractAddress _) ->
            updateStateIfMatchingAsset [ cTokenContractAddress ]

        _ ->
            state


updateStateForTrxUpdates : Config -> CollateralToggleModalState -> Model -> Maybe CollateralToggleModalState
updateStateForTrxUpdates config ({ actionState } as state) mainModel =
    let
        maybePendingCTokenTransaction =
            getMostRecentAssetPendingTransaction config state mainModel
    in
    case maybePendingCTokenTransaction of
        Just _ ->
            if actionState == DappInterface.MainModel.AwaitingEnterExitTransactionMined then
                Nothing

            else
                Just state

        Nothing ->
            Nothing


handleBNTransactionUpdate : Maybe Config -> BNTransactionMsg -> CollateralToggleModalState -> Model -> Maybe CollateralToggleModalState
handleBNTransactionUpdate maybeConfig bnTransactionMsg state mainModel =
    case ( maybeConfig, mainModel.network, bnTransactionMsg ) of
        ( Just config, Just _, BNTransaction.TransactionStateChange _ ) ->
            updateStateForTrxUpdates config state mainModel

        _ ->
            Just state


handleNonBNTransactionUpdate : Maybe Config -> TransactionMsg -> CollateralToggleModalState -> Model -> Maybe CollateralToggleModalState
handleNonBNTransactionUpdate maybeConfig transactionMsg state mainModel =
    case ( maybeConfig, mainModel.network, transactionMsg ) of
        ( Just config, Just _, Eth.Transaction.UpdateTransaction _ ) ->
            updateStateForTrxUpdates config state mainModel

        _ ->
            Just state


view : Model -> Html Msg
view mainModel =
    let
        maybeConfig =
            getCurrentConfig mainModel

        maybeEtherUsdPrice =
            maybeConfig
                |> Maybe.andThen (\config -> Eth.Oracle.getEtherPrice config mainModel.tokenState mainModel.oracleState)
    in
    case ( ( maybeConfig, mainModel.network ), ( mainModel.account, mainModel.collateralToggleModalState ) ) of
        ( ( Just config, Just actualNetwork ), ( Acct customerAddress _, Just visibleCollateralModalState ) ) ->
            div [ class "collateral-toggle modal" ]
                [ div [ class "cover active", onClick (ForParent DismissAndResetCollateralModal) ] []
                , div [ class "container-small" ]
                    [ div [ class "legacy-panel" ]
                        [ div [ class "header" ]
                            [ div [ class "close-x" ]
                                [ button [ onClick (ForParent DismissAndResetCollateralModal) ] []
                                ]
                            ]
                        , mainCollateralView config actualNetwork customerAddress maybeEtherUsdPrice visibleCollateralModalState mainModel
                        ]
                    ]
                ]

        _ ->
            text ""


mainCollateralView : Config -> Network -> CustomerAddress -> Maybe Decimal -> CollateralToggleModalState -> Model -> Html Msg
mainCollateralView config network customerAddress maybeEtherUsdPrice ({ chosenAsset, actionState } as collateralToggleModalState) mainModel =
    let
        ( maybePendingTransaction, chooseInputView ) =
            ( getMostRecentAssetPendingTransaction config collateralToggleModalState mainModel
            , borrowingLimitsView mainModel.userLanguage config network customerAddress maybeEtherUsdPrice collateralToggleModalState mainModel
            )

        -- If we are awaiting a pending transaction or we are awaiting the confirmation of an action,
        -- then we should override the normal view
        contentView =
            case maybePendingTransaction of
                Just pendingTransaction ->
                    awaitingPendingTransactionView mainModel.userLanguage config customerAddress collateralToggleModalState pendingTransaction mainModel

                Nothing ->
                    if actionState == DappInterface.MainModel.AwaitingEnterExitConfirmTransaction then
                        awaitingWeb3ConfirmView mainModel.userLanguage config customerAddress collateralToggleModalState mainModel

                    else
                        chooseInputView
    in
    contentView


getModalDescriptions : Translations.Lang -> Config -> CustomerAddress -> CollateralToggleModalState -> Model -> ( String, Html Msg )
getModalDescriptions userLanguage config customerAddress ({ chosenAsset, entering } as collateralToggleModalState) mainModel =
    let
        cTokens =
            Dict.values mainModel.tokenState.cTokens

        balanceTotalsUsd =
            Balances.getUnderlyingTotalsInUsd mainModel.compoundState cTokens mainModel.oracleState

        ( _, postActionBorrowLimitUsd ) =
            getPreAndPostActionBorrowLimits config customerAddress collateralToggleModalState mainModel

        borrowPercentOfNewLimitActual =
            (case Decimal.fastdiv balanceTotalsUsd.totalBorrow postActionBorrowLimitUsd of
                Just percent ->
                    percent

                Nothing ->
                    Decimal.zero
            )
                |> Decimal.mul (Decimal.fromInt 100)
    in
    if not entering then
        if Decimal.lt borrowPercentOfNewLimitActual Decimal.zero || Decimal.gte borrowPercentOfNewLimitActual (Decimal.fromInt 100) then
            ( Translations.collateral_required userLanguage
            , p [ class "small text-center" ]
                [ text (Translations.collateral_required_description userLanguage)
                ]
            )

        else
            ( Translations.disable_as_collateral userLanguage
            , p [ class "small text-center" ]
                [ text (Translations.disable_as_collateral_description userLanguage)
                , text " "
                , a ([ target "_blank" ] ++ href External "https://medium.com/compound-finance/faq-1a2636713b69") [ text (Translations.collateral_toggle_learn_more userLanguage) ]
                , text "."
                ]
            )

    else
        ( Translations.enable_as_collateral userLanguage
        , p [ class "small text-center" ]
            [ text (Translations.enable_as_collateral_description userLanguage)
            , text " "
            , a ([ target "_blank" ] ++ href External "https://medium.com/compound-finance/faq-1a2636713b69") [ text (Translations.collateral_toggle_learn_more userLanguage) ]
            , text "."
            ]
        )


awaitingWeb3ConfirmView : Translations.Lang -> Config -> CustomerAddress -> CollateralToggleModalState -> Model -> Html Msg
awaitingWeb3ConfirmView userLanguage config customerAddress collateralToggleModalState ({ connectedEthWalletModel } as mainModel) =
    let
        ( headerText, _ ) =
            getModalDescriptions userLanguage config customerAddress collateralToggleModalState mainModel

        confirmTransactionText =
            case connectedEthWalletModel.selectedProvider of
                Just ConnectedEthWallet.Metamask ->
                    Translations.confirm_the_transaction_with userLanguage (Translations.metamask userLanguage)

                Just ConnectedEthWallet.WalletLink ->
                    Translations.confirm_the_transaction_with userLanguage (Translations.coinbase_wallet userLanguage)

                Just ConnectedEthWallet.Ledger ->
                    Translations.confirm_the_transaction_with userLanguage (Translations.ledger userLanguage)

                _ ->
                    Translations.confirm_the_transaction userLanguage
    in
    div [ class "copy" ]
        [ h4 []
            [ text headerText ]
        , div [ class "enable-asset" ]
            [ div [ class "connecting-ring connecting-ring--for-borrowing" ]
                [ div [] [] ]
            , p [ class "small text-center" ]
                [ text confirmTransactionText ]
            ]
        ]


awaitingPendingTransactionView : Translations.Lang -> Config -> CustomerAddress -> CollateralToggleModalState -> Transaction -> Model -> Html Msg
awaitingPendingTransactionView userLanguage config customerAddress collateralToggleModalState transaction ({ network } as mainModel) =
    let
        ( headerText, _ ) =
            getModalDescriptions userLanguage config customerAddress collateralToggleModalState mainModel

        confirmTransactionText =
            Translations.transaction_broadcast_no_estimation userLanguage

        etherScanButton =
            Ethereum.etherscanLink
                network
                (Ethereum.TransactionHash transaction.trxHash)
                [ class "submit-button button main borrow" ]
                [ text (Translations.view_on_etherscan userLanguage) ]
    in
    div [ class "copy" ]
        [ h4 []
            [ text headerText ]
        , div [ class "enable-asset" ]
            [ div [ class "connecting-ring connecting-ring--for-borrowing" ]
                [ div [] [] ]
            , p [ class "small text-center" ]
                [ text confirmTransactionText ]
            , etherScanButton
            ]
        ]


borrowingLimitsView : Translations.Lang -> Config -> Network -> CustomerAddress -> Maybe Decimal -> CollateralToggleModalState -> Model -> Html Msg
borrowingLimitsView userLanguage config network customerAddress maybeEtherUsdPrice ({ chosenAsset, entering } as collateralToggleModalState) mainModel =
    let
        cTokens =
            Dict.values mainModel.tokenState.cTokens

        balanceTotalsUsd =
            Balances.getUnderlyingTotalsInUsd mainModel.compoundState cTokens mainModel.oracleState

        ( preActionBorrowLimitUsd, postActionBorrowLimitUsd ) =
            getPreAndPostActionBorrowLimits config customerAddress collateralToggleModalState mainModel

        preActionBorrowPercentAsWhole =
            case Decimal.fastdiv balanceTotalsUsd.totalBorrow preActionBorrowLimitUsd of
                Just percent ->
                    percent

                Nothing ->
                    Decimal.zero

        ( borrowPercentOfNewLimitActual, borrowPercentOfNewLimitWhole ) =
            let
                borrowPercent =
                    case Decimal.fastdiv balanceTotalsUsd.totalBorrow postActionBorrowLimitUsd of
                        Just percent ->
                            percent

                        Nothing ->
                            Decimal.zero
            in
            ( borrowPercent
                |> Decimal.mul (Decimal.fromInt 100)
            , borrowPercent
            )

        barClass =
            if Decimal.lt borrowPercentOfNewLimitActual Decimal.zero then
                "red"

            else
                case getBorrowingRisk borrowPercentOfNewLimitActual of
                    HighRisk ->
                        "red"

                    MediumRisk ->
                        "yellow"

                    LowRisk ->
                        "green"

                    NoRisk ->
                        "green"

        ( headerText, descriptionParagraph ) =
            getModalDescriptions userLanguage config customerAddress collateralToggleModalState mainModel

        borrowLimitContent =
            div [ class "calculation" ]
                [ span [ class "description" ] [ text (Translations.borrow_limit mainModel.userLanguage) ]
                , div [ class "row" ]
                    [ span [] [ text (formatCurrencyFunc preActionBorrowLimitUsd) ]
                    , span [ class "arrow borrow" ] []
                    , span [] [ text (formatCurrencyFunc (Functions.decimalMax postActionBorrowLimitUsd Decimal.zero)) ]
                    ]
                ]

        borrowLimitUsedContent =
            let
                postActionUsedContent =
                    if Decimal.lt borrowPercentOfNewLimitActual Decimal.zero || Decimal.gte borrowPercentOfNewLimitActual (Decimal.fromInt 100) then
                        span [] [ text "Liquidation" ]

                    else
                        span [] [ text (formatPercentageToNearestWhole borrowPercentOfNewLimitWhole) ]
            in
            div [ class "calculation" ]
                [ span [ class "description" ] [ text (Translations.borrow_limit_used userLanguage) ]
                , div [ class "row" ]
                    [ span [] [ text (formatPercentageToNearestWhole preActionBorrowPercentAsWhole) ]
                    , span [ class "arrow borrow" ] []
                    , postActionUsedContent
                    ]
                ]

        submitButton =
            if not entering then
                if Decimal.lt borrowPercentOfNewLimitActual Decimal.zero || Decimal.gte borrowPercentOfNewLimitActual (Decimal.fromInt 100) then
                    button [ class "submit-button button borrow", onClickStopPropagation (ForParent DismissAndResetCollateralModal) ]
                        [ text (Translations.dismiss userLanguage) ]

                else
                    button
                        [ class "submit-button button borrow"
                        , onClickStopPropagation (WrappedCompoundMsg <| Eth.Compound.Web3TransactionMsg <| Eth.Compound.ExitMarket network config.comptroller chosenAsset.contractAddress customerAddress)
                        ]
                        [ text (Translations.disable_token userLanguage chosenAsset.underlying.symbol) ]

            else
                button
                    [ class "submit-button button borrow"
                    , onClickStopPropagation (WrappedCompoundMsg <| Eth.Compound.Web3TransactionMsg <| Eth.Compound.EnterMarkets network config.comptroller [ chosenAsset.contractAddress ] customerAddress)
                    ]
                    [ text (Translations.use_token_as_collateral userLanguage chosenAsset.underlying.symbol) ]

        formatCurrencyFunc usdValue =
            DisplayCurrency.formatDisplayCurrencyInNumberSpec mainModel.preferences.displayCurrency maybeEtherUsdPrice (DisplayCurrency.UsdValue usdValue)
    in
    div [ class "copy" ]
        [ h4 []
            [ text headerText ]
        , descriptionParagraph
        , div [ class "form" ]
            [ borrowLimitContent
            , borrowLimitUsedContent
            ]
        , div [ class ("market-bar " ++ barClass) ]
            [ div [ class "bar" ]
                [ div [ class "fill", style "width" (formatRate (Functions.decimalMin (Decimal.fromInt 100) borrowPercentOfNewLimitActual)) ]
                    []
                ]
            ]
        , submitButton
        ]



-- HELPERS


getPreAndPostActionBorrowLimits : Config -> CustomerAddress -> CollateralToggleModalState -> Model -> ( Decimal, Decimal )
getPreAndPostActionBorrowLimits config customerAddress { chosenAsset, entering } mainModel =
    let
        cTokens =
            Dict.values mainModel.tokenState.cTokens

        preActionBorrowLimitUsd =
            Utils.SafeLiquidity.getCurrentBorrowLimitUsd mainModel.compoundState mainModel.tokenState mainModel.oracleState

        tokenValueUsd =
            Eth.Oracle.getOraclePrice mainModel.oracleState chosenAsset.underlying
                |> Maybe.withDefault Decimal.zero

        collateralFactor =
            mainModel.compoundState.cTokensMetadata
                |> Dict.get (Ethereum.getContractAddressString chosenAsset.contractAddress)
                |> Maybe.map .collateralFactor
                |> Maybe.withDefault Decimal.zero

        supplyBalance =
            Balances.getUnderlyingBalances mainModel.compoundState chosenAsset.contractAddress
                |> Maybe.map .underlyingSupplyBalance
                |> Maybe.withDefault Decimal.zero

        supplyWithCollateralFactor =
            Decimal.mul supplyBalance tokenValueUsd
                |> Decimal.mul collateralFactor

        postActionBorrowLimitUsd =
            if not entering then
                Decimal.mul supplyBalance tokenValueUsd
                    |> Decimal.mul collateralFactor
                    |> Decimal.sub preActionBorrowLimitUsd

            else
                Decimal.add preActionBorrowLimitUsd supplyWithCollateralFactor
    in
    ( preActionBorrowLimitUsd, postActionBorrowLimitUsd )


getMostRecentAssetPendingTransaction : Config -> CollateralToggleModalState -> Model -> Maybe Transaction
getMostRecentAssetPendingTransaction config { chosenAsset } { currentTime, network, account, tokenState, transactionState, bnTransactionState } =
    let
        pendingRecentTransactions =
            transactionState.transactions
                |> Eth.Transaction.getPendingTransactionsForAccount network account (Eth.Transaction.getDefaultOldestPendingTrxTime currentTime) (Just bnTransactionState)

        -- Any transaction for the asset will block the whole input view.
        pendingAssetTransactions =
            case ( network, account ) of
                ( Just actualNetwork, Acct customerAddress _ ) ->
                    Eth.Transaction.filteredTransactionsByCToken pendingRecentTransactions config actualNetwork customerAddress tokenState.cTokens chosenAsset

                _ ->
                    []
    in
    pendingAssetTransactions
        |> List.head
