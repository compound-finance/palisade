module DappInterface.PrimaryActionModal exposing
    ( InternalMsg
    , ParentMsg(..)
    , TranslationDictionary
    , Translator
    , handleBNTransactionUpdate
    , handleCompoundUpdate
    , handleTokenUpdate
    , handleTransactionUpdate
    , prepareToShowModal
    , translator
    , update
    , view
    )

import Balances
import Browser.Dom
import CompoundComponents.Console as Console
import CompoundComponents.DisplayCurrency as DisplayCurrency
import CompoundComponents.Eth.ConnectedEthWallet as ConnectedEthWallet
import CompoundComponents.Eth.Ethereum as Ethereum exposing (Account(..))
import CompoundComponents.Eth.Network as Network exposing (Network)
import CompoundComponents.Ether.BNTransaction as BNTransaction exposing (BNTransactionMsg)
import CompoundComponents.Functions as Functions
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (HrefLinkType(..), autocomplete, class, href, id, onClickStopPropagation, placeholder, style, target, type_, value)
import CompoundComponents.Utils.Markup exposing (disabled)
import CompoundComponents.Utils.NumberFormatter as NumberFormatter exposing (formatPercentageWithDots, formatRate)
import DappInterface.BorrowingPane
import DappInterface.CollateralPane
import DappInterface.Container
import DappInterface.MainModel exposing (BorrowingRisk(..), Model, PrimaryActionModalInput(..), PrimaryActionModalState, PrimaryActionType(..), getBorrowingRisk, getCurrentConfig)
import Decimal exposing (Decimal)
import Dict
import Eth.Compound exposing (CompoundMsg, CompoundState, cTokenIsApproved)
import Eth.Config exposing (Config)
import Eth.Oracle exposing (OracleState)
import Eth.Token exposing (CToken, TokenMsg, TokenState)
import Eth.Transaction exposing (Transaction, TransactionMsg)
import Eth.Validations exposing (hasSufficientBalanceForSupply, hasSufficientCollateralForBorrow)
import Html exposing (Html, a, button, div, input, label, p, span, text)
import Html.Events exposing (onClick, onInput)
import Strings.Translations as Translations
import Task
import Utils.CompAPYHelper exposing (compRate)
import Utils.SafeLiquidity


type ParentMsg
    = DismissAndResetActionModal


type InternalMsg
    = ChangeActionType PrimaryActionType
    | InputChanged String
    | MaxClicked PrimaryActionType Config CToken
    | NoOp
    | Error String


type Msg
    = ForSelf InternalMsg
    | ForParent ParentMsg
    | WrappedCompoundMsg CompoundMsg
    | WrappedTokenMsg TokenMsg


type alias TranslationDictionary msg =
    { onInternalMessage : InternalMsg -> msg
    , onParentMsg : ParentMsg -> msg
    , onWrappedCompoundMsg : CompoundMsg -> msg
    , onWrappedTokenMsg : TokenMsg -> msg
    }


type alias Translator msg =
    Msg -> msg


translator : TranslationDictionary msg -> Translator msg
translator { onInternalMessage, onParentMsg, onWrappedCompoundMsg, onWrappedTokenMsg } msg =
    case msg of
        ForSelf internal ->
            onInternalMessage internal

        ForParent parentMsg ->
            onParentMsg parentMsg

        WrappedCompoundMsg tokenMsg ->
            onWrappedCompoundMsg tokenMsg

        WrappedTokenMsg tokenMsg ->
            onWrappedTokenMsg tokenMsg


prepareToShowModal : Maybe Config -> DappInterface.Container.ParentMsg -> Model -> ( Maybe PrimaryActionModalState, Cmd Msg )
prepareToShowModal maybeConfig protocolActionRequest ({ tokenState } as mainModel) =
    let
        ( actionType, maybeClickedCToken ) =
            case protocolActionRequest of
                DappInterface.Container.MintActionRequest maybeCollateralCToken ->
                    ( MintAction, maybeCollateralCToken )

                DappInterface.Container.WithdrawActionRequest ->
                    ( RedeemAction, Nothing )

                DappInterface.Container.BorrowActionRequest maybeBorrowedCToken ->
                    ( BorrowAction, maybeBorrowedCToken )

                DappInterface.Container.PayBorrowActionRequest ->
                    ( RepayBorrowAction, Nothing )

                _ ->
                    -- Should not ever happen
                    ( MintAction, Nothing )

        -- We select Eth on Supply and USDC on Borrow as defaults
        -- Diff logic on withdrawing or repaying assets.
        maybePreSelectedCToken =
            if maybeClickedCToken /= Nothing then
                maybeClickedCToken

            else
                maybeConfig
                    |> Maybe.map
                        (\config ->
                            case actionType of
                                MintAction ->
                                    tokenState.cTokens
                                        |> Dict.values
                                        |> List.filter (\cToken -> Eth.Token.isCEtherToken config cToken)
                                        |> List.head

                                RedeemAction ->
                                    DappInterface.CollateralPane.getReadyCollateralAssets mainModel True
                                        |> List.head
                                        |> Maybe.map .cToken

                                BorrowAction ->
                                    tokenState.cTokens
                                        |> Dict.values
                                        |> List.filter (\cToken -> cToken.underlying.symbol == "USDC")
                                        |> List.head

                                RepayBorrowAction ->
                                    DappInterface.BorrowingPane.getReadyBorrowedAssets mainModel True
                                        |> List.head
                                        |> Maybe.map .cToken
                        )
                    |> Functions.demaybeify

        newPrimaryActionModalState =
            maybePreSelectedCToken
                |> Maybe.map
                    (\preSelectedCToken ->
                        { chosenAsset = preSelectedCToken
                        , primaryActionType = actionType
                        , inputActionPaneState = DappInterface.MainModel.ChoosingInputs
                        , inputValue = DappInterface.MainModel.Empty
                        , errors = []
                        }
                    )
    in
    ( newPrimaryActionModalState
    , Task.attempt (\_ -> ForSelf NoOp) (Browser.Dom.focus "action-input-box")
    )


getModalInput : String -> PrimaryActionModalInput
getModalInput newText =
    if String.isEmpty newText then
        Empty

    else
        Normal ( newText, Decimal.fromString newText )


update : Account -> CompoundState -> TokenState -> OracleState -> InternalMsg -> PrimaryActionModalState -> ( PrimaryActionModalState, Cmd Msg )
update account compoundState tokenState oracleState msg state =
    case msg of
        ChangeActionType newActionType ->
            let
                -- If we are changing states and max input is involved then let's convert it back to empty
                -- so we don't have funny stuff happening.
                updatedInput =
                    if state.primaryActionType /= newActionType && state.inputValue == Max then
                        Empty

                    else
                        state.inputValue
            in
            ( { state | primaryActionType = newActionType, inputValue = updatedInput }
            , Task.attempt (\_ -> ForSelf NoOp) (Browser.Dom.focus "action-input-box")
            )

        InputChanged newText ->
            ( { state | inputValue = getModalInput newText }, Cmd.none )

        MaxClicked primaryActionType config chosenAsset ->
            let
                truncatedInputString : Decimal -> String
                truncatedInputString actualDecimal =
                    actualDecimal
                        |> Decimal.truncate -8
                        |> Decimal.toString

                newInput =
                    case primaryActionType of
                        MintAction ->
                            let
                                walletBalance =
                                    Balances.getWalletBalanceSafeEther config account compoundState chosenAsset
                                        |> Maybe.withDefault Decimal.zero
                            in
                            Normal ( truncatedInputString walletBalance, Just walletBalance )

                        RedeemAction ->
                            let
                                tokenSupplyBalance =
                                    Balances.getUnderlyingBalances compoundState chosenAsset.contractAddress
                                        |> Maybe.map .underlyingSupplyBalance
                                        |> Maybe.withDefault Decimal.zero

                                maxSafeWithdraw =
                                    Utils.SafeLiquidity.getSafeMaxWithdrawForToken config compoundState tokenState oracleState chosenAsset tokenSupplyBalance
                            in
                            if Decimal.lte tokenSupplyBalance maxSafeWithdraw then
                                Max

                            else
                                Normal ( truncatedInputString maxSafeWithdraw, Just maxSafeWithdraw )

                        BorrowAction ->
                            let
                                maxSafeBorrow =
                                    Utils.SafeLiquidity.getSafeMaxBorrowForToken compoundState tokenState oracleState chosenAsset.underlying
                            in
                            Normal ( truncatedInputString maxSafeBorrow, Just maxSafeBorrow )

                        RepayBorrowAction ->
                            let
                                walletBalance =
                                    Balances.getWalletBalanceSafeEther config account compoundState chosenAsset
                                        |> Maybe.withDefault Decimal.zero

                                borrowBalance =
                                    Balances.getUnderlyingBalances compoundState chosenAsset.contractAddress
                                        |> Maybe.map .underlyingBorrowBalance
                                        |> Maybe.withDefault Decimal.zero
                            in
                            if Decimal.gt walletBalance borrowBalance then
                                Max

                            else
                                Normal ( truncatedInputString walletBalance, Just walletBalance )
            in
            ( { state | inputValue = newInput }, Cmd.none )

        NoOp ->
            ( state, Cmd.none )

        Error error ->
            ( { state | errors = error :: state.errors }, Console.error error )


handleCompoundUpdate : CompoundMsg -> PrimaryActionModalState -> PrimaryActionModalState
handleCompoundUpdate compoundMsg ({ chosenAsset, inputActionPaneState } as state) =
    let
        updateStateIfCorrectAsset : Ethereum.ContractAddress -> PrimaryActionModalState
        updateStateIfCorrectAsset targetContractAddress =
            if targetContractAddress == chosenAsset.contractAddress && inputActionPaneState == DappInterface.MainModel.ChoosingInputs then
                { state | inputActionPaneState = DappInterface.MainModel.AwaitingConfirmTransaction }

            else
                state
    in
    case compoundMsg of
        Eth.Compound.Web3TransactionMsg (Eth.Compound.CTokenMint _ contractAddress _ _ _) ->
            updateStateIfCorrectAsset contractAddress

        Eth.Compound.Web3TransactionMsg (Eth.Compound.CTokenRedeem _ contractAddress _ _ _ _ _) ->
            updateStateIfCorrectAsset contractAddress

        Eth.Compound.Web3TransactionMsg (Eth.Compound.CTokenBorrow _ contractAddress _ _ _) ->
            updateStateIfCorrectAsset contractAddress

        Eth.Compound.Web3TransactionMsg (Eth.Compound.CTokenRepayBorrow _ contractAddress _ _ _) ->
            updateStateIfCorrectAsset contractAddress

        _ ->
            state


handleTokenUpdate : TokenMsg -> PrimaryActionModalState -> PrimaryActionModalState
handleTokenUpdate tokenMsg ({ chosenAsset } as state) =
    case tokenMsg of
        Eth.Token.Web3TransactionMsg (Eth.Token.FaucetTokenApprove network cTokenAddress _ _ _) ->
            if cTokenAddress == chosenAsset.contractAddress then
                { state | inputActionPaneState = DappInterface.MainModel.AwaitingConfirmTransaction }

            else
                state

        _ ->
            state


updateStateForTrxUpdates : Config -> PrimaryActionModalState -> Model -> PrimaryActionModalState
updateStateForTrxUpdates config ({ inputActionPaneState } as state) mainModel =
    let
        mostRecentPendingTransaction =
            getMostRecentAssetPendingTransaction config state mainModel
    in
    case mostRecentPendingTransaction of
        Just recentPendingTrx ->
            if inputActionPaneState == DappInterface.MainModel.AwaitingConfirmTransaction then
                { state | inputActionPaneState = DappInterface.MainModel.ChoosingInputs, inputValue = Empty }

            else
                state

        Nothing ->
            { state | inputActionPaneState = DappInterface.MainModel.ChoosingInputs, inputValue = Empty }


handleTransactionUpdate : Maybe Config -> TransactionMsg -> PrimaryActionModalState -> Model -> PrimaryActionModalState
handleTransactionUpdate maybeConfig transactionMsg state mainModel =
    case ( maybeConfig, mainModel.network, transactionMsg ) of
        ( Just config, Just _, Eth.Transaction.UpdateTransaction _ ) ->
            updateStateForTrxUpdates config state mainModel

        _ ->
            state


handleBNTransactionUpdate : Maybe Config -> BNTransactionMsg -> PrimaryActionModalState -> Model -> PrimaryActionModalState
handleBNTransactionUpdate maybeConfig bnTransactionMsg state mainModel =
    case ( maybeConfig, mainModel.network, bnTransactionMsg ) of
        ( Just config, Just _, BNTransaction.TransactionStateChange _ ) ->
            updateStateForTrxUpdates config state mainModel

        _ ->
            state


view : Model -> Html Msg
view mainModel =
    let
        maybeConfig =
            getCurrentConfig mainModel

        maybeEtherUsdPrice =
            maybeConfig
                |> Maybe.andThen (\config -> Eth.Oracle.getEtherPrice config mainModel.tokenState mainModel.oracleState)
    in
    case mainModel.primaryActionModalState of
        Just visibleActionModalState ->
            div [ class "modal vote__modal" ]
                [ div [ class "cover active", onClick (ForParent DismissAndResetActionModal) ]
                    []
                , div [ class "container" ]
                    [ inputActionPane mainModel.userLanguage maybeConfig maybeEtherUsdPrice visibleActionModalState mainModel
                    ]
                ]

        Nothing ->
            text ""



-- aka right side of pane (bottom on mobile)


marketDetailPageUrl : CToken -> String
marketDetailPageUrl chosenAsset =
    let
        targetUnderlyingSymbol =
            if chosenAsset.symbol == "cWBTC2" then
                "WBTC2"

            else
                chosenAsset.underlying.symbol
    in
    "https://compound.finance/markets/" ++ targetUnderlyingSymbol


inputActionPane : Translations.Lang -> Maybe Config -> Maybe Decimal -> PrimaryActionModalState -> Model -> Html Msg
inputActionPane userLanguage maybeConfig maybeEtherUsdPrice primaryActionModalState mainModel =
    let
        ( tokenApproved, maybePendingTransaction ) =
            case maybeConfig of
                Just config ->
                    ( cTokenIsApproved config primaryActionModalState.chosenAsset mainModel.compoundState
                    , getMostRecentAssetPendingTransaction config primaryActionModalState mainModel
                    )

                Nothing ->
                    ( False
                    , Nothing
                    )

        chooseInputView =
            case maybeConfig of
                Just config ->
                    case primaryActionModalState.primaryActionType of
                        MintAction ->
                            if not tokenApproved then
                                enableAssetView mainModel.userLanguage config maybeEtherUsdPrice primaryActionModalState mainModel

                            else
                                supplyAndRedeemCollateralView mainModel.userLanguage config maybeEtherUsdPrice primaryActionModalState mainModel

                        RedeemAction ->
                            supplyAndRedeemCollateralView mainModel.userLanguage config maybeEtherUsdPrice primaryActionModalState mainModel

                        BorrowAction ->
                            borrowAndRepayView mainModel.userLanguage config maybeEtherUsdPrice primaryActionModalState mainModel

                        RepayBorrowAction ->
                            if not tokenApproved then
                                enableAssetView mainModel.userLanguage config maybeEtherUsdPrice primaryActionModalState mainModel

                            else
                                borrowAndRepayView mainModel.userLanguage config maybeEtherUsdPrice primaryActionModalState mainModel

                Nothing ->
                    text "Unsupported Configuration and Network"

        -- If we are awaiting a pending transaction or we are awaiting the confirmation of an action,
        -- then we should override the input field and show the waiting spinner.
        ( titleView, contentView ) =
            case maybePendingTransaction of
                Just pendingTransaction ->
                    ( div [ class "title" ]
                        [ span [] [ text (Translations.transaction_pending userLanguage) ]
                        ]
                    , awaitingPendingTransactionView pendingTransaction primaryActionModalState mainModel
                    )

                _ ->
                    if primaryActionModalState.inputActionPaneState == DappInterface.MainModel.AwaitingConfirmTransaction then
                        ( div [ class "title" ]
                            [ span [] [ text (Translations.confirm_transaction userLanguage) ]
                            ]
                        , awaitingWeb3ConfirmView mainModel.userLanguage primaryActionModalState mainModel
                        )

                    else
                        ( div [ class "title" ]
                            [ span [ class ("icon icon--small icon--" ++ primaryActionModalState.chosenAsset.underlying.symbol) ] []
                            , span [] [ text primaryActionModalState.chosenAsset.underlying.name ]
                            ]
                        , chooseInputView
                        )
    in
    div [ id "inputActionPanel", class "legacy-panel" ]
        [ div [ class "header" ]
            [ titleView
            , div [ class "close-x" ]
                [ button [ onClick (ForParent DismissAndResetActionModal) ]
                    []
                ]
            ]
        , contentView
        ]


actionToggle : Translations.Lang -> PrimaryActionModalState -> Html Msg
actionToggle userLanguage { primaryActionType } =
    let
        ( leftTabClass, rightTabClass ) =
            case primaryActionType of
                MintAction ->
                    ( "tab-group__option tab-group__option--active tab-group__option--active--large tab-group__option--active--large--green"
                    , "tab-group__option tab-group__option--action-modal-grey"
                    )

                RedeemAction ->
                    ( "tab-group__option tab-group__option--action-modal-grey"
                    , "tab-group__option tab-group__option--active tab-group__option--active--large tab-group__option--active--large--green"
                    )

                BorrowAction ->
                    ( "tab-group__option tab-group__option--active tab-group__option--active--large tab-group__option--active--large--purple"
                    , "tab-group__option tab-group__option--action-modal-grey"
                    )

                RepayBorrowAction ->
                    ( "tab-group__option tab-group__option--action-modal-grey"
                    , "tab-group__option tab-group__option--active tab-group__option--active--large tab-group__option--active--large--purple"
                    )

        { leftTabActionMsg, leftTabButtonText, rightTabActionMsg, rightTabButtonText } =
            if primaryActionType == MintAction || primaryActionType == RedeemAction then
                { leftTabActionMsg = ForSelf (ChangeActionType MintAction)
                , leftTabButtonText = Translations.supply userLanguage
                , rightTabActionMsg = ForSelf (ChangeActionType RedeemAction)
                , rightTabButtonText = Translations.withdraw userLanguage
                }

            else
                { leftTabActionMsg = ForSelf (ChangeActionType BorrowAction)
                , leftTabButtonText = Translations.borrow userLanguage
                , rightTabActionMsg = ForSelf (ChangeActionType RepayBorrowAction)
                , rightTabButtonText = Translations.repay userLanguage
                }
    in
    div [ class "tab-group" ]
        [ div [ class "tab-group__options" ]
            [ label [ class leftTabClass, onClickStopPropagation leftTabActionMsg ] [ text leftTabButtonText ]
            , label [ class rightTabClass, onClickStopPropagation rightTabActionMsg ] [ text rightTabButtonText ]
            ]
        , div [ class "tab-group__line" ] []
        ]


assetAndCompRateForm : Translations.Lang -> Config -> Maybe Decimal -> PrimaryActionModalState -> Model -> Html Msg
assetAndCompRateForm userLanguage config maybeEtherUsdPrice ({ chosenAsset, primaryActionType } as primaryActionModalState) mainModel =
    let
        ( formLabel, rateLabel ) =
            if primaryActionType == MintAction || primaryActionType == RedeemAction then
                ( Translations.supply_rates userLanguage
                , Translations.supply_apy userLanguage
                )

            else
                ( Translations.borrow_rates userLanguage
                , Translations.borrow_apy userLanguage
                )

        interestRate =
            interestRateForType chosenAsset primaryActionModalState mainModel.compoundState

        tokenValueUsd =
            Eth.Oracle.getOraclePrice mainModel.oracleState chosenAsset.underlying
                |> Maybe.withDefault Decimal.zero

        maybeCompUSDPrice =
            Eth.Oracle.getCompPriceUSD config mainModel.oracleState

        cTokenAddressString =
            Ethereum.getContractAddressString primaryActionModalState.chosenAsset.contractAddress

        ( marketTotalUSDValue, compSpeedPerDay ) =
            mainModel.compoundState.cTokensMetadata
                |> Dict.get cTokenAddressString
                |> Maybe.map
                    (\cTokenMetadata ->
                        let
                            marketTotalUsdForAction =
                                if primaryActionType == MintAction || primaryActionType == RedeemAction then
                                    cTokenMetadata.totalSupplyUnderlying
                                        |> Decimal.mul tokenValueUsd

                                else
                                    cTokenMetadata.totalBorrows
                                        |> Decimal.mul tokenValueUsd

                            compSpeedPerDayForAction =
                                if primaryActionType == MintAction || primaryActionType == RedeemAction then
                                    cTokenMetadata.compSupplySpeedPerDay
                                else
                                    cTokenMetadata.compBorrowSpeedPerDay
                        in
                        ( marketTotalUsdForAction, compSpeedPerDayForAction )
                    )
                |> Maybe.withDefault ( Decimal.zero, Decimal.zero )

        distributionApyText =
            case maybeCompUSDPrice of
                Just compUSDPrice ->
                    let
                        maybeDistributionCompAPY =
                            compRate compUSDPrice compSpeedPerDay marketTotalUSDValue
                    in
                    case maybeDistributionCompAPY of
                        Just distributionCompAPY ->
                            if Decimal.gt distributionCompAPY (Decimal.fromInt 1000000) then
                                "– %"

                            else
                                formatPercentageWithDots maybeDistributionCompAPY

                        Nothing ->
                            "– %"

                Nothing ->
                    "– %"
    in
    div [ class "form" ]
        [ a ([ class "label-link", target "__blank" ] ++ href External (marketDetailPageUrl chosenAsset))
            [ label [ class "dark" ] [ text formLabel ]
            , div [ class "line-icon line-icon--small line-icon--external-link line-icon--external-link--black" ] []
            ]
        , div [ class "calculation" ]
            [ span []
                [ span [ class ("icon icon--" ++ chosenAsset.underlying.symbol) ] []
                , span [ class "description" ]
                    [ text rateLabel
                    ]
                ]
            , span [] [ text (formatPercentageWithDots interestRate) ]
            ]
        , div [ class "calculation" ]
            [ span []
                [ span [ class "icon icon--COMP" ] []
                , span [ class "description" ]
                    [ text (Translations.distribution_apy userLanguage)
                    ]
                ]
            , span [] [ text distributionApyText ]
            ]
        ]


enableAssetView : Translations.Lang -> Config -> Maybe Decimal -> PrimaryActionModalState -> Model -> Html Msg
enableAssetView userLanguage config maybeEtherUsdPrice ({ chosenAsset, primaryActionType } as primaryActionModalState) mainModel =
    let
        enableButton =
            let
                buttonTypeClass =
                    if primaryActionType == MintAction || primaryActionType == RedeemAction then
                        ""

                    else
                        " borrow"
            in
            case ( mainModel.network, mainModel.account ) of
                ( Just network, Acct customerAddress _ ) ->
                    let
                        enableClickAction =
                            Eth.Token.FaucetTokenApprove
                                network
                                chosenAsset.contractAddress
                                chosenAsset.underlying.assetAddress
                                customerAddress
                                True
                                |> Eth.Token.Web3TransactionMsg
                                |> WrappedTokenMsg
                    in
                    button [ class ("submit-button button main" ++ buttonTypeClass), onClick enableClickAction ] [ text (Translations.enable userLanguage) ]

                _ ->
                    button [ class ("submit-button button main" ++ buttonTypeClass), disabled ] [ text (Translations.enable userLanguage) ]
    in
    div [ class "input-action" ]
        [ div [ class "enable-asset" ]
            [ div [ class ("center-icon ctoken ctoken--" ++ chosenAsset.symbol) ]
                []
            , p [ class "small text-center" ]
                [ text (Translations.enable_before_supply_or_repay userLanguage chosenAsset.underlying.name) ]
            ]
        , actionToggle userLanguage primaryActionModalState
        , div [ class "info" ]
            [ assetAndCompRateForm userLanguage config maybeEtherUsdPrice primaryActionModalState mainModel
            , div [ class "action-button" ]
                [ enableButton
                ]
            , bottomBalanceView userLanguage config primaryActionModalState mainModel
            ]
        ]


awaitingWeb3ConfirmView : Translations.Lang -> PrimaryActionModalState -> Model -> Html Msg
awaitingWeb3ConfirmView userLanguage { primaryActionType } { connectedEthWalletModel } =
    let
        connectingRingTypeClass =
            if primaryActionType == MintAction || primaryActionType == RedeemAction then
                ""

            else
                " connecting-ring--for-borrowing"

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
    div [ class "enable-asset extra-bottom-margin" ]
        [ div [ class ("connecting-ring" ++ connectingRingTypeClass) ]
            [ div [] [] ]
        , p [ class "small text-center" ]
            [ text confirmTransactionText ]
        ]


awaitingPendingTransactionView : Transaction -> PrimaryActionModalState -> Model -> Html Msg
awaitingPendingTransactionView transaction { primaryActionType } { network, userLanguage } =
    let
        ( connectingRingTypeClass, buttonTypeClass ) =
            if primaryActionType == MintAction || primaryActionType == RedeemAction then
                ( ""
                , ""
                )

            else
                ( " connecting-ring--for-borrowing"
                , " borrow"
                )

        confirmTransactionText =
            Translations.transaction_broadcast_no_estimation userLanguage

        etherScanButton =
            Ethereum.etherscanLink
                network
                (Ethereum.TransactionHash transaction.trxHash)
                [ class ("etherscan-button button main" ++ buttonTypeClass) ]
                [ text (Translations.view_on_etherscan userLanguage) ]
    in
    div [ class "enable-asset extra-bottom-margin" ]
        [ div [ class ("connecting-ring" ++ connectingRingTypeClass) ]
            [ div [] [] ]
        , p [ class "small text-center" ]
            [ text confirmTransactionText ]
        , etherScanButton
        ]


supplyAndRedeemCollateralView : Translations.Lang -> Config -> Maybe Decimal -> PrimaryActionModalState -> Model -> Html Msg
supplyAndRedeemCollateralView userLanguage config maybeEtherUsdPrice ({ chosenAsset, inputValue, primaryActionType } as primaryActionModalState) mainModel =
    let
        cTokens =
            Dict.values mainModel.tokenState.cTokens

        balanceTotalsUsd =
            Balances.getUnderlyingTotalsInUsd mainModel.compoundState cTokens mainModel.oracleState

        maxAvailableTokens =
            getAvailableTokensForAction (Just config) primaryActionModalState mainModel

        preActionBorrowLimitUsd =
            Utils.SafeLiquidity.getCurrentBorrowLimitUsd mainModel.compoundState mainModel.tokenState mainModel.oracleState

        tokenValueUsd =
            Eth.Oracle.getOraclePrice mainModel.oracleState chosenAsset.underlying
                |> Maybe.withDefault Decimal.zero

        inputValueDecimal =
            case inputValue of
                Normal ( _, Just inputDecimal ) ->
                    inputDecimal

                Max ->
                    maxAvailableTokens

                _ ->
                    Decimal.zero

        collateralFactor =
            mainModel.compoundState.cTokensMetadata
                |> Dict.get (Ethereum.getContractAddressString chosenAsset.contractAddress)
                |> Maybe.map .collateralFactor
                |> Maybe.withDefault Decimal.zero

        inputWithCollateralFactor =
            Decimal.mul inputValueDecimal tokenValueUsd
                |> Decimal.mul collateralFactor

        postActionBorrowLimitUsd =
            case ( Balances.hasEnteredAsset config mainModel.compoundState chosenAsset, primaryActionType ) of
                ( True, MintAction ) ->
                    Decimal.add preActionBorrowLimitUsd inputWithCollateralFactor

                ( True, RedeemAction ) ->
                    Decimal.sub preActionBorrowLimitUsd inputWithCollateralFactor

                _ ->
                    preActionBorrowLimitUsd

        borrowPercentOfNewLimit =
            (case Decimal.fastdiv balanceTotalsUsd.totalBorrow postActionBorrowLimitUsd of
                Just percent ->
                    percent

                Nothing ->
                    Decimal.zero
            )
                |> Decimal.mul (Decimal.fromInt 100)
                |> Functions.decimalMin (Decimal.fromInt 100)

        barClass =
            case getBorrowingRisk borrowPercentOfNewLimit of
                HighRisk ->
                    "red"

                MediumRisk ->
                    "yellow"

                LowRisk ->
                    "green"

                NoRisk ->
                    "green"
    in
    div [ class "input-action" ]
        [ div [ class "input" ]
            [ getInputBox config primaryActionModalState mainModel
            , getMaxButton userLanguage config primaryActionModalState mainModel
            ]
        , actionToggle userLanguage primaryActionModalState
        , div [ class "info" ]
            [ assetAndCompRateForm userLanguage config maybeEtherUsdPrice primaryActionModalState mainModel
            , div [ class "form" ]
                [ label [ class "dark" ] [ text (Translations.borrow_limit userLanguage) ]
                , borrowLimitWithAdjustedInputView config maybeEtherUsdPrice primaryActionModalState mainModel
                , borrowLimitUsedWithAdjustedInputView config primaryActionModalState mainModel
                ]
            , div [ class ("market-bar " ++ barClass) ]
                [ div [ class "bar" ]
                    [ div [ class "fill", style "width" (formatRate borrowPercentOfNewLimit) ]
                        []
                    ]
                ]
            , div [ class "action-button" ]
                [ getSubmitButton userLanguage config primaryActionModalState mainModel
                ]
            , bottomBalanceView userLanguage config primaryActionModalState mainModel
            , faucetAllocateButton config mainModel chosenAsset
            ]
        ]


borrowAndRepayView : Translations.Lang -> Config -> Maybe Decimal -> PrimaryActionModalState -> Model -> Html Msg
borrowAndRepayView userLanguage config maybeEtherUsdPrice ({ chosenAsset, inputValue, primaryActionType } as primaryActionModalState) mainModel =
    let
        preActionBorrowBalanceUsd =
            currentBorrowBalanceUsd mainModel

        maxAvailableTokens =
            getAvailableTokensForAction (Just config) primaryActionModalState mainModel

        tokenValueUsd =
            Eth.Oracle.getOraclePrice mainModel.oracleState chosenAsset.underlying
                |> Maybe.withDefault Decimal.zero

        inputValueDecimal =
            case inputValue of
                Normal ( _, Just inputDecimal ) ->
                    inputDecimal

                Max ->
                    maxAvailableTokens

                _ ->
                    Decimal.zero

        inputValueAsUsd =
            inputValueDecimal
                |> Decimal.mul tokenValueUsd

        postActionBorrowBalanceUsd =
            if primaryActionType == BorrowAction then
                Decimal.add preActionBorrowBalanceUsd inputValueAsUsd

            else
                Decimal.sub preActionBorrowBalanceUsd inputValueAsUsd

        currentBorrowLimitUsd =
            Utils.SafeLiquidity.getCurrentBorrowLimitUsd mainModel.compoundState mainModel.tokenState mainModel.oracleState

        newBorrowBalancePercentOfLimit =
            (case Decimal.fastdiv postActionBorrowBalanceUsd currentBorrowLimitUsd of
                Just percent ->
                    percent

                Nothing ->
                    Decimal.zero
            )
                |> Decimal.mul (Decimal.fromInt 100)
                |> Functions.decimalMin (Decimal.fromInt 100)

        barClass =
            case getBorrowingRisk newBorrowBalancePercentOfLimit of
                HighRisk ->
                    "red"

                MediumRisk ->
                    "yellow"

                LowRisk ->
                    "green"

                NoRisk ->
                    "green"
    in
    div [ class "input-action" ]
        [ div [ class "input" ]
            [ getInputBox config primaryActionModalState mainModel
            , getMaxButton userLanguage config primaryActionModalState mainModel
            ]
        , actionToggle userLanguage primaryActionModalState
        , div [ class "info" ]
            [ assetAndCompRateForm userLanguage config maybeEtherUsdPrice primaryActionModalState mainModel
            , div [ class "form" ]
                [ label [ class "dark" ] [ text (Translations.borrow_limit userLanguage) ]
                , borrowBalanceWithAdjustedInputView config maybeEtherUsdPrice primaryActionModalState mainModel
                , borrowLimitUsedWithAdjustedInputView config primaryActionModalState mainModel
                ]
            , div [ class ("market-bar " ++ barClass) ]
                [ div [ class "bar" ]
                    [ div [ class "fill", style "width" (formatRate newBorrowBalancePercentOfLimit) ]
                        []
                    ]
                ]
            , div [ class "action-button" ]
                [ getSubmitButton userLanguage config primaryActionModalState mainModel
                ]
            , bottomBalanceView userLanguage config primaryActionModalState mainModel
            ]
        ]


getSubmitButton : Translations.Lang -> Config -> PrimaryActionModalState -> Model -> Html Msg
getSubmitButton userLanguage config ({ chosenAsset, inputValue, primaryActionType } as primaryActionModalState) mainModel =
    let
        availableTokenBalance =
            getAvailableTokensForAction (Just config) primaryActionModalState mainModel

        actionConstructor panelMessage amount =
            case ( mainModel.network, mainModel.account ) of
                ( Just actualNetwork, Acct customerAddress _ ) ->
                    WrappedCompoundMsg <| Eth.Compound.Web3TransactionMsg <| panelMessage actualNetwork chosenAsset.contractAddress chosenAsset.underlying.decimals customerAddress amount

                _ ->
                    Error "Should not show submit button with no account or config"
                        |> ForSelf

        redeemConstructor panelMessage amount =
            case ( mainModel.network, mainModel.account, Dict.get (Ethereum.getContractAddressString chosenAsset.contractAddress) mainModel.compoundState.cTokensMetadata ) of
                ( Just actualNetwork, Acct customerAddress _, Just cTokenMetadata ) ->
                    WrappedCompoundMsg <| Eth.Compound.Web3TransactionMsg <| panelMessage actualNetwork chosenAsset.contractAddress cTokenMetadata.exchangeRate chosenAsset.decimals chosenAsset.underlying.decimals customerAddress amount

                _ ->
                    ForSelf (Error "Tried to config withdraw submit button without exchangeRate or account.")

        tokenAllowance =
            if Eth.Token.isCEtherToken config chosenAsset then
                Decimal.fromInt 1000000000000000

            else
                Dict.get (Ethereum.getContractAddressString chosenAsset.contractAddress) mainModel.compoundState.balances
                    |> Maybe.map .underlyingTokenAllowance
                    |> Maybe.withDefault Decimal.zero

        notEnoughAllowanceString =
            Translations.insufficient_allowance userLanguage

        panelData :
            { regularWord : String
            , disabledPlaceholdler : String
            , insufficientWord : String
            , maxAmount : Decimal
            , buttonClass : String
            , message : Decimal -> Msg
            , qualifier : Decimal -> Bool
            }
        panelData =
            case primaryActionType of
                MintAction ->
                    { regularWord = Translations.supply userLanguage
                    , disabledPlaceholdler = Translations.no_funds_available userLanguage
                    , insufficientWord = Translations.no_funds_available userLanguage
                    , maxAmount = Decimal.zero
                    , buttonClass = "main"
                    , message = actionConstructor Eth.Compound.CTokenMint
                    , qualifier =
                        hasSufficientBalanceForSupply config mainModel.account mainModel.compoundState chosenAsset
                    }

                RedeemAction ->
                    { regularWord = Translations.withdraw userLanguage
                    , disabledPlaceholdler = Translations.no_balance_to_withdraw userLanguage
                    , insufficientWord = Translations.insufficient_liquidity userLanguage
                    , maxAmount = Decimal.minusOne
                    , buttonClass = "main"
                    , message = redeemConstructor Eth.Compound.CTokenRedeem
                    , qualifier = Decimal.gte availableTokenBalance
                    }

                BorrowAction ->
                    { regularWord = Translations.borrow userLanguage
                    , disabledPlaceholdler = Translations.borrowing_limit_reached userLanguage
                    , insufficientWord = Translations.insufficient_collateral userLanguage
                    , maxAmount = availableTokenBalance
                    , buttonClass = "borrow"
                    , message = actionConstructor Eth.Compound.CTokenBorrow
                    , qualifier =
                        hasSufficientCollateralForBorrow mainModel.compoundState.maybeAccountLiquidityUsd mainModel.oracleState chosenAsset.underlying
                    }

                RepayBorrowAction ->
                    { regularWord = Translations.repay userLanguage
                    , disabledPlaceholdler = Translations.no_funds_available userLanguage
                    , insufficientWord = Translations.no_funds_available userLanguage
                    , maxAmount = Decimal.minusOne
                    , buttonClass = "borrow"
                    , message = actionConstructor Eth.Compound.CTokenRepayBorrow
                    , qualifier = Decimal.gte availableTokenBalance
                    }

        ( proposedAction, proposedText ) =
            case inputValue of
                Normal ( _, Just amount ) ->
                    if Decimal.lte amount Decimal.zero then
                        ( [ disabled ], panelData.regularWord )

                    else if panelData.qualifier amount then
                        ( [ onClick <| panelData.message amount ], panelData.regularWord )

                    else if Decimal.lt tokenAllowance amount && (primaryActionType == MintAction || primaryActionType == RepayBorrowAction) then
                        ( [ disabled ], notEnoughAllowanceString )

                    else
                        ( [ disabled ], panelData.insufficientWord )

                Empty ->
                    ( [ disabled ], panelData.regularWord )

                Max ->
                    let
                        borrowBalance =
                            Balances.getUnderlyingBalances mainModel.compoundState chosenAsset.contractAddress
                                |> Maybe.map .underlyingBorrowBalance
                                |> Maybe.withDefault Decimal.zero
                    in
                    if primaryActionType == RepayBorrowAction && Decimal.lt tokenAllowance borrowBalance then
                        ( [ disabled ], notEnoughAllowanceString )

                    else
                        ( [ onClick <| panelData.message panelData.maxAmount ], panelData.regularWord )

                Normal ( _, Nothing ) ->
                    ( [ disabled ], panelData.regularWord )

        hasReachedBorrowCap =
            let
                inputValueAmount =
                    case inputValue of
                        Normal ( _, Just amount ) ->
                            amount

                        Normal ( _, Nothing ) ->
                            Decimal.zero

                        Empty ->
                            Decimal.zero

                        Max ->
                            panelData.maxAmount

                maybeCTokenMetadata =
                    Dict.get (Ethereum.getContractAddressString chosenAsset.contractAddress) mainModel.compoundState.cTokensMetadata

                borrowCap =
                    maybeCTokenMetadata
                        |> Maybe.map .borrowCap
                        |> Maybe.withDefault Decimal.zero
            in
            case ( Decimal.gt borrowCap Decimal.zero && primaryActionType == BorrowAction, maybeCTokenMetadata ) of
                ( True, Just cTokenMetadata ) ->
                    let
                        totalBorrowsAdjustedWithInput =
                            Decimal.add inputValueAmount cTokenMetadata.totalBorrows
                    in
                    if Decimal.gte totalBorrowsAdjustedWithInput borrowCap then
                        True

                    else
                        False

                _ ->
                    False

        supplyingAndBorrowingPaused =
            case ( mainModel.network, chosenAsset.symbol ) of
                ( Just Network.MainNet, "cWBTC" ) ->
                    primaryActionType == MintAction || primaryActionType == BorrowAction

                _ ->
                    False

        ( buttonAttributes, buttonText ) =
            if Decimal.lte availableTokenBalance Decimal.zero then
                -- Repay is the only special one we want different text for
                let
                    borrowBalance =
                        Balances.getUnderlyingBalances mainModel.compoundState chosenAsset.contractAddress
                            |> Maybe.map .underlyingBorrowBalance
                            |> Maybe.withDefault Decimal.zero

                    disabledText =
                        if primaryActionType == RepayBorrowAction && Decimal.eq borrowBalance Decimal.zero then
                            Translations.no_balance_to_repay userLanguage

                        else
                            panelData.disabledPlaceholdler
                in
                ( [ class ("submit-button button " ++ panelData.buttonClass), disabled ], disabledText )

            else if hasReachedBorrowCap then
                ( [ class ("submit-button button " ++ panelData.buttonClass), disabled ], Translations.token_borrow_cap_reached userLanguage chosenAsset.underlying.symbol )

            else if supplyingAndBorrowingPaused then
                ( [ class ("submit-button button " ++ panelData.buttonClass), disabled ], Translations.market_disabled userLanguage )

            else
                -- Check if the user is trying to supply or repay more than their allowance for the token...
                ( [ class ("submit-button button " ++ panelData.buttonClass) ] ++ proposedAction, proposedText )
    in
    button buttonAttributes [ text buttonText ]


bottomBalanceView : Translations.Lang -> Config -> PrimaryActionModalState -> Model -> Html Msg
bottomBalanceView userLanguage config { chosenAsset, primaryActionType } mainModel =
    let
        ( descriptionString, balanceToShow ) =
            case primaryActionType of
                MintAction ->
                    ( Translations.wallet_balance userLanguage
                    , Balances.getWalletBalanceSafeEther config mainModel.account mainModel.compoundState chosenAsset
                        |> Maybe.withDefault Decimal.zero
                    )

                RedeemAction ->
                    ( Translations.currently_supplying userLanguage
                    , Balances.getUnderlyingBalances mainModel.compoundState chosenAsset.contractAddress
                        |> Maybe.map .underlyingSupplyBalance
                        |> Maybe.withDefault Decimal.zero
                    )

                BorrowAction ->
                    ( Translations.currently_borrowing userLanguage
                    , Balances.getUnderlyingBalances mainModel.compoundState chosenAsset.contractAddress
                        |> Maybe.map .underlyingBorrowBalance
                        |> Maybe.withDefault Decimal.zero
                    )

                RepayBorrowAction ->
                    ( Translations.wallet_balance userLanguage
                    , Balances.getWalletBalanceSafeEther config mainModel.account mainModel.compoundState chosenAsset
                        |> Maybe.withDefault Decimal.zero
                    )

        tokenFormatter tokenBalance =
            NumberFormatter.formatTokenBalanceInNumberSpecWithSymbol tokenBalance chosenAsset.underlying.symbol
    in
    div [ class "bottom-note" ]
        [ div [ class "calculation" ]
            [ span [ class "description" ] [ text descriptionString ]
            , span [] [ text (tokenFormatter balanceToShow) ]
            ]
        ]


faucetAllocateButton : Config -> Model -> CToken -> Html Msg
faucetAllocateButton config { account, network, userLanguage } chosenAsset =
    case ( network, account, config.maybeFauceteer ) of
        ( Just actualNetwork, Acct customerAddress _, Just fauceteerAddress ) ->
            if config.cEtherToken.address /= chosenAsset.contractAddress && actualNetwork /= Network.MainNet then
                a [ class "faucet-link", onClick <| WrappedTokenMsg <| Eth.Token.Web3TransactionMsg (Eth.Token.FauceteerDrip actualNetwork fauceteerAddress chosenAsset.contractAddress chosenAsset.underlying.assetAddress customerAddress) ]
                    [ text (Translations.faucet userLanguage) ]

            else
                text ""

        ( Just actualNetwork, Acct customerAddress _, Nothing ) ->
            if
                (config.cEtherToken.address /= chosenAsset.contractAddress && actualNetwork /= Network.MainNet)
                    && not (actualNetwork == Network.Kovan && (chosenAsset.symbol == "cSAI" || chosenAsset.symbol == "cDAI"))
                    && not (actualNetwork == Network.Ropsten && (chosenAsset.symbol == "cTBTC" || chosenAsset.symbol == "cUSDT"))
            then
                a [ class "faucet-link", onClick <| WrappedTokenMsg <| Eth.Token.Web3TransactionMsg (Eth.Token.FaucetTokenAllocate actualNetwork chosenAsset.contractAddress chosenAsset.underlying.assetAddress customerAddress chosenAsset.underlying.decimals) ]
                    [ text (Translations.faucet userLanguage) ]

            else
                text ""

        _ ->
            text ""



-- HELPERS


getMostRecentAssetPendingTransaction : Config -> PrimaryActionModalState -> Model -> Maybe Transaction
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


interestRateForType : CToken -> PrimaryActionModalState -> CompoundState -> Maybe Decimal
interestRateForType cToken { primaryActionType } compoundState =
    let
        cTokenTnterestRates =
            Balances.getInterestRate compoundState.cTokensMetadata cToken.contractAddress

        maybeCurrentRate =
            case primaryActionType of
                MintAction ->
                    Maybe.map .supplyRate cTokenTnterestRates

                RedeemAction ->
                    Maybe.map .supplyRate cTokenTnterestRates

                BorrowAction ->
                    Maybe.map .borrowRate cTokenTnterestRates

                RepayBorrowAction ->
                    Maybe.map .borrowRate cTokenTnterestRates
    in
    maybeCurrentRate


getAvailableTokensForAction : Maybe Config -> PrimaryActionModalState -> Model -> Decimal
getAvailableTokensForAction maybeConfig { chosenAsset, primaryActionType } { account, compoundState, tokenState, oracleState } =
    let
        underlyingBalances =
            Balances.getUnderlyingBalances compoundState chosenAsset.contractAddress

        ( supplyBalance, borrowBalance ) =
            case account of
                Acct _ _ ->
                    ( Maybe.map .underlyingSupplyBalance underlyingBalances
                        |> Maybe.withDefault Decimal.zero
                    , Maybe.map .underlyingBorrowBalance underlyingBalances
                        |> Maybe.withDefault Decimal.zero
                    )

                _ ->
                    ( Decimal.zero, Decimal.zero )

        ( walletBalance, absoluteMaxWithdraw ) =
            case maybeConfig of
                Just config ->
                    ( Balances.getWalletBalanceSafeEther config account compoundState chosenAsset
                        |> Maybe.withDefault Decimal.zero
                    , Utils.SafeLiquidity.getAbsoluteMaxWithdrawForToken config compoundState tokenState oracleState chosenAsset supplyBalance
                    )

                Nothing ->
                    ( Decimal.zero
                    , Decimal.zero
                    )

        absoluteMaxBorrow =
            Utils.SafeLiquidity.getAbsoluteMaxBorrowForToken compoundState oracleState chosenAsset.underlying
    in
    case primaryActionType of
        -- Wallet Balance
        MintAction ->
            walletBalance

        -- MaxSafeWithdraw
        RedeemAction ->
            absoluteMaxWithdraw

        -- MaxSafeBorrow
        BorrowAction ->
            absoluteMaxBorrow

        -- Min (BorrowBalance+Interest, Wallet Balance)
        RepayBorrowAction ->
            Functions.decimalMin borrowBalance walletBalance


getInputBox : Config -> PrimaryActionModalState -> Model -> Html Msg
getInputBox config ({ primaryActionType, inputValue } as primaryActionModalState) mainModel =
    let
        availableTokenBalance =
            getAvailableTokensForAction (Just config) primaryActionModalState mainModel

        maxValueString =
            "~ " ++ NumberFormatter.formatActionInput availableTokenBalance

        classString =
            case primaryActionType of
                MintAction ->
                    "supply"

                RedeemAction ->
                    "supply"

                BorrowAction ->
                    "borrow"

                RepayBorrowAction ->
                    "borrow"

        enabledValue =
            case inputValue of
                Normal ( inputText, _ ) ->
                    inputText

                Empty ->
                    ""

                Max ->
                    maxValueString

        styleFontSize =
            let
                thresholdCount =
                    10

                inputStringLength =
                    String.length enabledValue

                fontDecrementAmount =
                    "0.2"

                fontMinSize =
                    Decimal.fromString "1.4"
            in
            if inputStringLength > thresholdCount then
                case ( Decimal.fromString "2.625", Decimal.fromString fontDecrementAmount, fontMinSize ) of
                    ( Just startingPoint, Just decrement, Just minSize ) ->
                        let
                            adjustedDecementedValue =
                                Decimal.fromInt (inputStringLength - thresholdCount)
                                    |> Decimal.mul decrement

                            newFontSize =
                                Decimal.sub startingPoint adjustedDecementedValue
                                    |> Functions.decimalMax minSize
                        in
                        Decimal.toString newFontSize ++ "rem"

                    _ ->
                        "2.625rem"

            else
                "2.625rem"
    in
    input
        [ id "action-input-box"
        , type_ "text"
        , class classString
        , style "font-size" styleFontSize
        , placeholder "0"
        , onInput (ForSelf << InputChanged)
        , value enabledValue
        , autocomplete False
        ]
        []


getMaxButton : Translations.Lang -> Config -> PrimaryActionModalState -> Model -> Html Msg
getMaxButton userLanguage config { chosenAsset, primaryActionType } { tokenState, compoundState, oracleState } =
    let
        maxButtonText =
            case primaryActionType of
                MintAction ->
                    text (Translations.max userLanguage)

                RedeemAction ->
                    let
                        supplyBalance =
                            Balances.getUnderlyingBalances compoundState chosenAsset.contractAddress
                                |> Maybe.map .underlyingSupplyBalance
                                |> Maybe.withDefault Decimal.zero

                        maxSafeWithdraw =
                            Utils.SafeLiquidity.getSafeMaxWithdrawForToken config compoundState tokenState oracleState chosenAsset supplyBalance

                        absoluteMaxWithdraw =
                            Utils.SafeLiquidity.getAbsoluteMaxWithdrawForToken config compoundState tokenState oracleState chosenAsset supplyBalance
                    in
                    if Decimal.lt maxSafeWithdraw absoluteMaxWithdraw then
                        div []
                            [ div [] [ text (Translations.eighty_percent userLanguage) ]
                            , div [] [ text (Translations.limit userLanguage) ]
                            ]

                    else
                        text (Translations.max userLanguage)

                BorrowAction ->
                    div []
                        [ div [] [ text (Translations.eighty_percent userLanguage) ]
                        , div [] [ text (Translations.limit userLanguage) ]
                        ]

                RepayBorrowAction ->
                    text (Translations.max userLanguage)
    in
    button [ class "max", onClick (ForSelf (MaxClicked primaryActionType config chosenAsset)) ] [ maxButtonText ]


borrowLimitWithAdjustedInputView : Config -> Maybe Decimal -> PrimaryActionModalState -> Model -> Html Msg
borrowLimitWithAdjustedInputView config maybeEtherUsdPrice ({ chosenAsset, primaryActionType, inputValue } as primaryActionModalState) mainModel =
    let
        maxAvailableTokens =
            getAvailableTokensForAction (Just config) primaryActionModalState mainModel

        preActionBorrowLimitUsd =
            Utils.SafeLiquidity.getCurrentBorrowLimitUsd mainModel.compoundState mainModel.tokenState mainModel.oracleState

        tokenValueUsd =
            Eth.Oracle.getOraclePrice mainModel.oracleState chosenAsset.underlying
                |> Maybe.withDefault Decimal.zero

        ( hasEnteredInput, inputValueDecimal ) =
            case inputValue of
                Normal ( _, Just inputDecimal ) ->
                    ( True, inputDecimal )

                Max ->
                    ( True, maxAvailableTokens )

                _ ->
                    ( False, Decimal.zero )

        collateralFactor =
            mainModel.compoundState.cTokensMetadata
                |> Dict.get (Ethereum.getContractAddressString chosenAsset.contractAddress)
                |> Maybe.map .collateralFactor
                |> Maybe.withDefault Decimal.zero

        inputWithCollateralFactor =
            Decimal.mul inputValueDecimal tokenValueUsd
                |> Decimal.mul collateralFactor

        postActionBorrowLimitUsd =
            case ( Balances.hasEnteredAsset config mainModel.compoundState chosenAsset, primaryActionType ) of
                ( True, MintAction ) ->
                    Decimal.add preActionBorrowLimitUsd inputWithCollateralFactor

                ( True, RedeemAction ) ->
                    Decimal.sub preActionBorrowLimitUsd inputWithCollateralFactor

                _ ->
                    preActionBorrowLimitUsd

        balanceContent =
            if hasEnteredInput then
                [ span [] [ text (formatCurrencyFunc preActionBorrowLimitUsd) ]
                , span [ class "arrow" ] []
                , span [] [ text (formatCurrencyFunc postActionBorrowLimitUsd) ]
                ]

            else
                [ span [] [ text (formatCurrencyFunc preActionBorrowLimitUsd) ] ]

        formatCurrencyFunc usdValue =
            DisplayCurrency.formatDisplayCurrencyInNumberSpec mainModel.preferences.displayCurrency maybeEtherUsdPrice (DisplayCurrency.UsdValue usdValue)
    in
    div [ class "calculation" ]
        [ span [ class "description" ] [ text (Translations.borrow_limit mainModel.userLanguage) ]
        , div [ class "row" ]
            balanceContent
        ]


borrowBalanceWithAdjustedInputView : Config -> Maybe Decimal -> PrimaryActionModalState -> Model -> Html Msg
borrowBalanceWithAdjustedInputView config maybeEtherUsdPrice ({ chosenAsset, primaryActionType, inputValue } as primaryActionModalState) mainModel =
    let
        maxAvailableTokens =
            getAvailableTokensForAction (Just config) primaryActionModalState mainModel

        preActionBorrowBalanceUsd =
            currentBorrowBalanceUsd mainModel

        tokenValueUsd =
            Eth.Oracle.getOraclePrice mainModel.oracleState chosenAsset.underlying
                |> Maybe.withDefault Decimal.zero

        inputValueAsUsd =
            (case inputValue of
                Normal ( _, Just inputDecimal ) ->
                    inputDecimal

                Max ->
                    maxAvailableTokens

                _ ->
                    Decimal.zero
            )
                |> Decimal.mul tokenValueUsd

        postActionBorrowBalanceUsd =
            if primaryActionType == BorrowAction then
                Decimal.add preActionBorrowBalanceUsd inputValueAsUsd

            else
                Decimal.sub preActionBorrowBalanceUsd inputValueAsUsd

        balanceContent =
            if Decimal.eq preActionBorrowBalanceUsd postActionBorrowBalanceUsd then
                [ span [] [ text (formatCurrencyFunc preActionBorrowBalanceUsd) ] ]

            else
                [ span [] [ text (formatCurrencyFunc preActionBorrowBalanceUsd) ]
                , span [ class "arrow borrow" ] []
                , span [] [ text (formatCurrencyFunc postActionBorrowBalanceUsd) ]
                ]

        formatCurrencyFunc usdValue =
            DisplayCurrency.formatDisplayCurrencyInNumberSpec mainModel.preferences.displayCurrency maybeEtherUsdPrice (DisplayCurrency.UsdValue usdValue)
    in
    div [ class "calculation" ]
        [ span [ class "description" ] [ text (Translations.borrow_balance mainModel.userLanguage) ]
        , div [ class "row" ]
            balanceContent
        ]


borrowLimitUsedWithAdjustedInputView : Config -> PrimaryActionModalState -> Model -> Html Msg
borrowLimitUsedWithAdjustedInputView config ({ chosenAsset, primaryActionType, inputValue } as primaryActionModalState) mainModel =
    let
        maxAvailableTokens =
            getAvailableTokensForAction (Just config) primaryActionModalState mainModel

        preActionBorrowBalanceUsd =
            currentBorrowBalanceUsd mainModel

        tokenValueUsd =
            Eth.Oracle.getOraclePrice mainModel.oracleState chosenAsset.underlying
                |> Maybe.withDefault Decimal.zero

        inputValueAsUsd =
            (case inputValue of
                Normal ( _, Just inputDecimal ) ->
                    inputDecimal

                Max ->
                    maxAvailableTokens

                _ ->
                    Decimal.zero
            )
                |> Decimal.mul tokenValueUsd

        currentBorrowLimitUsd =
            Utils.SafeLiquidity.getCurrentBorrowLimitUsd mainModel.compoundState mainModel.tokenState mainModel.oracleState

        ( postActionBorrowBalanceUsd, postActionBorrowLimitUsd ) =
            let
                collateralFactor =
                    mainModel.compoundState.cTokensMetadata
                        |> Dict.get (Ethereum.getContractAddressString chosenAsset.contractAddress)
                        |> Maybe.map .collateralFactor
                        |> Maybe.withDefault Decimal.zero

                inputWithCollateralFactor =
                    if Balances.hasEnteredAsset config mainModel.compoundState chosenAsset then
                        inputValueAsUsd
                            |> Decimal.mul collateralFactor

                    else
                        Decimal.zero
            in
            case primaryActionType of
                MintAction ->
                    ( preActionBorrowBalanceUsd
                    , Decimal.add currentBorrowLimitUsd inputWithCollateralFactor
                    )

                RedeemAction ->
                    ( preActionBorrowBalanceUsd
                    , Decimal.sub currentBorrowLimitUsd inputWithCollateralFactor
                        |> Functions.decimalMax Decimal.zero
                    )

                BorrowAction ->
                    ( Decimal.add preActionBorrowBalanceUsd inputValueAsUsd
                    , currentBorrowLimitUsd
                    )

                RepayBorrowAction ->
                    ( Decimal.sub preActionBorrowBalanceUsd inputValueAsUsd
                        |> Functions.decimalMax Decimal.zero
                    , currentBorrowLimitUsd
                    )

        currentBorrowBalancePercentOfLimit =
            (case Decimal.fastdiv preActionBorrowBalanceUsd currentBorrowLimitUsd of
                Just percent ->
                    percent

                Nothing ->
                    Decimal.zero
            )
                |> Decimal.mul (Decimal.fromInt 100)

        newBorrowBalancePercentOfLimit =
            (case Decimal.fastdiv postActionBorrowBalanceUsd postActionBorrowLimitUsd of
                Just percent ->
                    percent

                Nothing ->
                    if Decimal.eq inputValueAsUsd Decimal.zero then
                        Decimal.zero

                    else if primaryActionType == RedeemAction then
                        Decimal.fromInt 100

                    else
                        Decimal.zero
            )
                |> Decimal.mul (Decimal.fromInt 100)
                |> Functions.decimalMin (Decimal.fromInt 100)
                |> (\newPercent ->
                        if Decimal.lt newPercent Decimal.zero then
                            Decimal.zero

                        else
                            newPercent
                   )

        balanceContent =
            let
                arrowExtraClass =
                    if primaryActionType == MintAction || primaryActionType == RedeemAction then
                        " supply"

                    else
                        " borrow"
            in
            if Decimal.eq currentBorrowBalancePercentOfLimit newBorrowBalancePercentOfLimit then
                [ span [] [ text (formatRate currentBorrowBalancePercentOfLimit) ] ]

            else
                [ span [] [ text (formatRate currentBorrowBalancePercentOfLimit) ]
                , span [ class ("arrow" ++ arrowExtraClass) ] []
                , span [] [ text (formatRate newBorrowBalancePercentOfLimit) ]
                ]
    in
    div [ class "calculation" ]
        [ span [ class "description" ] [ text (Translations.borrow_limit_used mainModel.userLanguage) ]
        , div [ class "row" ]
            balanceContent
        ]


currentBorrowBalanceUsd : Model -> Decimal
currentBorrowBalanceUsd { compoundState, oracleState, tokenState } =
    let
        cTokens =
            Dict.values tokenState.cTokens

        balanceTotalsUsd =
            Balances.getUnderlyingTotalsInUsd compoundState cTokens oracleState
    in
    balanceTotalsUsd.totalBorrow
