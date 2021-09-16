module DappInterface.Container exposing
    ( InternalMsg(..)
    , ParentMsg(..)
    , Translator
    , handleAccountSwitch
    , handleTimeTick
    , init
    , translator
    , update
    , view
    )

import Balances
import CompoundComponents.DisplayCurrency as DisplayCurrency exposing (DisplayCurrency(..))
import CompoundComponents.Eth.Ethereum exposing (Account(..), AssetAddress(..), ContractAddress(..), CustomerAddress(..))
import CompoundComponents.Eth.Network as Network exposing (Network)
import CompoundComponents.Eth.ProviderInfo as EthProviderInfo
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (HrefLinkType(..), class, id, style)
import CompoundComponents.Utils.Time
import DappInterface.BorrowingPane as BorrowingPane
import DappInterface.CollateralPane as CollateralPane
import DappInterface.MainModel
    exposing
        ( BorrowingContainerState
        , BorrowingRisk(..)
        , Model
        , MouseEvent
        , getConfig
        , getCurrentConfig
        )
import DappInterface.OverviewHeader as OverviewHeader
import DappInterface.Page exposing (Page(..))
import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Eth.Compound exposing (CompoundMsg)
import Eth.Config exposing (Config)
import Eth.Oracle
import Eth.Token exposing (CToken)
import Html exposing (Html, b, div, p, section, span, text)
import Preferences exposing (PreferencesMsg(..))
import Strings.Translations as Translations
import Time
import Utils.BrowserInfo


type ParentMsg
    = AccountAddressClicked
    | CompButtonClicked
    | MintActionRequest (Maybe CToken)
    | WithdrawActionRequest
    | BorrowActionRequest (Maybe CToken)
    | PayBorrowActionRequest
    | UseAsCollateralToggleClicked CToken


type InternalMsg
    = BorrowingLimitMouseEnter MouseEvent
    | BorrowingLimitMouseLeave MouseEvent
    | NetAPYMouseEnter MouseEvent
    | NetAPYMouseLeave MouseEvent


type OutMsg
    = WrappedCompoundMsg CompoundMsg
    | WrappedPreferencesMsg PreferencesMsg


type Msg
    = ForSelf InternalMsg
    | ForParent ParentMsg
    | ForEthControllers OutMsg


type alias TranslationDictionary msg =
    { onContainerParentMsg : ParentMsg -> msg
    , onInternalMsg : InternalMsg -> msg
    , onWrappedCompoundMsg : CompoundMsg -> msg
    , onWrappedPreferencesMsg : PreferencesMsg -> msg
    }


type alias Translator msg =
    Msg -> msg


translator : TranslationDictionary msg -> Translator msg
translator { onContainerParentMsg, onInternalMsg, onWrappedCompoundMsg, onWrappedPreferencesMsg } msg =
    case msg of
        ForSelf internalMsg ->
            onInternalMsg internalMsg

        ForParent containerParentMsg ->
            onContainerParentMsg containerParentMsg

        ForEthControllers (WrappedCompoundMsg compoundMsg) ->
            onWrappedCompoundMsg compoundMsg

        ForEthControllers (WrappedPreferencesMsg preferencesMsg) ->
            onWrappedPreferencesMsg preferencesMsg


overviewHeaderMsgMapper : OverviewHeader.ParentMsg -> Msg
overviewHeaderMsgMapper overviewHeaderMsg =
    case overviewHeaderMsg of
        OverviewHeader.BorrowingLimitMouseEnter mouseEvent ->
            ForSelf (BorrowingLimitMouseEnter mouseEvent)

        OverviewHeader.BorrowingLimitMouseLeave mouseEvent ->
            ForSelf (BorrowingLimitMouseLeave mouseEvent)

        OverviewHeader.NetAPYMouseEnter mouseEvent ->
            ForSelf (NetAPYMouseEnter mouseEvent)

        OverviewHeader.NetAPYMouseLeave mouseEvent ->
            ForSelf (NetAPYMouseLeave mouseEvent)


collateralPaneMsgMapper : CollateralPane.Msg -> Msg
collateralPaneMsgMapper collateralPaneMsg =
    case collateralPaneMsg of
        CollateralPane.ForCompoundController compoundMsg ->
            ForEthControllers (WrappedCompoundMsg compoundMsg)

        CollateralPane.ForPreferences preferencesMsg ->
            ForEthControllers (WrappedPreferencesMsg preferencesMsg)

        CollateralPane.ForParent (CollateralPane.CollateralAssetClicked cToken) ->
            ForParent (MintActionRequest (Just cToken))

        CollateralPane.ForParent (CollateralPane.UseAsCollateralToggleClicked cToken) ->
            ForParent (UseAsCollateralToggleClicked cToken)


borrowingPaneMsgMapper : BorrowingPane.Msg -> Msg
borrowingPaneMsgMapper borrowingPaneMsg =
    case borrowingPaneMsg of
        BorrowingPane.ForParent (BorrowingPane.BorrowedAssetClicked cToken) ->
            ForParent (BorrowActionRequest (Just cToken))

        BorrowingPane.ForPreferences preferencesMsg ->
            ForEthControllers (WrappedPreferencesMsg preferencesMsg)


init : BorrowingContainerState
init =
    { maybeBorrowingLimitPopoverPosition = Nothing
    , showNetAPYHoverView = False
    , maybePreviousAnimatedBalances = Nothing
    , maybeCurrentAnimatedBalances = Nothing
    , maybeLastBalancesChangeTimestamp = Nothing
    , shouldRemoveSlotMachineActiveClass = False
    }


update : InternalMsg -> BorrowingContainerState -> BorrowingContainerState
update msg borrowingContainerModel =
    case msg of
        BorrowingLimitMouseEnter event ->
            let
                -- We'll keep the x client position but take the target y position
                -- So the popover has a consistent x position at least.
                desiredPosition =
                    { x = event.clientPos.x
                    , y = event.targetPos.y
                    }
            in
            { borrowingContainerModel | maybeBorrowingLimitPopoverPosition = Just desiredPosition }

        BorrowingLimitMouseLeave _ ->
            { borrowingContainerModel | maybeBorrowingLimitPopoverPosition = Nothing }

        NetAPYMouseEnter _ ->
            { borrowingContainerModel | showNetAPYHoverView = True }

        NetAPYMouseLeave _ ->
            { borrowingContainerModel | showNetAPYHoverView = False }


handleAccountSwitch : BorrowingContainerState -> BorrowingContainerState
handleAccountSwitch borrowingContainerModel =
    { borrowingContainerModel
        | maybePreviousAnimatedBalances = Nothing
        , maybeCurrentAnimatedBalances = Nothing
        , maybeLastBalancesChangeTimestamp = Nothing
        , shouldRemoveSlotMachineActiveClass = False
    }



-- This function is meant to be called every second and keeps track of 2 things:
-- 1. Keeps track of previous and current balances and only updates previous balances
--    after 2 seconds have passed. Enough time to start the slot machine animation.
-- 2. Also sets a model flag to remove the 'active' class from the animation as that
--    is a way to get the animation to restart itself... Prety complicated to restart one...


handleTimeTick : BorrowingContainerState -> Model -> Time.Posix -> BorrowingContainerState
handleTimeTick oldBorrowingContainerModel mainModel currTime =
    let
        cTokens =
            Dict.values mainModel.tokenState.cTokens

        newBalanceTotalsUsd =
            Balances.getUnderlyingTotalsInUsd mainModel.compoundState cTokens mainModel.oracleState

        shouldUpdate =
            case oldBorrowingContainerModel.maybeCurrentAnimatedBalances of
                Nothing ->
                    True

                Just ( oldSupplyBalanceUsd, oldBorrowBalanceUsd ) ->
                    if Decimal.eq oldSupplyBalanceUsd newBalanceTotalsUsd.totalSupply && Decimal.eq oldBorrowBalanceUsd newBalanceTotalsUsd.totalBorrow then
                        False

                    else
                        True

        currAnimatedBalancesUpdate =
            if shouldUpdate then
                -- This has the effect of triggering a new slot machine animation.
                { oldBorrowingContainerModel
                    | maybeCurrentAnimatedBalances = Just ( newBalanceTotalsUsd.totalSupply, newBalanceTotalsUsd.totalBorrow )
                    , maybeLastBalancesChangeTimestamp = Just currTime
                    , shouldRemoveSlotMachineActiveClass = False
                }

            else
                oldBorrowingContainerModel
    in
    case currAnimatedBalancesUpdate.maybeLastBalancesChangeTimestamp of
        Just lastChangeTimestamp ->
            if CompoundComponents.Utils.Time.differenceInSeconds currTime lastChangeTimestamp > 2 then
                -- This is getting the model to begin the next slot machine animation.
                { currAnimatedBalancesUpdate
                    | maybePreviousAnimatedBalances = currAnimatedBalancesUpdate.maybeCurrentAnimatedBalances
                    , shouldRemoveSlotMachineActiveClass = True
                }

            else
                currAnimatedBalancesUpdate

        Nothing ->
            currAnimatedBalancesUpdate


invalidNetwork : Maybe Network -> Dict String Config -> Bool
invalidNetwork maybeNetwork configs =
    case maybeNetwork of
        Just network ->
            case getConfig configs network of
                Just _ ->
                    False

                Nothing ->
                    True

        --We have decided that the NOP state is still a valid network...
        Nothing ->
            False


testNetwork : Maybe Network -> Maybe String
testNetwork maybeNetwork =
    case maybeNetwork of
        Just Network.MainNet ->
            Nothing

        Just network ->
            Just (Network.networkName network)

        _ ->
            Nothing


view : Model -> Html Msg
view mainModel =
    let
        showingAlertClass : Model -> String
        showingAlertClass ({ connectedEthWalletModel, account, browserType, network } as model) =
            if not (browserType == Utils.BrowserInfo.Desktop || EthProviderInfo.hasProvider connectedEthWalletModel.providerType) then
                " alert-small"

            else if invalidNetwork model.network model.configs then
                " alert-small"

            else
                case account of
                    Acct (Customer _) maybeBalance ->
                        let
                            hasZeroEthBalance =
                                case maybeBalance of
                                    Just balance ->
                                        Decimal.eq balance Decimal.zero

                                    Nothing ->
                                        False
                        in
                        case ( testNetwork network, hasZeroEthBalance ) of
                            ( Just _, True ) ->
                                " alert-large"

                            ( Just _, _ ) ->
                                " alert-small"

                            _ ->
                                ""

                    _ ->
                        ""

        maybeConfig =
            getCurrentConfig mainModel

        maybeEtherUsdPrice =
            maybeConfig
                |> Maybe.andThen (\config -> Eth.Oracle.getEtherPrice config mainModel.tokenState mainModel.oracleState)
    in
    span []
        [ Html.map overviewHeaderMsgMapper (OverviewHeader.view maybeConfig maybeEtherUsdPrice mainModel)
        , section []
            [ div [ class "container-large" ]
                [ div [ id "interface-container", class ("row" ++ showingAlertClass mainModel) ]
                    [ Html.map collateralPaneMsgMapper (CollateralPane.view maybeConfig maybeEtherUsdPrice mainModel)
                    , Html.map borrowingPaneMsgMapper (BorrowingPane.view maybeConfig maybeEtherUsdPrice mainModel)
                    ]
                ]
            ]
        , borrowingLimitPopoverView maybeEtherUsdPrice mainModel
        ]


borrowingLimitPopoverView : Maybe Decimal -> Model -> Html Msg
borrowingLimitPopoverView maybeEtherUsdPrice ({ borrowingContainerState, compoundState, oracleState, tokenState, preferences, userLanguage } as mainModel) =
    let
        px f =
            String.fromFloat f ++ "px"

        cTokens =
            Dict.values tokenState.cTokens

        balanceTotalsUsd =
            Balances.getUnderlyingTotalsInUsd compoundState cTokens oracleState

        hasAnyBorrowBalance =
            Decimal.gt balanceTotalsUsd.totalBorrow Decimal.zero

        accountLiquidityUsd =
            compoundState.maybeAccountLiquidityUsd
                |> Maybe.withDefault Decimal.zero

        --Total Borrow Limit is AccountLiquidity + TotalBorrowBalance
        totalBorrowLimitUsd =
            Decimal.add balanceTotalsUsd.totalBorrow accountLiquidityUsd

        formatCurrencyFunc =
            DisplayCurrency.formatDisplayCurrencyInNumberSpec preferences.displayCurrency maybeEtherUsdPrice
    in
    case borrowingContainerState.maybeBorrowingLimitPopoverPosition of
        Just borrowingLimitPopoverPosition ->
            let
                xPos =
                    toFloat borrowingLimitPopoverPosition.x

                yPos =
                    toFloat borrowingLimitPopoverPosition.y

                collateralBalanceDeclineAmountString =
                    Decimal.sub totalBorrowLimitUsd balanceTotalsUsd.totalBorrow
                        |> DisplayCurrency.UsdValue
                        |> formatCurrencyFunc

                desciptionParagraph =
                    if hasAnyBorrowBalance then
                        span []
                            [ p [] [ text (Translations.liquidation_occurs_if_decline_part1 userLanguage) ]
                            , b []
                                [ p []
                                    [ text (Translations.liquidation_occurs_if_decline_part2 userLanguage collateralBalanceDeclineAmountString) ]
                                ]
                            ]

                    else
                        span []
                            [ p [] [ text (Translations.liquidation_only_for_borrows userLanguage) ]
                            ]
            in
            div
                [ class "popover"
                , style "left" <| px (xPos - 120)
                , style "top" <| px (yPos + 12)
                , style "display" "fixed"
                ]
                [ desciptionParagraph ]

        Nothing ->
            text ""
