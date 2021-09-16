module DappInterface.ClaimCompModal exposing (..)

import CompoundComponents.DisplayCurrency as DisplayCurrency
import CompoundComponents.Eth.Ethereum as Ethereum exposing (Account(..), CustomerAddress, getContractAddressString)
import CompoundComponents.Eth.Network exposing (Network(..))
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (HrefLinkType(..), class, disabled, href, id, onClickStopPropagation, onError, placeholder, src, style, target, type_)
import CompoundComponents.Utils.NumberFormatter as NumberFormatter exposing (formatToDecimalPlaces)
import DappInterface.CommonViews exposing (compOrVoteBalanceSpan)
import Decimal exposing (Decimal)
import Dict
import Eth.Compound exposing (CompoundState)
import Eth.Config exposing (Config)
import Eth.Governance
    exposing
        ( GovernanceMsg(..)
        , GovernanceState
        , getCompAccruedBalance
        , getCompoundGovernanceTokenBalance
        )
import Eth.Oracle exposing (OracleState)
import Eth.Token exposing (TokenState)
import Eth.Transaction exposing (Transaction, TransactionMsg, TransactionState, TransactionStatus(..))
import Html exposing (Html, button, div, h5, img, label, p, section, span, text)
import Html.Events exposing (onClick)
import Maybe
import Preferences exposing (Preferences)
import Strings.Translations as Translations


type ClaimCompModalStep
    = AwaitingConfirmClaimTransaction
    | AwaitingClaimTransactionMined
    | ClaimModalNotShown
    | ShowClaimModal


type InternalMsg
    = ClaimCompClicked Decimal
    | DismissClaimCompModal


type Msg
    = ForSelf InternalMsg
    | WrappedGovernanceMsg GovernanceMsg


type alias TranslationDictionary msg =
    { onInternalMessage : InternalMsg -> msg
    , onWrappedGovernanceMessage : GovernanceMsg -> msg
    }


type alias Translator msg =
    Msg -> msg


translator : TranslationDictionary msg -> Translator msg
translator { onInternalMessage, onWrappedGovernanceMessage } msg =
    case msg of
        ForSelf internal ->
            onInternalMessage internal

        WrappedGovernanceMsg governanceMsg ->
            onWrappedGovernanceMessage governanceMsg


type alias Model =
    { step : ClaimCompModalStep
    , compEstimate : Maybe Decimal
    }


init : Model
init =
    { step = ClaimModalNotShown
    , compEstimate = Nothing
    }


updateClaimModalStep : ClaimCompModalStep -> Model -> Model
updateClaimModalStep newStep oldState =
    { oldState | step = newStep }


handleTransactionUpdate : Maybe Config -> Maybe Network -> Account -> TransactionState -> TransactionMsg -> Model -> Model
handleTransactionUpdate maybeConfig maybeNetwork account transactionState transactionMsg model =
    case ( maybeConfig, maybeNetwork, account ) of
        ( Just config, Just actualNetwork, Acct customer _ ) ->
            case transactionMsg of
                Eth.Transaction.NewTransaction transaction ->
                    if
                        (transaction.customer == customer)
                            && (transaction.network == actualNetwork)
                            && (transaction.function == "claimComp")
                            && (transaction.status == Eth.Transaction.Pending)
                    then
                        let
                            updatedModel =
                                if model.step == AwaitingConfirmClaimTransaction then
                                    updateClaimModalStep AwaitingClaimTransactionMined model

                                else
                                    model
                        in
                        updatedModel

                    else
                        model

                _ ->
                    model

        _ ->
            model


handleGovernanceMsgUpdate : GovernanceMsg -> Model -> Model
handleGovernanceMsgUpdate governanceMsg model =
    case governanceMsg of
        Eth.Governance.ClaimComp _ _ _ ->
            let
                updatedModel =
                    if model.step == AwaitingConfirmClaimTransaction then
                        updateClaimModalStep AwaitingClaimTransactionMined model

                    else
                        model
            in
            updatedModel

        _ ->
            model


getPendingClaimCompTransaction : Network -> CustomerAddress -> TransactionState -> Maybe Transaction
getPendingClaimCompTransaction network customer transactionState =
    transactionState.transactions
        |> List.filter
            (\transaction ->
                (transaction.customer == customer)
                    && (transaction.network == network)
                    && (transaction.function == "claimComp")
                    && (transaction.status == Eth.Transaction.Pending)
            )
        |> List.head


update : InternalMsg -> Maybe Config -> Account -> CompoundState -> Model -> ( Model, Cmd Msg )
update internalMsg maybeConfig account compoundState model =
    case internalMsg of
        ClaimCompClicked estimateComp ->
            case ( maybeConfig, account ) of
                ( Just config, Acct customer _ ) ->
                    let
                        marketsWithBalances =
                            Dict.toList compoundState.balances
                                |> List.filterMap
                                    (\( cTokenAddress, balances ) ->
                                        if Decimal.gt balances.cTokenWalletBalance Decimal.zero || Decimal.gt balances.underlyingBorrowBalance Decimal.zero then
                                            Just cTokenAddress

                                        else
                                            Nothing
                                    )

                        markets =
                            -- Handle when a user has unclaimed COMP but no longer has any balances
                            if List.length marketsWithBalances == 0 then
                                [ getContractAddressString config.cEtherToken.address ]

                            else
                                marketsWithBalances
                    in
                    ( { model
                        | step = AwaitingConfirmClaimTransaction
                        , compEstimate = Just estimateComp
                      }
                    , Cmd.map WrappedGovernanceMsg (Eth.Governance.askClaimComp config.comptroller customer markets)
                    )

                _ ->
                    ( model, Cmd.none )

        DismissClaimCompModal ->
            ( { model
                | step = ClaimModalNotShown
                , compEstimate = Nothing
              }
            , Cmd.none
            )


view : Translations.Lang -> Maybe Config -> Maybe Network -> Account -> TokenState -> OracleState -> TransactionState -> GovernanceState -> Preferences -> Model -> Html Msg
view userLanguage maybeConfig maybeNetwork account tokenState oracleState transactionState governanceState preferences model =
    case ( account, maybeNetwork ) of
        ( Acct accountAddress _, Just network ) ->
            let
                maybeEtherUsdPrice =
                    maybeConfig
                        |> Maybe.andThen (\config -> Eth.Oracle.getEtherPrice config tokenState oracleState)

                compPrice =
                    tokenState.cTokens
                        |> Dict.values
                        |> List.filter (\{ symbol } -> symbol == "cCOMP")
                        |> List.head
                        |> Maybe.andThen (\{ underlying } -> Eth.Oracle.getOraclePrice oracleState underlying)
                        |> Maybe.withDefault Decimal.zero

                formatCurrencyFunc usdValue =
                    DisplayCurrency.formatDisplayCurrencyInNumberSpec preferences.displayCurrency maybeEtherUsdPrice (DisplayCurrency.UsdValue usdValue)
            in
            case model.step of
                ClaimModalNotShown ->
                    text ""

                AwaitingConfirmClaimTransaction ->
                    let
                        compEarned =
                            getCompAccruedBalance accountAddress governanceState
                                |> Maybe.withDefault Decimal.zero

                        totalValue =
                            Decimal.mul compEarned compPrice
                    in
                    standardModalView (Translations.confirm_transaction userLanguage)
                        [ div [ class "connecting-ring" ] [ div [] [] ]
                        , h5 [ class "claim-comp__modal__body__balance" ] [ text (Translations.claiming_comp userLanguage (formatToDecimalPlaces 8 False compEarned)) ]
                        , label [ class "claim-comp__modal__body__value" ] [ text (formatCurrencyFunc totalValue) ]
                        , label [ class "claim-comp__modal__body__help" ] [ text (Translations.confirm_the_transaction userLanguage) ]
                        ]

                AwaitingClaimTransactionMined ->
                    let
                        compEarned =
                            getCompAccruedBalance accountAddress governanceState
                                |> Maybe.withDefault Decimal.zero

                        totalValue =
                            Decimal.mul compEarned compPrice

                        maybePendingClaimCompTransaction =
                            getPendingClaimCompTransaction network accountAddress transactionState

                        greenCircle =
                            if maybePendingClaimCompTransaction == Nothing then
                                div [ class "claim-comp__modal__body__success" ] [ img [ src "images/check-big.svg" ] [] ]

                            else
                                div [ class "connecting-ring" ] [ div [] [] ]

                        etherscanButton =
                            maybePendingClaimCompTransaction
                                |> Maybe.map
                                    (\transaction ->
                                        Ethereum.etherscanLink
                                            maybeNetwork
                                            (Ethereum.TransactionHash transaction.trxHash)
                                            [ class "submit-button button main claim-comp__modal__body__button" ]
                                            [ text (Translations.view_on_etherscan userLanguage) ]
                                    )
                                |> Maybe.withDefault (text "")
                    in
                    standardModalView (Translations.transaction_pending userLanguage)
                        [ greenCircle
                        , h5 [ class "claim-comp__modal__body__balance" ] [ text (Translations.claiming_comp userLanguage (formatToDecimalPlaces 8 False compEarned)) ]
                        , label [ class "claim-comp__modal__body__value" ] [ text (formatCurrencyFunc totalValue) ]
                        , label [ class "claim-comp__modal__body__help" ] [ text (Translations.transaction_broadcast_no_estimation userLanguage) ]
                        , etherscanButton
                        ]

                ShowClaimModal ->
                    let
                        compEarned =
                            getCompAccruedBalance accountAddress governanceState
                                |> Maybe.withDefault Decimal.zero

                        compBalance =
                            getCompoundGovernanceTokenBalance accountAddress governanceState
                                |> Maybe.withDefault Decimal.zero

                        totalComp =
                            Decimal.add compEarned compBalance

                        totalValue =
                            Decimal.mul totalComp compPrice

                        buttonView =
                            if Decimal.gt compEarned Decimal.zero then
                                button [ class "submit-button button main claim-comp__modal__body__button", onClick (ForSelf (ClaimCompClicked compEarned)) ]
                                    [ text (Translations.claim_comp userLanguage (formatToDecimalPlaces 4 False compEarned)) ]

                            else
                                button [ class "submit-button button main claim-comp__modal__body__button", disabled True ]
                                    [ text (Translations.nothing_to_claim userLanguage) ]
                    in
                    standardModalView (Translations.votes_page_comp_balance userLanguage)
                        [ div [ class "icon icon--COMP" ] []
                        , h5 [ class "claim-comp__modal__body__balance" ] [ text (formatToDecimalPlaces 8 False totalComp) ]
                        , label [ class "claim-comp__modal__body__value" ] [ text (formatCurrencyFunc totalValue) ]
                        , div [ class "claim-comp__modal__body__rows" ]
                            [ div [ class "claim-comp__modal__body__rows__row" ]
                                [ label [ class "claim-comp__modal__body__rows__row__label" ] [ text (Translations.wallet_balance userLanguage) ]
                                , label [ class "claim-comp__modal__body__rows__row__value" ] [ text (formatToDecimalPlaces 4 False compBalance) ]
                                ]
                            , div [ class "claim-comp__modal__body__rows__row" ]
                                [ label [ class "claim-comp__modal__body__rows__row__label" ] [ text (Translations.unclaimed_balance userLanguage) ]
                                , label [ class "claim-comp__modal__body__rows__row__value" ] [ text (formatToDecimalPlaces 4 False compEarned) ]
                                ]
                            , div [ class "claim-comp__modal__body__rows__row" ]
                                [ label [ class "claim-comp__modal__body__rows__row__label" ] [ text (Translations.price userLanguage) ]
                                , label [ class "claim-comp__modal__body__rows__row__value" ] [ text (formatCurrencyFunc compPrice) ]
                                ]
                            ]
                        , buttonView
                        ]

        _ ->
            text ""


standardModalView : String -> List (Html Msg) -> Html Msg
standardModalView headerText body =
    div [ class "modal claim-comp__modal" ]
        [ div [ class "cover active", onClick (ForSelf DismissClaimCompModal) ] []
        , div [ class "legacy-panel" ]
            [ div [ class "header" ]
                [ div [ class "title" ]
                    [ span [] [ text headerText ]
                    ]
                , div [ class "close-x" ]
                    [ button [ onClick (ForSelf DismissClaimCompModal) ]
                        []
                    ]
                ]
            , div [ class "claim-comp__modal__body" ] body
            ]
        ]
