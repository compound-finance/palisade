port module DappInterface.Vote exposing
    ( DelegateModalStep(..)
    , InternalMsg(..)
    , Model
    , Msg(..)
    , Translator
    , emptyState
    , getVoteDashboardData
    , handleShouldLoadCurrentDelegatee
    , handleTokenUpdate
    , handleTransactionUpdate
    , init
    , subscriptions
    , translator
    , update
    , view
    )

import CompoundApi.Governance.AccountService.Decoders exposing (governanceAccountResponseDecoder, governanceAccountSearchResponseDecoder)
import CompoundApi.Governance.AccountService.Models exposing (GovernanceAccountResponse, GovernanceAccountSearchResponse)
import CompoundApi.Governance.AccountService.Urls exposing (governanceAccountRequestUrl, governanceAccountSearchRequestUrl)
import CompoundApi.Governance.Common.Models exposing (CompAccount)
import CompoundApi.Governance.ProposalService.Decoders exposing (proposalWithDetailDecoder)
import CompoundApi.Governance.ProposalService.Models as ProposalServiceModels exposing (ProposalState, ProposalStateEnum(..), ProposalWithDetail)
import CompoundComponents.Console as Console
import CompoundComponents.DisplayCurrency exposing (DisplayCurrency(..))
import CompoundComponents.Eth.Decoders exposing (decimal)
import CompoundComponents.Eth.Ethereum as Ethereum exposing (Account(..), AssetAddress(..), ContractAddress(..), CustomerAddress(..), EtherscanLinkValue(..), getContractAddressString, getCustomerAddressString, isValidAddress, shortenedAddressString, zeroAddress)
import CompoundComponents.Eth.Ledger exposing (intToLedgerAccount)
import CompoundComponents.Eth.Network exposing (Network(..), networkName)
import CompoundComponents.Functions exposing (handleError)
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (HrefLinkType(..), class, href, id, onClickStopPropagation, placeholder, src, style, target, type_)
import CompoundComponents.Utils.Markup exposing (disabled)
import CompoundComponents.Utils.NumberFormatter exposing (formatPercentage, formatPercentageToNearestWhole, formatToDecimalPlaces, formatTokenBalance)
import CompoundComponents.Utils.Time
import DappInterface.CommonViews exposing (compOrVoteBalanceSpan)
import DappInterface.Page exposing (Page(..), getHrefUrl)
import Debounce exposing (Debounce)
import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Eth.Config exposing (Config)
import Eth.Governance
    exposing
        ( DelegatedAddress(..)
        , GovernanceMsg(..)
        , GovernanceState
        , getCompoundGovernanceTokenBalance
        , getCurrentVotes
        , getDelegatedAddress
        , getDelegatedAddressString
        )
import Eth.Token exposing (TokenMsg, TokenState)
import Eth.Transaction exposing (Transaction, TransactionMsg, TransactionState, TransactionStatus(..))
import FromLanding.Governance.CommonViews exposing (loadingProposalRow, proposalContentView)
import FromLanding.Page exposing (DocsSubPage(..), LandingPage(..), landingUrlForPage)
import Html exposing (Html, a, button, div, h4, img, input, label, p, section, span, text, textarea)
import Html.Attributes exposing (value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Http
import Json.Decode exposing (bool, dict, field, int, list, map3, map5, string)
import Port exposing (imageError)
import Strings.Translations as Translations
import Task
import Time
import Url.Builder as UrlBuilder
import Utils.GovProfileImageHelper as ProfileImageHelper
import Utils.GovernanceHelper as GovernanceHelper
import Utils.Http


desiredPageSize : Int
desiredPageSize =
    5


type alias VoterReceipt =
    { support : Int
    , votes : Decimal
    , proposal_id : Int
    }


type alias VoterDashboardData =
    { proposals : List ProposalWithDetail
    , proposalVoteReceipts : Dict String VoterReceipt
    , priorVotes : Dict String Decimal
    , network : String
    , voter : String
    }


type DelegateModalStep
    = SelectDelegationType
    | DelegateVoting
    | DelegateTransaction
    | NotShown


type alias DelegateModalState =
    { step : DelegateModalStep
    , steps : List DelegateModalStep
    , maybeNewDelegateAddress : Maybe String
    , maybeNewDelegateAddressIsValid : Maybe Bool
    , maybeSelectedDelegate : Maybe CompAccount
    , suggestedDelegates : List CompAccount
    , topDelegates : List CompAccount
    , addressDebounce : Debounce String
    , inputFocused : Bool
    }


type VoteModalState
    = SelectVoteType ProposalWithDetail (Maybe VoteType) String
    | ConfirmVoteType ProposalWithDetail VoteType String
    | VoteModalNotShown


type VoteType
    = For
    | Against
    | Abstain


voteTypeAsInt : VoteType -> Int
voteTypeAsInt voteType =
    case voteType of
        For ->
            1

        Against ->
            0

        Abstain ->
            2


intToMaybeVoteType : Int -> Maybe VoteType
intToMaybeVoteType int =
    case int of
        0 ->
            Just Against

        1 ->
            Just For

        2 ->
            Just Abstain

        _ ->
            Nothing


type ApproveCAPStep
    = AwaitingConfirmApproveCAPTransaction
    | AwaitingApproveCAPTransactionMined
    | ApproveCAPModalNotShown


type alias Model =
    { proposals : Maybe (List ProposalWithDetail)
    , proposalVoteReceipts : Dict String VoterReceipt
    , priorVotes : Dict String Decimal
    , delegateModalState : DelegateModalState
    , voteModalState : VoteModalState
    , currentProposalsPageNumber : Int
    , requestCurrentDelegatee : Bool
    , currentDelegateeAcccount : Maybe CompAccount
    , approveCAPState : ApproveCAPStep
    , errors : List String
    }


type InternalMsg
    = SetVoteModal VoteModalState
    | SetDelegateModal DelegateModalStep
    | VoteDashboardDataResult VoterDashboardData
    | ProposalsPagerPrevious
    | ProposalsPagerNext
    | SetNewDelegateAddress String
    | SelectDelegate CompAccount
    | SetProposalsPager Int
    | SearchDelegates String
    | TopDelegatesResponse (Result Http.Error GovernanceAccountResponse)
    | SearchDelegatesResponse (Result Http.Error GovernanceAccountSearchResponse)
    | DelegateInputFocused
    | DelegateInputBlurred
    | DebounceMsg Debounce.Msg
    | ImageError String String
    | DelegateTo CustomerAddress
    | CurrentDelegateeResponse (Result Http.Error GovernanceAccountResponse)
    | DismissApproveCAPModal
    | Error String


type Msg
    = ForSelf InternalMsg
    | WrappedGovernanceMsg GovernanceMsg
    | WrappedTokenMsg TokenMsg


type alias TranslationDictionary msg =
    { onInternalMessage : InternalMsg -> msg
    , onWrappedGovernanceMessage : GovernanceMsg -> msg
    , onWrappedTokenMessage : TokenMsg -> msg
    }


type alias Translator msg =
    Msg -> msg


translator : TranslationDictionary msg -> Translator msg
translator { onInternalMessage, onWrappedGovernanceMessage, onWrappedTokenMessage } msg =
    case msg of
        ForSelf internal ->
            onInternalMessage internal

        WrappedGovernanceMsg governanceMsg ->
            onWrappedGovernanceMessage governanceMsg

        WrappedTokenMsg tokenMsg ->
            onWrappedTokenMessage tokenMsg


emptyState : Model
emptyState =
    { proposals = Nothing
    , proposalVoteReceipts = Dict.empty
    , priorVotes = Dict.empty
    , delegateModalState =
        { step = NotShown
        , steps = []
        , maybeNewDelegateAddress = Nothing
        , maybeNewDelegateAddressIsValid = Nothing
        , maybeSelectedDelegate = Nothing
        , suggestedDelegates = []
        , topDelegates = []
        , addressDebounce = Debounce.init
        , inputFocused = False
        }
    , voteModalState = VoteModalNotShown
    , currentProposalsPageNumber = 1
    , requestCurrentDelegatee = True
    , currentDelegateeAcccount = Nothing
    , approveCAPState = ApproveCAPModalNotShown
    , errors = []
    }


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 100
    , transform = ForSelf << DebounceMsg
    }


handleShouldLoadCurrentDelegatee : Dict String String -> Maybe Network -> Account -> GovernanceState -> Model -> ( Model, Cmd Msg )
handleShouldLoadCurrentDelegatee apiBaseUrlMap maybeNetwork account governanceState ({ requestCurrentDelegatee } as model) =
    let
        maybeRequestCurrentDelegateeUrl =
            case ( requestCurrentDelegatee, maybeNetwork, account ) of
                ( True, Just network, Acct customer _ ) ->
                    case getDelegatedAddress customer governanceState of
                        Just (Delegatee (Customer delegateeAddressString)) ->
                            { addresses = [ delegateeAddressString ]
                            , order_by = Nothing
                            , page_size = 1
                            , page_number = 1
                            , with_history = False
                            }
                                |> governanceAccountRequestUrl apiBaseUrlMap network

                        _ ->
                            Nothing

                _ ->
                    Nothing
    in
    case maybeRequestCurrentDelegateeUrl of
        Just requestUrl ->
            let
                currentDelegateeRequest =
                    Http.get requestUrl governanceAccountResponseDecoder
            in
            ( { model | requestCurrentDelegatee = False }
            , Http.send (ForSelf << CurrentDelegateeResponse) currentDelegateeRequest
            )

        Nothing ->
            ( model
            , Cmd.none
            )


search : String -> Cmd Msg
search term =
    Task.perform (ForSelf << SearchDelegates) (Task.succeed term)


loadTop3Delegates : Dict String String -> Network -> Cmd Msg
loadTop3Delegates apiBaseUrlMap network =
    let
        maybeRequestTopDelegatesUrl =
            { addresses = []
            , order_by = Nothing
            , page_size = 3
            , page_number = 1
            , with_history = False
            }
                |> governanceAccountRequestUrl apiBaseUrlMap network
    in
    case maybeRequestTopDelegatesUrl of
        Just requestTopDelegatesUrl ->
            let
                topDelegatesRequest =
                    Http.get requestTopDelegatesUrl governanceAccountResponseDecoder
            in
            Http.send (ForSelf << TopDelegatesResponse) topDelegatesRequest

        Nothing ->
            Cmd.none


loadDelegatesForSearchTerm : Dict String String -> Network -> String -> Cmd Msg
loadDelegatesForSearchTerm apiBaseUrlMap network term =
    let
        maybeSearchDelegatesUrl =
            { term = term
            , page_size = 3
            , page_number = 1
            }
                |> governanceAccountSearchRequestUrl apiBaseUrlMap network
    in
    case maybeSearchDelegatesUrl of
        Just requestSearchDelegatesUrl ->
            let
                searchDelegatesRequest =
                    Http.get requestSearchDelegatesUrl governanceAccountSearchResponseDecoder
            in
            Http.send (ForSelf << SearchDelegatesResponse) searchDelegatesRequest

        Nothing ->
            Cmd.none


init : Dict String Config -> ( Model, Cmd Msg )
init configs =
    let
        initState =
            emptyState
    in
    ( initState, Cmd.none )


getPendingApproveCAPTransaction : Config -> Network -> CustomerAddress -> TransactionState -> Maybe Transaction
getPendingApproveCAPTransaction config network customer transactionState =
    config.maybeCompToken
        |> Maybe.andThen
            (\compToken ->
                transactionState.transactions
                    |> List.filter
                        (\transaction ->
                            (transaction.customer == customer)
                                && (transaction.network == network)
                                && (transaction.contract == compToken.address)
                                && (transaction.function == "approve")
                                && (transaction.status == Eth.Transaction.Pending)
                        )
                    |> List.head
            )


handleTransactionUpdate : Maybe Config -> Maybe Network -> Account -> TransactionState -> TransactionMsg -> Model -> Model
handleTransactionUpdate maybeConfig maybeNetwork account transactionState transactionMsg model =
    case ( maybeConfig, maybeNetwork, account ) of
        ( Just config, Just actualNetwork, Acct customer _ ) ->
            case transactionMsg of
                Eth.Transaction.NewTransaction transaction ->
                    if transaction.function == "approve" then
                        let
                            updatedModel =
                                case config.maybeCompToken of
                                    Just compToken ->
                                        if
                                            (transaction.customer == customer)
                                                && (transaction.network == actualNetwork)
                                                && (transaction.contract == compToken.address)
                                                && (transaction.status == Eth.Transaction.Pending)
                                                && (model.approveCAPState == AwaitingConfirmApproveCAPTransaction)
                                        then
                                            { model | approveCAPState = AwaitingApproveCAPTransactionMined }

                                        else
                                            model

                                    Nothing ->
                                        model
                        in
                        updatedModel

                    else if transaction.function == "castVote" || transaction.function == "castVoteWithReason" then
                        { model | voteModalState = VoteModalNotShown }

                    else
                        model

                Eth.Transaction.UpdateTransaction _ ->
                    let
                        pendingApproveCAPTransaction =
                            getPendingApproveCAPTransaction config actualNetwork customer transactionState
                    in
                    if model.approveCAPState == AwaitingApproveCAPTransactionMined && pendingApproveCAPTransaction == Nothing then
                        { model | approveCAPState = ApproveCAPModalNotShown }

                    else
                        model

                _ ->
                    model

        _ ->
            model


update : InternalMsg -> Maybe Config -> Maybe Network -> Dict String String -> Account -> Model -> ( Model, Cmd Msg )
update internalMsg maybeConfig maybeNetwork apiBaseUrlMap account model =
    case internalMsg of
        ProposalsPagerNext ->
            ( { model | currentProposalsPageNumber = model.currentProposalsPageNumber + 1 }, Cmd.none )

        ProposalsPagerPrevious ->
            ( { model | currentProposalsPageNumber = model.currentProposalsPageNumber - 1 }, Cmd.none )

        SetProposalsPager page ->
            ( { model | currentProposalsPageNumber = page }, Cmd.none )

        VoteDashboardDataResult data ->
            case ( maybeNetwork, account ) of
                ( Just network, Acct (Customer voter) _ ) ->
                    if String.toLower (networkName network) == data.network && voter == data.voter then
                        let
                            isCompletedState : List ProposalState -> Bool
                            isCompletedState states =
                                states
                                    |> List.foldl
                                        (\state acc ->
                                            if acc then
                                                acc

                                            else
                                                let
                                                    maybeCurrentState =
                                                        states |> List.reverse |> List.head |> Maybe.map .state
                                                in
                                                case maybeCurrentState of
                                                    Just Failed ->
                                                        True

                                                    Just Executed ->
                                                        True

                                                    Just Canceled ->
                                                        True

                                                    Just Expired ->
                                                        True

                                                    _ ->
                                                        False
                                        )
                                        False

                            sortedProposals =
                                data.proposals
                                    |> List.filter
                                        (\proposal ->
                                            if network == Ropsten then
                                                True

                                            else
                                                not (isCompletedState proposal.states)
                                        )
                                    |> List.sortBy .id
                                    |> List.reverse
                        in
                        ( { model | proposals = Just sortedProposals, proposalVoteReceipts = data.proposalVoteReceipts, priorVotes = data.priorVotes }, Cmd.none )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetDelegateModal modalStep ->
            let
                previousDelegateModalState =
                    model.delegateModalState

                newDelegateModalState =
                    let
                        newSteps =
                            case List.head previousDelegateModalState.steps of
                                Just prevHead ->
                                    if prevHead == modalStep then
                                        List.drop 1 previousDelegateModalState.steps

                                    else
                                        previousDelegateModalState.step :: previousDelegateModalState.steps

                                Nothing ->
                                    [ previousDelegateModalState.step ]

                        ( maybeNewDelegateAddress, maybeNewDelegateAddressIsValid ) =
                            if modalStep == NotShown || modalStep == SelectDelegationType then
                                ( Nothing, Nothing )

                            else
                                ( previousDelegateModalState.maybeNewDelegateAddress, previousDelegateModalState.maybeNewDelegateAddressIsValid )
                    in
                    { previousDelegateModalState | step = modalStep, steps = newSteps, maybeNewDelegateAddress = maybeNewDelegateAddress, maybeNewDelegateAddressIsValid = maybeNewDelegateAddressIsValid }

                loadTopDelegatesCmd =
                    case ( maybeNetwork, previousDelegateModalState.step, newDelegateModalState.step ) of
                        ( Just network, SelectDelegationType, DelegateVoting ) ->
                            loadTop3Delegates apiBaseUrlMap network

                        _ ->
                            Cmd.none
            in
            ( { model | delegateModalState = newDelegateModalState }, loadTopDelegatesCmd )

        DelegateInputFocused ->
            let
                previousDelegateModalState =
                    model.delegateModalState

                newDelegatModalState =
                    { previousDelegateModalState | inputFocused = True }
            in
            ( { model | delegateModalState = newDelegatModalState }, Cmd.none )

        DelegateInputBlurred ->
            let
                previousDelegateModalState =
                    model.delegateModalState

                newDelegatModalState =
                    { previousDelegateModalState | inputFocused = False }
            in
            ( { model | delegateModalState = newDelegatModalState }, Cmd.none )

        SelectDelegate delegate ->
            let
                previousDelegateModalState =
                    model.delegateModalState

                newDelegateModalState =
                    { previousDelegateModalState | maybeNewDelegateAddress = Just delegate.address, maybeNewDelegateAddressIsValid = Just (isValidAddress delegate.address), maybeSelectedDelegate = Just delegate }
            in
            ( { model | delegateModalState = newDelegateModalState }, Cmd.none )

        SearchDelegates term ->
            let
                searchDelegatesCmd =
                    case maybeNetwork of
                        Just network ->
                            loadDelegatesForSearchTerm apiBaseUrlMap network term

                        _ ->
                            Cmd.none
            in
            ( model, searchDelegatesCmd )

        SetNewDelegateAddress address ->
            let
                previousDelegateModalState =
                    model.delegateModalState

                ( newDelegateModalState, searchDelegatesCmd ) =
                    let
                        ( delegates, ( debounce, debounceCmd ) ) =
                            if String.length address > 2 then
                                ( previousDelegateModalState.suggestedDelegates, Debounce.push debounceConfig address previousDelegateModalState.addressDebounce )

                            else
                                ( previousDelegateModalState.topDelegates, ( previousDelegateModalState.addressDebounce, Cmd.none ) )
                    in
                    ( { previousDelegateModalState | maybeNewDelegateAddress = Just address, maybeNewDelegateAddressIsValid = Just (isValidAddress address), suggestedDelegates = delegates, addressDebounce = debounce }, debounceCmd )
            in
            ( { model | delegateModalState = newDelegateModalState }, searchDelegatesCmd )

        SetVoteModal voteModalState ->
            let
                voteCmd =
                    case voteModalState of
                        ConfirmVoteType proposal voteType reason ->
                            case ( maybeConfig, account ) of
                                ( Just config, Acct accountAddress _ ) ->
                                    case config.maybeGovernor of
                                        Just ( governorAddress, isBravo ) ->
                                            voteProposal accountAddress (getContractAddressString governorAddress) isBravo proposal.id voteType reason

                                        _ ->
                                            Cmd.none

                                _ ->
                                    Cmd.none

                        _ ->
                            Cmd.none
            in
            ( { model | voteModalState = voteModalState }, voteCmd )

        TopDelegatesResponse result ->
            case result of
                Ok delegatesResponse ->
                    let
                        previousDelegateModalState =
                            model.delegateModalState

                        newDelegateModalState =
                            { previousDelegateModalState | suggestedDelegates = delegatesResponse.accounts, topDelegates = delegatesResponse.accounts }
                    in
                    ( { model | delegateModalState = newDelegateModalState }, Cmd.none )

                Err errMsg ->
                    ( model, Console.error ("Error from Comp Accounts API, " ++ Utils.Http.showError errMsg) )

        SearchDelegatesResponse result ->
            case result of
                Ok delegatesResponse ->
                    let
                        previousDelegateModalState =
                            model.delegateModalState

                        newDelegateModalState =
                            { previousDelegateModalState | suggestedDelegates = delegatesResponse.accounts }
                    in
                    ( { model | delegateModalState = newDelegateModalState }, Cmd.none )

                Err errMsg ->
                    ( model, Console.error ("Error from Comp Accounts Search API, " ++ Utils.Http.showError errMsg) )

        DebounceMsg msg_ ->
            let
                previousDelegateModalState =
                    model.delegateModalState

                ( addressDebounce, debounceCmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast search)
                        msg_
                        previousDelegateModalState.addressDebounce

                newDelegateModalState =
                    { previousDelegateModalState | addressDebounce = addressDebounce }
            in
            ( { model | delegateModalState = newDelegateModalState }, debounceCmd )

        DelegateTo ((Customer delegateAddress) as targetAddress) ->
            case maybeConfig of
                Just config ->
                    case ( config.maybeCompToken, account ) of
                        ( Just comp, Acct accountAddress _ ) ->
                            let
                                isValidDelegate =
                                    isValidAddress delegateAddress

                                previousDelegateModalState =
                                    model.delegateModalState

                                newDelegateModalState =
                                    let
                                        newSteps =
                                            previousDelegateModalState.step :: previousDelegateModalState.steps
                                    in
                                    { previousDelegateModalState | step = DelegateTransaction, steps = newSteps, maybeNewDelegateAddress = Just delegateAddress, maybeNewDelegateAddressIsValid = Just isValidDelegate }

                                updatedModel =
                                    { model | delegateModalState = newDelegateModalState }
                            in
                            ( updatedModel, Cmd.map WrappedGovernanceMsg (Eth.Governance.askDelegateTo comp.address accountAddress targetAddress) )

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DismissApproveCAPModal ->
            ( { model | approveCAPState = ApproveCAPModalNotShown }, Cmd.none )

        ImageError elementId imgSrc ->
            ( model, imageError elementId imgSrc )

        CurrentDelegateeResponse result ->
            case result of
                Ok delegateeResponse ->
                    ( { model | currentDelegateeAcccount = List.head delegateeResponse.accounts }, Cmd.none )

                Err errMsg ->
                    ( model, Console.error ("Error from Comp Accounts API getting delegatee address, " ++ Utils.Http.showError errMsg) )

        Error error ->
            ( { model | errors = error :: model.errors }, Console.error error )


handleTokenUpdate : Config -> TokenMsg -> Model -> Model
handleTokenUpdate config tokenMsg model =
    case tokenMsg of
        Eth.Token.Web3TransactionMsg (Eth.Token.FaucetTokenApprove network possibleCapFactoryAddress possibleCompAddress _ _) ->
            case config.maybeCrowdProposalFactory of
                Just capFactory ->
                    if possibleCapFactoryAddress == capFactory then
                        { model | approveCAPState = AwaitingConfirmApproveCAPTransaction }

                    else
                        model

                Nothing ->
                    model

        _ ->
            model


view : Translations.Lang -> Maybe Config -> Maybe Network -> Time.Zone -> Maybe Time.Posix -> Account -> TransactionState -> GovernanceState -> TokenState -> Model -> Html Msg
view userLanguage maybeConfig maybeNetwork timezone maybeCurrentTime account transactionState governanceState tokenState model =
    let
        { headerView, walletPanel, proposalsPanelView, votingModalView, delegateModalView, approveCAPModalView } =
            case ( maybeConfig, account, maybeNetwork ) of
                ( Just config, Acct accountAddress _, Just network ) ->
                    case config.maybeCompToken of
                        Just _ ->
                            let
                                voteTransactionsByProposalId =
                                    transactionState.transactions
                                        |> List.filter
                                            (\transaction ->
                                                (transaction.customer == accountAddress)
                                                    && (transaction.network == network)
                                                    && (transaction.function
                                                            == "castVote"
                                                            || transaction.function
                                                            == "castVoteWithReason"
                                                            || transaction.function
                                                            == "queue"
                                                            || transaction.function
                                                            == "execute"
                                                       )
                                            )
                                        |> List.foldl
                                            (\transaction accum ->
                                                let
                                                    maybeProposalId =
                                                        transaction.args
                                                            |> List.head
                                                in
                                                case maybeProposalId of
                                                    Just proposalId ->
                                                        Dict.insert proposalId transaction accum

                                                    Nothing ->
                                                        accum
                                            )
                                            Dict.empty

                                maybeMostRecentDelegateTransaction =
                                    transactionState.transactions
                                        |> List.filter
                                            (\transaction ->
                                                (transaction.customer == accountAddress)
                                                    && (transaction.network == network)
                                                    && (transaction.function == "delegate")
                                            )
                                        |> List.head

                                maybePendingApproveCAPTransaction =
                                    getPendingApproveCAPTransaction config network accountAddress transactionState

                                voteHeader =
                                    case ( getCurrentVotes accountAddress governanceState, getCompoundGovernanceTokenBalance accountAddress governanceState ) of
                                        ( Just votes, Just compBalance ) ->
                                            let
                                                ( headerText, headerSpan ) =
                                                    if Decimal.eq votes Decimal.zero && getDelegatedAddress accountAddress governanceState /= Just Undelegated then
                                                        ( Translations.delegated_votes userLanguage, compOrVoteBalanceSpan ( "", "balance-suffix--dark" ) compBalance )

                                                    else
                                                        ( Translations.votes userLanguage, compOrVoteBalanceSpan ( "", "balance-suffix--dark" ) votes )
                                            in
                                            div [ class "vote__header" ]
                                                [ label [] [ text headerText ]
                                                , div [ class "headline" ]
                                                    headerSpan
                                                ]

                                        _ ->
                                            loadingHeader
                            in
                            { headerView = voteHeader
                            , walletPanel = votingWalletPanel userLanguage config network accountAddress maybeMostRecentDelegateTransaction governanceState tokenState model
                            , proposalsPanelView = proposalsPanel userLanguage timezone maybeCurrentTime config network account model voteTransactionsByProposalId
                            , votingModalView = votingModal userLanguage model
                            , delegateModalView = delegateModal userLanguage network maybeMostRecentDelegateTransaction accountAddress governanceState model.delegateModalState
                            , approveCAPModalView = approveCAPModal userLanguage config maybeNetwork accountAddress tokenState maybePendingApproveCAPTransaction model
                            }

                        Nothing ->
                            let
                                header_ =
                                    div [ class "vote__header" ]
                                        [ label [ class "vote__header__label" ] [ text (Translations.no_governance userLanguage) ]
                                        , div [ class "headline vote__header__value vote__header__value--no-governance" ] []
                                        ]
                            in
                            { headerView = header_
                            , walletPanel = loadingVotingWalletPanel userLanguage
                            , proposalsPanelView = loadingProposalsPanel userLanguage
                            , votingModalView = text ""
                            , delegateModalView = text ""
                            , approveCAPModalView = text ""
                            }

                _ ->
                    { headerView = loadingHeader
                    , walletPanel = loadingVotingWalletPanel userLanguage
                    , proposalsPanelView = loadingProposalsPanel userLanguage
                    , votingModalView = text ""
                    , delegateModalView = text ""
                    , approveCAPModalView = text ""
                    }
    in
    div [ id "Vote" ]
        [ section [ class "hero" ]
            [ div [ class "container" ]
                [ headerView
                ]
            ]
        , section [ class "vote__details" ]
            [ div [ class "container-large" ]
                [ div [ class "row" ]
                    [ walletPanel
                    , proposalsPanelView
                    ]
                ]
            ]
        , votingModalView
        , delegateModalView
        , approveCAPModalView
        ]


delegateModal : Translations.Lang -> Network -> Maybe Transaction -> CustomerAddress -> GovernanceState -> DelegateModalState -> Html Msg
delegateModal userLanguage network maybeDelegateTransaction ((Customer addressString) as accountAddress) governanceState { step, steps, maybeNewDelegateAddress, maybeNewDelegateAddressIsValid, maybeSelectedDelegate, inputFocused, suggestedDelegates } =
    let
        previousStep =
            steps
                |> List.head
                |> Maybe.withDefault NotShown
    in
    if step == NotShown then
        text ""

    else
        let
            ( backButton, headerText, body ) =
                case step of
                    SelectDelegationType ->
                        let
                            { manualActiveTextClass, delegateActiveTextClass, manualOptionDisabledClass, manualOptionOnClick } =
                                let
                                    defaultObject =
                                        { manualOptionDisabledClass = ""
                                        , manualActiveTextClass = ""
                                        , delegateActiveTextClass = ""
                                        , manualOptionOnClick = [ onClick (ForSelf (DelegateTo accountAddress)) ]
                                        }
                                in
                                case getDelegatedAddress accountAddress governanceState of
                                    Just Self ->
                                        { defaultObject | manualOptionOnClick = [], manualOptionDisabledClass = " vote__modal__body__delegate-option--disabled", manualActiveTextClass = " vote__modal__body__delegate-option__text-view__title--active" }

                                    Just (Delegatee _) ->
                                        { defaultObject | delegateActiveTextClass = " vote__modal__body__delegate-option__text-view__title--active" }

                                    _ ->
                                        defaultObject
                        in
                        ( text ""
                        , Translations.choose_delegation_type userLanguage
                        , div [ class "vote__modal__body vote__modal__body--delegate-modal" ]
                            [ div (class ("vote__modal__body__delegate-option" ++ manualOptionDisabledClass) :: manualOptionOnClick)
                                [ img [ class "vote__modal__body__delegate-option__icon", src "/images/icn-voting-manual.svg" ] []
                                , div [ class "vote__modal__body__delegate-option__text-view" ]
                                    [ p [ class ("vote__modal__body__delegate-option__text-view__title" ++ manualActiveTextClass) ] [ text (Translations.manual_voting userLanguage) ]
                                    , p [ class "vote__modal__body__paragraph-text vote__modal__body__delegate-option__text-view__description" ] [ text (Translations.manual_voting_description userLanguage) ]
                                    ]
                                ]
                            , div [ class "vote__modal__body__delegate-option", onClick (ForSelf (SetDelegateModal DelegateVoting)) ]
                                [ img [ class "vote__modal__body__delegate-option__icon", src "/images/icn-voting-delegate.svg" ] []
                                , div [ class "vote__modal__body__delegate-option__text-view" ]
                                    [ p [ class ("vote__modal__body__delegate-option__text-view__title" ++ delegateActiveTextClass) ] [ text (Translations.delegate_voting userLanguage) ]
                                    , p [ class "vote__modal__body__paragraph-text vote__modal__body__delegate-option__text-view__description" ] [ text (Translations.delegate_voting_description userLanguage) ]
                                    ]
                                ]
                            ]
                        )

                    DelegateTransaction ->
                        let
                            votesToDelegate =
                                getCompoundGovernanceTokenBalance accountAddress governanceState
                                    |> Maybe.withDefault Decimal.zero

                            title =
                                case maybeNewDelegateAddress of
                                    Just newDelegateAddress ->
                                        if newDelegateAddress == addressString then
                                            Translations.manual_voting_from userLanguage (shortenedAddressString 4 4 newDelegateAddress)

                                        else
                                            Translations.delegating_to_address userLanguage (shortenedAddressString 4 4 newDelegateAddress)

                                    Nothing ->
                                        ""

                            { headerText_, greenCircle, helpText, etherscanButton } =
                                case maybeDelegateTransaction of
                                    Just transaction ->
                                        let
                                            maybeDelegateArg =
                                                transaction.args
                                                    |> List.head

                                            ( modalHeaderText, helpText_, greenCircle_ ) =
                                                if transaction.status == Success && maybeDelegateArg == maybeNewDelegateAddress then
                                                    ( Translations.transaction_confirmed userLanguage, Translations.transaction_confirmed userLanguage, div [ class "vote__modal__body__success" ] [ img [ src "/images/check-big.svg" ] [] ] )

                                                else
                                                    ( Translations.transaction_pending userLanguage, Translations.transaction_broadcast_no_estimation userLanguage, div [ class "connecting-ring" ] [ div [] [] ] )
                                        in
                                        { headerText_ = modalHeaderText
                                        , greenCircle = greenCircle_
                                        , helpText = helpText_
                                        , etherscanButton =
                                            Ethereum.etherscanLink
                                                (Just network)
                                                (Ethereum.TransactionHash transaction.trxHash)
                                                [ class "submit-button button main vote__modal__body__button" ]
                                                [ text (Translations.view_on_etherscan userLanguage) ]
                                        }

                                    Nothing ->
                                        { headerText_ = Translations.confirm_transaction userLanguage
                                        , greenCircle = div [ class "connecting-ring" ] [ div [] [] ]
                                        , helpText = Translations.confirm_the_transaction userLanguage
                                        , etherscanButton = text ""
                                        }
                        in
                        ( div [ class "back-arrow" ]
                            [ button [ onClick (ForSelf (SetDelegateModal previousStep)) ]
                                []
                            ]
                        , headerText_
                        , div [ class "vote__modal__body" ]
                            [ div [ class "vote__modal__body__votes-view" ]
                                [ p [ class "vote__modal__body__votes" ] [ text (formatTokenBalance votesToDelegate ++ " " ++ Translations.votes userLanguage) ]
                                ]
                            , p [ class "vote__modal__body__title" ] [ text title ]
                            , greenCircle
                            , label [ class "vote__modal__body__help" ] [ text helpText ]
                            , etherscanButton
                            ]
                        )

                    DelegateVoting ->
                        let
                            valueText =
                                maybeNewDelegateAddress
                                    |> Maybe.withDefault ""

                            delegatedAddressString =
                                getDelegatedAddressString accountAddress governanceState
                                    |> Maybe.withDefault zeroAddress

                            ( helpText, actionButton ) =
                                if String.toLower valueText == delegatedAddressString then
                                    ( p [ class "vote__modal__body__delegate-voting__input-view__help-text vote__modal__body__delegate-voting__input-view__help-text--invalid" ] [ text (Translations.already_delegating_address userLanguage) ]
                                    , button [ disabled, class "submit-button button main vote__modal__body__delegate-voting__button" ] [ text (Translations.delegate_votes userLanguage) ]
                                    )

                                else if maybeNewDelegateAddressIsValid == Just True then
                                    let
                                        message =
                                            case maybeSelectedDelegate of
                                                Just delegate ->
                                                    let
                                                        displayName =
                                                            delegate.display_name
                                                                |> Maybe.withDefault (shortenedAddressString 4 4 delegate.address)

                                                        voteWeight =
                                                            Translations.voting_weight userLanguage ++ ": " ++ formatPercentage delegate.vote_weight
                                                    in
                                                    displayName ++ " - " ++ voteWeight

                                                Nothing ->
                                                    Translations.valid_address userLanguage
                                    in
                                    ( p [ class "vote__modal__body__delegate-voting__input-view__help-text vote__modal__body__delegate-voting__input-view__help-text--valid" ] [ text message ]
                                    , button [ class "submit-button button main vote__modal__body__delegate-voting__button", onClick (ForSelf (DelegateTo (Customer valueText))) ] [ text (Translations.delegate_votes userLanguage) ]
                                    )

                                else if maybeNewDelegateAddressIsValid == Just False then
                                    ( p [ class "vote__modal__body__delegate-voting__input-view__help-text vote__modal__body__delegate-voting__input-view__help-text--invalid" ] [ text (Translations.invalid_address userLanguage) ]
                                    , button [ disabled, class "submit-button button main vote__modal__body__delegate-voting__button" ] [ text (Translations.delegate_votes userLanguage) ]
                                    )

                                else
                                    ( text ""
                                    , button [ disabled, class "submit-button button main vote__modal__body__delegate-voting__button" ] [ text (Translations.delegate_votes userLanguage) ]
                                    )

                            inputView =
                                let
                                    dropdownActive =
                                        if inputFocused then
                                            " active"

                                        else
                                            ""

                                    leaderboardUrl =
                                        landingUrlForPage network GovernanceLeaderboard
                                in
                                div [ class "dropdown dropdown--big" ]
                                    [ input
                                        [ class "vote__modal__body__delegate-voting__input-view__input"
                                        , type_ "text"
                                        , placeholder (Translations.enter_address userLanguage)
                                        , value valueText
                                        , onFocus (ForSelf DelegateInputFocused)
                                        , onBlur (ForSelf DelegateInputBlurred)
                                        , onInput (ForSelf << SetNewDelegateAddress)
                                        ]
                                        []
                                    , div [ class ("dropdown__options dropdown__options--light vote__modal__body__delegate-voting__input-view__delegate-options" ++ dropdownActive) ]
                                        ((suggestedDelegates
                                            |> List.map
                                                (\delegate ->
                                                    let
                                                        displayName =
                                                            delegate.display_name
                                                                |> Maybe.withDefault (shortenedAddressString 4 4 delegate.address)

                                                        voteWeight =
                                                            Translations.voting_weight userLanguage ++ ": " ++ formatPercentage delegate.vote_weight

                                                        profileImageComponent =
                                                            { maybeProfileImageUrl = delegate.image_url
                                                            , address = delegate.address
                                                            , size = ProfileImageHelper.Small
                                                            , showProposerIcon = Decimal.gte delegate.votes (GovernanceHelper.proposalThreshold network)
                                                            , useLightStyle = True
                                                            , showCrowdProposalIcon = delegate.crowd_proposal /= Nothing
                                                            }
                                                                |> ProfileImageHelper.profileImage
                                                                |> Html.map
                                                                    (\profileImageMsg ->
                                                                        case profileImageMsg of
                                                                            ProfileImageHelper.ProfileImageError profileElementId defaultImage ->
                                                                                ForSelf (ImageError profileElementId defaultImage)
                                                                    )

                                                        delegateUrl =
                                                            landingUrlForPage network (GovernanceAccountProfile delegate.address)
                                                    in
                                                    div [ class "dropdown__option dropdown__option--light vote__modal__body__delegate-voting__input-view__delegate-option", onClick (ForSelf (SelectDelegate delegate)) ]
                                                        [ profileImageComponent
                                                        , div [ class "vote__modal__body__delegate-voting__input-view__delegate-option__details" ]
                                                            [ p [ class "vote__modal__body__delegate-voting__input-view__delegate-option__details__display-name" ] [ text displayName ]
                                                            , p [ class "vote__modal__body__delegate-voting__input-view__delegate-option__details__vote-weight" ] [ text voteWeight ]
                                                            ]
                                                        , a ([ class "vote__modal__body__delegate-voting__input-view__delegate-option__link", target "__blank" ] ++ href External delegateUrl)
                                                            [ div [ class "line-icon line-icon--external-link" ] []
                                                            ]
                                                        ]
                                                )
                                         )
                                            ++ [ a ([ class "dropdown__option dropdown__option--light vote__modal__body__delegate-voting__input-view__delegate-option-footer", target "__blank" ] ++ href External leaderboardUrl) [ text (Translations.view_delegate_leaderboard userLanguage) ] ]
                                        )
                                    ]
                        in
                        ( div [ class "back-arrow" ]
                            [ button [ onClick (ForSelf (SetDelegateModal previousStep)) ]
                                []
                            ]
                        , Translations.delegate_voting userLanguage
                        , div [ class "vote__modal__body vote__modal__body--delegate-modal" ]
                            [ div [ class "vote__modal__body__delegate-voting" ]
                                [ p [] [ text (Translations.select_an_address userLanguage) ]
                                , p [ class "vote__modal__body__paragraph-text vote__modal__body__delegate-voting__description" ] [ text (Translations.provide_address_description userLanguage) ]
                                , div [ class "vote__modal__body__delegate-voting__input-view" ]
                                    [ div [ class "vote__modal__body__delegate-voting__input-view__label-view" ]
                                        [ p [ class "vote__modal__body__delegate-voting__input-view__label" ] [ text (Translations.delegate_address userLanguage) ]
                                        , a ([ class "vote__modal__body__delegate-voting__input-view__link", target "__blank" ] ++ href External (landingUrlForPage network GovernanceLeaderboard)) [ text (Translations.delegate_leaderboard userLanguage) ]
                                        ]
                                    , inputView
                                    , helpText
                                    ]
                                , actionButton
                                ]
                            ]
                        )

                    NotShown ->
                        ( text "", "", text "" )
        in
        div [ class "modal vote__modal" ]
            [ div [ class "cover active", onClick (ForSelf (SetDelegateModal NotShown)) ] []
            , div [ class "legacy-panel" ]
                [ div [ class "header" ]
                    [ backButton
                    , div [ class "title" ]
                        [ span [] [ text headerText ]
                        ]
                    , div [ class "close-x" ]
                        [ button [ onClick (ForSelf (SetDelegateModal NotShown)) ]
                            []
                        ]
                    ]
                , body
                ]
            ]


votingModal : Translations.Lang -> Model -> Html Msg
votingModal userLanguage model =
    case model.voteModalState of
        SelectVoteType proposal maybeVoteType reason ->
            let
                ( forPercent, againstPercent, abstainPercent ) =
                    proposalPercentages proposal

                actionButton =
                    case maybeVoteType of
                        Just voteType ->
                            let
                                voteTypeString =
                                    case voteType of
                                        For ->
                                            Translations.for userLanguage

                                        Against ->
                                            Translations.against userLanguage

                                        Abstain ->
                                            Translations.abstain userLanguage
                            in
                            button [ class "submit-button button main vote__modal__body__votes-view__button", onClick (ForSelf (SetVoteModal (ConfirmVoteType proposal voteType reason))) ] [ text (Translations.cast_choice_vote userLanguage voteTypeString) ]

                        Nothing ->
                            button [ disabled, class "submit-button button main vote__modal__body__votes-view__button" ] [ text (Translations.choose_a_vote userLanguage) ]
            in
            div [ class "modal vote__modal" ]
                [ div [ class "cover active", onClick (ForSelf (SetVoteModal VoteModalNotShown)) ] []
                , div [ class "legacy-panel" ]
                    [ div [ class "header" ]
                        [ div [ class "title" ]
                            [ span [] [ text (Translations.cast_vote userLanguage) ]
                            ]
                        , div [ class "close-x" ]
                            [ button [ onClick (ForSelf (SetVoteModal VoteModalNotShown)) ]
                                []
                            ]
                        ]
                    , div [ class "vote__modal__body vote__modal__body--selecting-vote-type" ]
                        [ p [ class "vote__modal__body--selecting-vote-type__title" ] [ text proposal.title ]
                        , div [ class "tally-card-view", onClick (ForSelf (SetVoteModal (SelectVoteType proposal (Just For) reason))) ]
                            [ voteTallyCard userLanguage For forPercent (maybeVoteType == Just For)
                            ]
                        , div [ class "tally-card-view", onClick (ForSelf (SetVoteModal (SelectVoteType proposal (Just Against) reason))) ]
                            [ voteTallyCard userLanguage Against againstPercent (maybeVoteType == Just Against)
                            ]
                        , div [ class "tally-card-view", onClick (ForSelf (SetVoteModal (SelectVoteType proposal (Just Abstain) reason))) ]
                            [ voteTallyCard userLanguage Abstain abstainPercent (maybeVoteType == Just Abstain)
                            ]
                        , p [ class "vote__modal__body--selecting-vote-type__label-header" ] [ text (Translations.add_reason userLanguage) ]
                        , textarea [ class "vote__modal__body__text-area", placeholder (Translations.tell_others userLanguage), onInput (ForSelf << SetVoteModal << SelectVoteType proposal maybeVoteType), value reason ] []
                        , actionButton
                        ]
                    ]
                ]

        ConfirmVoteType proposal voteType _ ->
            let
                votes =
                    Dict.get (String.fromInt proposal.id) model.priorVotes
                        |> Maybe.withDefault Decimal.zero

                voteTypeString =
                    case voteType of
                        For ->
                            Translations.for userLanguage

                        Against ->
                            Translations.against userLanguage

                        Abstain ->
                            Translations.abstain userLanguage
            in
            div
                [ class "modal vote__modal" ]
                [ div [ class "cover active", onClick (ForSelf (SetVoteModal VoteModalNotShown)) ] []
                , div [ class "legacy-panel" ]
                    [ div [ class "header" ]
                        [ div [ class "title" ]
                            [ span [] [ text (Translations.cast_choice_vote userLanguage voteTypeString) ]
                            ]
                        , div [ class "close-x" ]
                            [ button [ onClick (ForSelf (SetVoteModal VoteModalNotShown)) ]
                                []
                            ]
                        ]
                    , div [ class "vote__modal__body" ]
                        [ div [ class "vote__modal__body__votes-view" ]
                            [ p [ class "vote__modal__body__votes" ] [ text (formatTokenBalance votes ++ " " ++ Translations.votes userLanguage) ]
                            , div [ class "vote__modal__body__tooltip" ]
                                [ img [ src "/images/icon_i.svg" ] []
                                , div [ class "vote__modal__body__tooltip__text" ]
                                    [ p [ class "vote__modal__body__tooltip__text__question" ] [ text (Translations.votes_tooltip_question userLanguage) ]
                                    , p [ class "vote__modal__body__tooltip__text__answer" ] [ text (Translations.votes_tooltip userLanguage) ]
                                    ]
                                ]
                            ]
                        , p [ class "vote__modal__body__title" ] [ text proposal.title ]
                        , div [ class "connecting-ring" ] [ div [] [] ]
                        , label [ class "vote__modal__body__help" ] [ text (Translations.confirm_the_transaction userLanguage) ]
                        ]
                    ]
                ]

        VoteModalNotShown ->
            text ""


approveCAPModal : Translations.Lang -> Config -> Maybe Network -> CustomerAddress -> TokenState -> Maybe Transaction -> Model -> Html Msg
approveCAPModal userLanguage config maybeNetwork customer tokenState maybePendingApproveCAPTransaction model =
    if model.approveCAPState /= ApproveCAPModalNotShown then
        let
            { headerText, greenCircle, helpText, etherscanButton } =
                if model.approveCAPState == AwaitingConfirmApproveCAPTransaction then
                    { headerText = Translations.confirm_transaction userLanguage
                    , greenCircle = div [ class "connecting-ring" ] [ div [] [] ]
                    , helpText = Translations.confirm_the_transaction userLanguage
                    , etherscanButton = text ""
                    }

                else
                    { headerText = Translations.transaction_pending userLanguage
                    , greenCircle =
                        if maybePendingApproveCAPTransaction == Nothing then
                            div [ class "vote__modal__body__success" ] [ img [ src "/images/check-big.svg" ] [] ]

                        else
                            div [ class "connecting-ring" ] [ div [] [] ]
                    , helpText = Translations.transaction_broadcast_no_estimation userLanguage
                    , etherscanButton =
                        maybePendingApproveCAPTransaction
                            |> Maybe.map
                                (\transaction ->
                                    Ethereum.etherscanLink
                                        maybeNetwork
                                        (Ethereum.TransactionHash transaction.trxHash)
                                        [ class "submit-button button main vote__modal__body__button" ]
                                        [ text (Translations.view_on_etherscan userLanguage) ]
                                )
                            |> Maybe.withDefault (text "")
                    }
        in
        div [ class "modal vote__modal" ]
            [ div [ class "cover active", onClick (ForSelf DismissApproveCAPModal) ] []
            , div [ class "legacy-panel" ]
                [ div [ class "header" ]
                    [ div [ class "title" ]
                        [ span [] [ text headerText ]
                        ]
                    , div [ class "close-x" ]
                        [ button [ onClick (ForSelf DismissApproveCAPModal) ]
                            []
                        ]
                    ]
                , div [ class "vote__modal__body" ]
                    [ p [ class "vote__modal__body__title" ] [ text (Translations.confirm_transaction userLanguage) ]
                    , greenCircle
                    , label [ class "vote__modal__body__help" ] [ text helpText ]
                    , etherscanButton
                    ]
                ]
            ]

    else
        text ""


loadingHeader : Html msg
loadingHeader =
    div [ class "vote__header" ]
        [ label [ class "vote__header__label vote__header__label--loading" ] []
        , div [ class "headline vote__header__value vote__header__value--loading" ] []
        ]


loadingVotingWalletPanel : Translations.Lang -> Html msg
loadingVotingWalletPanel userLanguage =
    div [ class "col-md-4 wallet-panel" ]
        [ div [ class "panel" ]
            [ div [ class "panel__header" ]
                [ h4 [] [ text (Translations.voting_wallet userLanguage) ] ]
            , div [ class "wallet-panel__data" ]
                [ div [ class "wallet-panel__data__row" ]
                    [ label [ class "wallet-panel__data__row__label wallet-panel__data__row__label--loading" ] []
                    , p [ class "wallet-panel__data__row__value wallet-panel__data__row__value--loading" ] []
                    , div [ class "wallet-panel__data__row__vote-weight" ] []
                    ]
                , div [ class "wallet-panel__data__row" ]
                    [ label [ class "wallet-panel__data__row__label wallet-panel__data__row__label--loading" ] []
                    , p [ class "wallet-panel__data__row__value wallet-panel__data__row__value--loading" ] []
                    ]
                , div [ class "wallet-panel__data__row" ]
                    [ label [ class "wallet-panel__data__row__label wallet-panel__data__row__label--loading" ] []
                    , p [ class "wallet-panel__data__row__value wallet-panel__data__row__value--loading" ] []
                    , div [ class "button wallet-panel__data__row__button wallet-panel__data__row__button--loading", disabled ] [ div [ class "wallet-panel__data__row__button__text" ] [] ]
                    ]
                ]
            ]
        ]


votingWalletPanel : Translations.Lang -> Config -> Network -> CustomerAddress -> Maybe Transaction -> GovernanceState -> TokenState -> Model -> Html Msg
votingWalletPanel userLanguage config network accountAddress maybeDelegateTransaction governanceState tokenState { currentDelegateeAcccount } =
    case ( getCurrentVotes accountAddress governanceState, getCompoundGovernanceTokenBalance accountAddress governanceState, getDelegatedAddress accountAddress governanceState ) of
        ( Just votes_, Just balance_, Just delegateType ) ->
            let
                ( votesSpan, voteWeight ) =
                    let
                        weight =
                            Decimal.fastdiv votes_ (GovernanceHelper.proposalThreshold network)
                                |> Maybe.withDefault Decimal.zero
                    in
                    ( compOrVoteBalanceSpan ( "", "balance-suffix" ) votes_, formatPercentage weight )

                balanceSpans =
                    compOrVoteBalanceSpan ( "", "balance-suffix" ) balance_

                delegateHrefForAddress address =
                    let
                        addressString =
                            getCustomerAddressString address
                    in
                    href External (landingUrlForPage network (GovernanceAccountProfile addressString))

                ( delegateImage, delegateDisplayName, delegateUrlLink ) =
                    case delegateType of
                        Self ->
                            ( text ""
                            , text (Translations.self userLanguage)
                            , a ([ class "wallet-panel__data__row__value--option-link", target "__blank" ] ++ delegateHrefForAddress accountAddress)
                                [ div [ class "line-icon line-icon--external-link line-icon--external-link--black" ] []
                                ]
                            )

                        Delegatee (Customer delegateeAddressString) ->
                            let
                                ( displayName, profileImageConfig ) =
                                    case currentDelegateeAcccount of
                                        Just delegate ->
                                            ( delegate.display_name
                                                |> Maybe.withDefault (shortenedAddressString 4 4 delegate.address)
                                            , { maybeProfileImageUrl = delegate.image_url
                                              , address = delegate.address
                                              , size = ProfileImageHelper.Small
                                              , showProposerIcon = Decimal.gte delegate.votes (GovernanceHelper.proposalThreshold network)
                                              , useLightStyle = True
                                              , showCrowdProposalIcon = delegate.crowd_proposal /= Nothing
                                              }
                                            )

                                        Nothing ->
                                            ( shortenedAddressString 4 4 delegateeAddressString
                                            , { maybeProfileImageUrl = Nothing
                                              , address = delegateeAddressString
                                              , size = ProfileImageHelper.Small
                                              , showProposerIcon = False
                                              , useLightStyle = True
                                              , showCrowdProposalIcon = False
                                              }
                                            )

                                profileImageComponent =
                                    profileImageConfig
                                        |> ProfileImageHelper.profileImage
                                        |> Html.map
                                            (\profileImageMsg ->
                                                case profileImageMsg of
                                                    ProfileImageHelper.ProfileImageError profileElementId defaultImage ->
                                                        ForSelf (ImageError profileElementId defaultImage)
                                            )
                            in
                            ( profileImageComponent
                            , text displayName
                            , a ([ class "wallet-panel__data__row__value--option-link", target "__blank" ] ++ delegateHrefForAddress (Customer delegateeAddressString))
                                [ div [ class "line-icon line-icon--external-link line-icon--external-link--black" ] []
                                ]
                            )

                        Undelegated ->
                            ( text ""
                            , text (Translations.undelegated userLanguage)
                            , text ""
                            )

                ( delegateP, delegateButton ) =
                    let
                        defaultTuple =
                            ( p [ class "wallet-panel__data__row__value" ]
                                [ delegateDisplayName
                                , delegateUrlLink
                                ]
                            , div [ class "wallet-panel__data__row__action" ]
                                [ a [ onClick (ForSelf (SetDelegateModal SelectDelegationType)) ] [ text (Translations.change userLanguage) ]
                                ]
                            )
                    in
                    case maybeDelegateTransaction of
                        Just transaction ->
                            if transaction.status == Eth.Transaction.Pending then
                                ( p [ class "wallet-panel__data__row__value wallet-panel__data__row__value--pending" ] [ text (Translations.pending userLanguage) ]
                                , text ""
                                )

                            else
                                defaultTuple

                        _ ->
                            defaultTuple

                createProposalViewOrMeter =
                    let
                        showCreateProposalView =
                            Decimal.gt votes_ (GovernanceHelper.proposalThreshold network)

                        crowdProposalThreshold =
                            Decimal.fromInt 100

                        showCreateCrowdProposalView =
                            Decimal.gte balance_ crowdProposalThreshold
                    in
                    if showCreateProposalView then
                        div []
                            [ a (class "button main create-proposal-panel__button" :: href PageNavigation (getHrefUrl Propose)) [ text (Translations.create_proposal userLanguage) ] ]

                    else if showCreateCrowdProposalView then
                        let
                            isCAPApproved =
                                Eth.Token.isCAPFactoryApproved config tokenState

                            enableClickOrDisabled =
                                case ( config.maybeCompToken, config.maybeCrowdProposalFactory ) of
                                    ( Just compToken, Just capFactory ) ->
                                        let
                                            compAssetAddress =
                                                Ethereum.contractAddressToAssetAddress compToken.address
                                        in
                                        Eth.Token.FaucetTokenApprove
                                            network
                                            capFactory
                                            compAssetAddress
                                            accountAddress
                                            True
                                            |> Eth.Token.Web3TransactionMsg
                                            |> WrappedTokenMsg
                                            |> onClickStopPropagation

                                    _ ->
                                        disabled
                        in
                        if isCAPApproved then
                            a (class "button main create-proposal-panel__button" :: href PageNavigation (getHrefUrl CrowdPropose)) [ text (Translations.create_autonomous_proposal userLanguage) ]

                        else
                            a (class "button main create-proposal-panel__button" :: [ enableClickOrDisabled ]) [ text (Translations.approve_proposal_creation userLanguage) ]

                    else
                        let
                            proposalThreshold =
                                GovernanceHelper.proposalThreshold network

                            formattedThreshold =
                                formatToDecimalPlaces 0 False proposalThreshold

                            formattedVotes =
                                formatToDecimalPlaces 0 False votes_

                            docsUrl =
                                landingUrlForPage MainNet (Docs Governance Nothing)
                        in
                        div [ class "tooltip" ]
                            [ div [ class "wallet-panel__data__row__vote-weight" ]
                                [ div [ class "wallet-panel__data__row__vote-weight__fill", style "width" voteWeight ] []
                                , div [ class "tooltip__text" ]
                                    [ p [ class "tooltip__text__headline" ]
                                        [ text formattedVotes
                                        , span [ class "wallet-panel__data__row__vote-weight__separator" ] [ text " / " ]
                                        , text formattedThreshold
                                        ]
                                    , p [ class "tooltip__text__subtext wallet-panel__data__row__vote-weight__tooltip" ] [ text (Translations.votes_received_description userLanguage (Decimal.toString proposalThreshold)) ]
                                    , a ([ class "tooltip__text__link", target "__blank" ] ++ href External docsUrl) [ text (Translations.learn_more userLanguage) ]
                                    ]
                                ]
                            ]

                conditionalView =
                    if Decimal.eq votes_ Decimal.zero && delegateType == Undelegated then
                        [ div [ class "wallet-panel__data__row" ]
                            [ div [ class "wallet-panel__data__row__setup-title" ] [ text (Translations.setup_voting userLanguage) ]
                            , p [ class "wallet-panel__data__row__setup-description" ]
                                [ text (Translations.setup_voting_description userLanguage)
                                , a (target "_blank" :: href External "https://medium.com/compound-finance/compound-governance-5531f524cf68") [ text (Translations.learn_more userLanguage ++ ".") ]
                                ]
                            , div [ class "button main wallet-panel__data__row__button wallet-panel__data__row__button--undelegated", onClick (ForSelf (SetDelegateModal SelectDelegationType)) ]
                                [ div [ class "wallet-panel__data__row__button__text" ]
                                    [ text (Translations.get_started userLanguage) ]
                                ]
                            ]
                        ]

                    else
                        [ div [ class "wallet-panel__data__row" ]
                            [ label [ class "wallet-panel__data__row__label" ] [ text (Translations.delegating_to userLanguage) ]
                            , div [ class "wallet-panel__data__row__with-icon" ]
                                [ delegateImage
                                , delegateP
                                , delegateButton
                                ]
                            ]
                        , div [ class "wallet-panel__data__row" ]
                            [ label [ class "wallet-panel__data__row__label" ] [ text (Translations.votes_received userLanguage) ]
                            , p [ class "wallet-panel__data__row__value" ]
                                votesSpan
                            , createProposalViewOrMeter
                            ]
                        ]
            in
            div [ class "col-md-4 wallet-panel" ]
                [ div [ class "panel" ]
                    [ div [ class "panel__header" ]
                        [ h4 [] [ text (Translations.voting_wallet userLanguage) ] ]
                    , div [ class "wallet-panel__data" ]
                        ([ div [ class "wallet-panel__data__row" ]
                            [ label [ class "wallet-panel__data__row__label" ] [ text (Translations.votes_page_comp_balance userLanguage) ]
                            , div [ class "wallet-panel__data__row__with-icon" ]
                                [ p [ class "wallet-panel__data__row__value" ]
                                    balanceSpans
                                , img [ class "wallet-panel__data__row__comp", src "./images/comp-icn.svg" ] []
                                ]
                            ]
                         ]
                            ++ conditionalView
                        )
                    ]
                ]

        _ ->
            loadingVotingWalletPanel userLanguage


loadingProposalsPanel : Translations.Lang -> Html Msg
loadingProposalsPanel userLanguage =
    div [ class "col-md-8 proposals-panel" ]
        [ div [ class "panel" ]
            ([ div [ class "panel__header" ]
                [ h4 [] [ text (Translations.active_proposals userLanguage) ] ]
             , div [ class "panel__labels" ]
                [ div [ class "col-xs-6 text-left" ] [ label [] [ text (Translations.title userLanguage) ] ]
                , div [ class "col-xs-6 text-right" ] [ label [ class "proposals-panel__labels__action" ] [ text (Translations.action userLanguage) ] ]
                ]
             ]
                ++ List.repeat 5 loadingProposalRow
            )
        ]


proposalsPanel : Translations.Lang -> Time.Zone -> Maybe Time.Posix -> Config -> Network -> Account -> Model -> Dict String Transaction -> Html Msg
proposalsPanel userLanguage timezone maybeCurrentTime config network account model voteTransactionsByProposalId =
    let
        proposalRows =
            case model.proposals of
                Just proposals ->
                    let
                        currentProposalsForPage =
                            proposals
                                |> List.drop
                                    ((model.currentProposalsPageNumber - 1)
                                        * desiredPageSize
                                    )
                                |> List.take desiredPageSize

                        totalPages =
                            (toFloat (List.length proposals)
                                / toFloat desiredPageSize
                            )
                                |> ceiling

                        pagerFooter =
                            if totalPages > 1 then
                                let
                                    prevButton =
                                        if model.currentProposalsPageNumber > 1 then
                                            a [ class "panel__pager__previous", onClickStopPropagation (ForSelf ProposalsPagerPrevious) ]
                                                [ label [] [ text (Translations.prev userLanguage) ]
                                                ]

                                        else
                                            span [] [ text "" ]

                                    pagerIndicator =
                                        div [ class "panel__pager__indicator" ]
                                            (List.range 1 totalPages
                                                |> List.map
                                                    (\i ->
                                                        let
                                                            activeClass =
                                                                if model.currentProposalsPageNumber == i then
                                                                    " panel__pager__indicator__text--active"

                                                                else
                                                                    ""
                                                        in
                                                        span [ class ("panel__pager__indicator__text" ++ activeClass), onClickStopPropagation (ForSelf (SetProposalsPager i)) ] [ text (String.fromInt i) ]
                                                    )
                                            )

                                    nextButton =
                                        if model.currentProposalsPageNumber < totalPages then
                                            a [ class "panel__pager__next", onClickStopPropagation (ForSelf ProposalsPagerNext) ]
                                                [ label [] [ text (Translations.next userLanguage) ]
                                                ]

                                        else
                                            span [] [ text "" ]
                                in
                                [ div [ class "panel__pager" ]
                                    [ prevButton
                                    , pagerIndicator
                                    , nextButton
                                    ]
                                ]

                            else
                                [ div [ class "panel__pager" ]
                                    [ a ([ class "panel__pager__indicator panel__pager__indicator--clickable" ] ++ href External (landingUrlForPage network GovernanceProposalOverview))
                                        [ label [] [ text (Translations.all_proposals userLanguage) ]
                                        ]
                                    ]
                                ]
                    in
                    (if List.isEmpty currentProposalsForPage then
                        [ noActiveProposalRow userLanguage ]

                     else
                        currentProposalsForPage
                            |> List.map
                                (\proposal ->
                                    voteProposalRow userLanguage timezone maybeCurrentTime config network account model voteTransactionsByProposalId proposal
                                )
                    )
                        ++ pagerFooter

                Nothing ->
                    List.repeat 5 loadingProposalRow
    in
    div [ class "col-md-8 proposals-panel" ]
        [ div [ class "panel" ]
            ([ div [ class "panel__header" ]
                [ h4 [] [ text (Translations.active_proposals userLanguage) ] ]
             ]
                ++ proposalRows
            )
        ]


voteProposalRow : Translations.Lang -> Time.Zone -> Maybe Time.Posix -> Config -> Network -> Account -> Model -> Dict String Transaction -> ProposalWithDetail -> Html Msg
voteProposalRow userLanguage timezone maybeCurrentTime config network account model voteTransactionsByProposalId proposal =
    let
        proposalUrl =
            landingUrlForPage network (GovernanceProposalDetail proposal.id)

        maybeVoterReceipt =
            Dict.get (String.fromInt proposal.id) model.proposalVoteReceipts

        maybeVoteType =
            maybeVoterReceipt
                |> Maybe.map .support
                |> Maybe.andThen intToMaybeVoteType

        priorVotes =
            Dict.get (String.fromInt proposal.id) model.priorVotes
                |> Maybe.withDefault Decimal.zero

        ( forPercent, againstPercent, abstainPercent ) =
            proposalPercentages proposal

        detailView =
            let
                maybeCurrentState =
                    proposal.states |> List.reverse |> List.head |> Maybe.map .state
            in
            case Dict.get (String.fromInt proposal.id) voteTransactionsByProposalId of
                Just transaction ->
                    if transaction.status == Eth.Transaction.Pending then
                        div [ class "proposal__receipt-support proposal__receipt-support--pending-vote" ]
                            [ p [ class "proposal__receipt-support__text" ] [ text (Translations.pending userLanguage) ] ]

                    else
                        text ""

                Nothing ->
                    case maybeCurrentState of
                        Just ProposalServiceModels.Pending ->
                            if network == Ropsten then
                                div [ class "button proposal__queue-actions-button", onClickStopPropagation (ForSelf (SetVoteModal (SelectVoteType proposal Nothing ""))) ] [ text (Translations.vote userLanguage) ]

                            else
                                text ""

                        Just Active ->
                            if Decimal.gt priorVotes Decimal.zero && maybeVoterReceipt == Nothing then
                                div [ class "button proposal__queue-actions-button", onClickStopPropagation (ForSelf (SetVoteModal (SelectVoteType proposal Nothing ""))) ] [ text (Translations.vote userLanguage) ]

                            else
                                text ""

                        Just Succeeded ->
                            case ( config.maybeGovernor, account ) of
                                ( Just governor, Acct customer _ ) ->
                                    div [ class "button main proposal__queue-execute-button", onClickStopPropagation (WrappedGovernanceMsg (Eth.Governance.QueueProposal governor customer proposal.id)) ]
                                        [ text (Translations.queue userLanguage) ]

                                _ ->
                                    text ""

                        Just Queued ->
                            let
                                maybeRawProposalState =
                                    proposal.states |> List.reverse |> List.head
                            in
                            case ( config.maybeGovernor, account, maybeRawProposalState ) of
                                ( Just governor, Acct customer _, Just rawProposalState ) ->
                                    case ( maybeCurrentTime, rawProposalState.end_time ) of
                                        ( Just time, Just endTime ) ->
                                            if CompoundComponents.Utils.Time.posixToSeconds time > CompoundComponents.Utils.Time.posixToSeconds endTime then
                                                div [ class "button main proposal__queue-execute-button", onClickStopPropagation (WrappedGovernanceMsg (Eth.Governance.ExecuteProposal governor customer proposal.id)) ]
                                                    [ text (Translations.execute userLanguage) ]

                                            else
                                                text ""

                                        _ ->
                                            text ""

                                _ ->
                                    text ""

                        _ ->
                            text ""
    in
    a ([ class "proposal vote-proposal", target "__blank" ] ++ href External proposalUrl)
        [ div [ class "vote-proposal__top" ]
            [ proposalContentView userLanguage timezone maybeCurrentTime proposal False
            , div [ class "vote-proposal__top__buttons" ]
                [ detailView
                ]
            ]
        , div [ class "vote-proposal__bottom" ]
            [ div [ class "row" ]
                [ div [ class "col-sm-4" ]
                    [ voteTallyCard userLanguage For forPercent (maybeVoteType == Just For)
                    ]
                , div [ class "col-sm-4" ]
                    [ voteTallyCard userLanguage Against againstPercent (maybeVoteType == Just Against)
                    ]
                , div [ class "col-sm-4" ]
                    [ voteTallyCard userLanguage Abstain abstainPercent (maybeVoteType == Just Abstain)
                    ]
                ]
            ]
        ]


noActiveProposalRow : Translations.Lang -> Html msg
noActiveProposalRow userLanguage =
    div [ class "proposal proposal--empty-vote" ]
        [ div [ class "proposal__content proposal__content--empty" ] [ text (Translations.no_current_active_proposals userLanguage) ]
        , div [ class "proposal__receipt-support proposal__receipt-support--view-only" ]
            [ p [ class "proposal__receipt-support__text proposal__receipt-support__text--empty" ] [ text "—" ] ]
        ]


voteTallyCard : Translations.Lang -> VoteType -> String -> Bool -> Html msg
voteTallyCard userLanguage voteType percentage isSelected =
    let
        selectedVoteClass =
            if isSelected then
                " tally-card--vote-receipt"

            else
                ""

        ( labelText, tallyClass ) =
            case voteType of
                For ->
                    ( Translations.for userLanguage, " tally-card--for" )

                Against ->
                    ( Translations.against userLanguage, " tally-card--against" )

                Abstain ->
                    ( Translations.abstain userLanguage, "" )
    in
    div [ class ("tally-card" ++ tallyClass ++ selectedVoteClass) ]
        [ div [ class "tally-card__details" ]
            [ p [ class "tally-card__details__label" ] [ text labelText ]
            , p [ class "tally-card__details__value" ] [ text percentage ]
            ]
        , div [ class "tally-card__meter" ]
            [ div [ class "tally-card__meter__fill", style "width" percentage ] []
            ]
        ]


proposalPercentages : ProposalWithDetail -> ( String, String, String )
proposalPercentages proposal =
    let
        total =
            Decimal.add proposal.for_votes proposal.against_votes
                |> Decimal.add proposal.abstain_votes

        for =
            Decimal.fastdiv proposal.for_votes total
                |> Maybe.withDefault Decimal.zero
                |> formatPercentageToNearestWhole

        against =
            Decimal.fastdiv proposal.against_votes total
                |> Maybe.withDefault Decimal.zero
                |> formatPercentageToNearestWhole

        abstain =
            Decimal.fastdiv proposal.abstain_votes total
                |> Maybe.withDefault Decimal.zero
                |> formatPercentageToNearestWhole
    in
    ( for, against, abstain )



-- Ports


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ giveVoteDashboardData (handleError (ForSelf << Error << Json.Decode.errorToString) (ForSelf << VoteDashboardDataResult))
        ]


port getVoteDashboardDataPort : { governorAddress : String, isBravo : Bool, compoundLens : String, governanceTokenAddress : String, decimals : Int, initialBlockNumber : Int, currentBlockNumber : Int, voter : String, network : String } -> Cmd msg


getVoteDashboardData : Dict String Config -> Maybe Network -> Maybe Int -> Account -> Cmd msg
getVoteDashboardData configs maybeNetwork maybeCurrentBlockNumber account =
    let
        nameOfNetwork =
            case maybeNetwork of
                Just network ->
                    String.toLower (networkName network)

                Nothing ->
                    String.toLower (networkName MainNet)

        getTrxsCmd =
            case ( Dict.get nameOfNetwork configs, account, maybeCurrentBlockNumber ) of
                ( Just config, Acct (Customer voter) _, Just currentBlockNumber ) ->
                    case ( config.maybeGovernor, config.maybeCompToken ) of
                        ( Just ( governorAddress, isBravo ), Just comp ) ->
                            let
                                initialBlock =
                                    if isBravo then
                                        Dict.get "GovernorBravo" config.blocks

                                    else
                                        Dict.get "GovernorAlpha" config.blocks
                            in
                            initialBlock
                                |> Maybe.map
                                    (\blockNumber ->
                                        getVoteDashboardDataPort
                                            { governorAddress = getContractAddressString governorAddress
                                            , isBravo = isBravo
                                            , compoundLens = getContractAddressString config.compoundLens
                                            , governanceTokenAddress = getContractAddressString comp.address
                                            , initialBlockNumber = blockNumber
                                            , currentBlockNumber = currentBlockNumber
                                            , voter = voter
                                            , decimals = comp.decimals
                                            , network = nameOfNetwork
                                            }
                                    )
                                |> Maybe.withDefault Cmd.none

                        _ ->
                            Cmd.none

                _ ->
                    Cmd.none
    in
    getTrxsCmd


port giveVoteDashboardDataPort : (Json.Decode.Value -> msg) -> Sub msg


giveVoteDashboardData : (Result Json.Decode.Error VoterDashboardData -> msg) -> Sub msg
giveVoteDashboardData wrapper =
    let
        receiptDecoder =
            map3 VoterReceipt
                (field "support" int)
                (field "votes" decimal)
                (field "proposal_id" int)

        decoder =
            map5 VoterDashboardData
                (field "proposals" (list proposalWithDetailDecoder))
                (field "proposalVoteReceipts" (dict receiptDecoder))
                (field "priorVotes" (dict decimal))
                (field "network" string)
                (field "voter" string)
    in
    giveVoteDashboardDataPort
        (Json.Decode.decodeValue decoder >> wrapper)


port governanceVoteProposalPort : { adminAddress : String, governorAddress : String, isBravo : Bool, proposalId : Int, supportValue : Int, reason : String } -> Cmd msg


voteProposal : CustomerAddress -> String -> Bool -> Int -> VoteType -> String -> Cmd msg
voteProposal (Customer adminAddress) governorAddress isBravo proposalId voteType reason =
    let
        supportValue =
            voteTypeAsInt voteType
    in
    governanceVoteProposalPort
        { adminAddress = adminAddress
        , governorAddress = governorAddress
        , isBravo = isBravo
        , proposalId = proposalId
        , supportValue = supportValue
        , reason = reason
        }
