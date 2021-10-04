port module Main exposing (Flags, badNetworkAlert, init, invalidNetwork, liquidateTranslator, main, newBlockCmd, subscriptions, update, view)

import Admin
import Browser
import Browser.Events
import Browser.Navigation
import CompoundApi.GasService.Decoders
import CompoundApi.GasService.Models
import CompoundApi.GasService.Urls
import CompoundComponents.Console as Console
import CompoundComponents.DisplayCurrency exposing (DisplayCurrency(..))
import CompoundComponents.Eth.ConnectedEthWallet as ConnectedEthWallet exposing (tryConnect)
import CompoundComponents.Eth.Ethereum as Ethereum exposing (Account(..), AssetAddress(..), ContractAddress(..), CustomerAddress(..))
import CompoundComponents.Eth.Ledger exposing (LedgerAccount(..))
import CompoundComponents.Eth.Network as Network exposing (Network(..), networkId, networkName)
import CompoundComponents.Ether.BNTransaction as BNTransaction exposing (BNTransactionMsg)
import CompoundComponents.Ether.Helpers
import CompoundComponents.Functions exposing (handleError)
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (HrefLinkType(..), class, href, id, target)
import CompoundComponents.Utils.Time
import DappInterface.ClaimCompModal as ClaimCompModal
import DappInterface.CollateralToggleModal as CollateralToggleModal
import DappInterface.CommonViews as CommonViews exposing (pageFooter, pageHeader)
import DappInterface.Container
import DappInterface.MainModel exposing (ChooseWalletModalState(..), Model, PrimaryActionType(..), getConfig, getCurrentConfig, getProvider)
import DappInterface.Page exposing (Page(..), getPage, getPageTitle)
import DappInterface.PrimaryActionModal
import DappInterface.Propose as Propose
import DappInterface.Terms as DappTerms
import DappInterface.Vote as Vote
import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Eth.Compound exposing (CompoundMsg(..), clearCompoundState, compoundInit, compoundNewBlockCmd, compoundSubscriptions, compoundUpdate)
import Eth.Config exposing (Config, loadConfigs)
import Eth.Governance exposing (GovernanceMsg(..))
import Eth.Oracle exposing (OracleMsg(..), oracleInit, oracleNewBlockCmd, oracleSubscriptions, oracleUpdate)
import Eth.Token exposing (TokenMsg(..), clearTokenState, tokenInit, tokenNewBlockCmd, tokenSubscriptions, tokenUpdate)
import Eth.Transaction as Transaction exposing (TransactionMsg)
import Html exposing (Html, a, button, div, span, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode
import Json.Encode
import Liquidate
import Port exposing (askNetwork, askNewBlock, askSetBlockNativeNetworkPort, giveAccountBalance, giveError, giveNewBlock, setGasPrice, setTitle)
import Preferences exposing (PreferencesMsg(..), preferencesInit, preferencesSubscriptions, preferencesUpdate)
import Repl
import Strings.Translations as Translations
import Task
import Time
import Tuple
import Url
import Utils.BrowserInfo
import Utils.Http


type Msg
    = UrlChange (Maybe Url.Url)
    | ConnectedEthWalletMsg ConnectedEthWallet.InternalMsg
    | ContainerInternalMsg DappInterface.Container.InternalMsg
    | ContainerMsg DappInterface.Container.ParentMsg
    | CommonViewsMsg CommonViews.InternalMsg
    | ClaimCompModalMsg ClaimCompModal.InternalMsg
    | HideAndResetConnectModal Bool
    | ConnectModalBack
    | CollateralToggleModalParentMsg CollateralToggleModal.ParentMsg
    | PrimaryActionModalMsg DappInterface.PrimaryActionModal.InternalMsg
    | PrimaryActionModalParentMsg DappInterface.PrimaryActionModal.ParentMsg
    | ProposeMsg Propose.InternalMsg
    | SetAccountBalance Decimal
    | SetBlockNumber Int
    | Error String
    | KeyPress String
    | WrappedTransactionMsg TransactionMsg
    | WrappedBNTransactionMsg BNTransactionMsg
    | WrappedTokenMsg TokenMsg
    | WrappedCompoundMsg CompoundMsg
    | WrappedOracleMsg OracleMsg
    | AdminMsg Admin.InternalMsg
    | ReplMsg Repl.InternalMsg
    | WrappedPreferencesMsg PreferencesMsg
    | LiquidateMsg Liquidate.InternalMsg
    | WrappedGovernanceMsg GovernanceMsg
    | Tick Time.Posix
    | SetTimeZone Time.Zone
    | VoteMsg Vote.InternalMsg
    | CheckVersion Time.Posix
    | CheckedVersion (Result Http.Error Float)
    | RefreshGasPrice (Result Http.Error CompoundApi.GasService.Models.API_GasPriceResponse)


type alias Flags =
    { path : String
    , configurations : Json.Encode.Value
    , configAbiFiles : Json.Encode.Value
    , dataProviders : Json.Encode.Value
    , apiBaseUrlMap : Json.Encode.Value
    , userAgent : Json.Encode.Value
    , providerType : Json.Encode.Value
    , language : String
    }


connectedEthWalletTranslator : ConnectedEthWallet.Translator Msg
connectedEthWalletTranslator =
    ConnectedEthWallet.translator
        { onInternalMessage = ConnectedEthWalletMsg
        }


adminTranslator : Admin.Translator Msg
adminTranslator =
    Admin.translator
        { onInternalMessage = AdminMsg
        }


claimCompModalTranslator : ClaimCompModal.Translator Msg
claimCompModalTranslator =
    ClaimCompModal.translator
        { onInternalMessage = ClaimCompModalMsg
        , onWrappedGovernanceMessage = WrappedGovernanceMsg
        }


commonViewsTranslator : CommonViews.Translator Msg
commonViewsTranslator =
    CommonViews.translator
        { onInternalMessage = CommonViewsMsg
        , onWrappedPreferencesdMsg = WrappedPreferencesMsg
        , onAccountButtonClicked = ContainerMsg DappInterface.Container.AccountAddressClicked
        , onCompButtonClicked = ContainerMsg DappInterface.Container.CompButtonClicked
        }


replTranslator : Repl.Translator Msg
replTranslator =
    Repl.translator
        { onInternalMessage = ReplMsg
        }


liquidateTranslator : Liquidate.Translator Msg
liquidateTranslator =
    Liquidate.translator
        { onInternalMessage = LiquidateMsg
        , onWrappedTokenMsg = WrappedTokenMsg
        }


proposeTranslator : Propose.Translator Msg
proposeTranslator =
    Propose.translator
        { onInternalMessage = ProposeMsg
        }


voteTranslator : Vote.Translator Msg
voteTranslator =
    Vote.translator
        { onInternalMessage = VoteMsg
        , onWrappedGovernanceMessage = WrappedGovernanceMsg
        , onWrappedTokenMessage = WrappedTokenMsg
        }


containerTranslator : DappInterface.Container.Translator Msg
containerTranslator =
    DappInterface.Container.translator
        { onContainerParentMsg = ContainerMsg
        , onInternalMsg = ContainerInternalMsg
        , onWrappedCompoundMsg = WrappedCompoundMsg
        , onWrappedPreferencesMsg = WrappedPreferencesMsg
        }


collateralToggleModalTranslator : CollateralToggleModal.Translator Msg
collateralToggleModalTranslator =
    CollateralToggleModal.translator
        { onParentMsg = CollateralToggleModalParentMsg
        , onWrappedCompoundMsg = WrappedCompoundMsg
        }


primaryActionModalTranslator : DappInterface.PrimaryActionModal.Translator Msg
primaryActionModalTranslator =
    DappInterface.PrimaryActionModal.translator
        { onInternalMessage = PrimaryActionModalMsg
        , onParentMsg = PrimaryActionModalParentMsg
        , onWrappedCompoundMsg = WrappedCompoundMsg
        , onWrappedTokenMsg = WrappedTokenMsg
        }


init : Flags -> ( Model, Cmd Msg )
init { path, configurations, configAbiFiles, dataProviders, apiBaseUrlMap, userAgent, providerType, language } =
    let
        initialPage =
            Url.fromString path
                |> Maybe.map getPage
                |> Maybe.withDefault Home

        configs =
            loadConfigs configurations
                |> Result.withDefault Dict.empty

        decodedApiBaseUrlMap =
            Json.Decode.decodeValue (Json.Decode.dict Json.Decode.string) apiBaseUrlMap
                |> Result.withDefault Dict.empty

        browserType =
            Json.Decode.decodeValue Json.Decode.string userAgent
                |> Result.map Utils.BrowserInfo.detectBrowser
                |> Result.withDefault Utils.BrowserInfo.Desktop

        providerTypeString =
            Json.Decode.decodeValue Json.Decode.string providerType
                |> Result.withDefault ""

        ( initConnectedEthWalletModel, initConnectedEthWalletCmd ) =
            ConnectedEthWallet.init False providerTypeString

        ( initCommonViewsModel, initCommonViewsCmd ) =
            CommonViews.init

        ( initTransactionState, initTransactionCmd ) =
            Transaction.init

        ( initBNTransactionState, initBNTransactionCmd ) =
            BNTransaction.init

        ( initCompoundState, initCompoundCmd ) =
            compoundInit

        ( initOracleState, initOracleCmd ) =
            oracleInit

        ( initPreferences, initPreferencesCmd ) =
            preferencesInit (Translations.getLnFromCode language)

        ( initVoteModel, _ ) =
            Vote.init configs

        initChooseLedgerAccountModal =
            { isVisible = False
            , pathSelectorActive = False
            , addressSelectorActive = False
            , chooseLedgerInLegacyMode = True
            , choosenLedgerAccount = Nothing
            }
    in
    ( { page = initialPage
      , adminModel = Admin.emptyState
      , liquidateModel = Liquidate.emptyState
      , appVersion = Nothing
      , apiBaseUrlMap = decodedApiBaseUrlMap
      , configs = configs
      , configAbis = configAbiFiles
      , dataProviders = Result.withDefault Dict.empty (Json.Decode.decodeValue (Json.Decode.dict Json.Decode.string) dataProviders)
      , account = NoAccount
      , network = Nothing
      , commonViewsModel = initCommonViewsModel
      , connectedEthWalletModel = initConnectedEthWalletModel
      , claimCompModalState = ClaimCompModal.init
      , borrowingContainerState = DappInterface.Container.init
      , collateralToggleModalState = Nothing
      , primaryActionModalState = Nothing
      , blockNumber = Nothing
      , transactionState = initTransactionState
      , bnTransactionState = initBNTransactionState
      , tokenState = Eth.Token.emptyState
      , compoundState = initCompoundState
      , oracleState = initOracleState
      , preferences = initPreferences
      , governanceState = Eth.Governance.init
      , errors = []
      , currentTime = Nothing
      , currentTimeZone = Time.utc
      , browserType = browserType
      , proposeModel = Propose.emptyState
      , voteModel = initVoteModel
      , maybeGasPrice = Nothing
      , userLanguage = initPreferences.userLanguage
      , repl = Repl.emptyState
      }
    , Cmd.batch
        [ setTitle (getPageTitle initPreferences.userLanguage initialPage)
        , initCommonViewsCmd
        , tryConnect (browserType == Utils.BrowserInfo.Desktop)
        , askNetwork
        , askNewBlock
        , Cmd.map connectedEthWalletTranslator initConnectedEthWalletCmd
        , Cmd.map WrappedPreferencesMsg initPreferencesCmd
        , Cmd.map WrappedBNTransactionMsg initBNTransactionCmd
        , Task.perform Tick Time.now
        , Task.perform CheckVersion Time.now
        , Task.perform SetTimeZone Time.here
        ]
    )


newNetworkCmd : Network -> Model -> Cmd Msg
newNetworkCmd newNetwork model =
    Cmd.batch
        [ Cmd.map WrappedTransactionMsg (Tuple.second Transaction.init)
        , Cmd.map WrappedCompoundMsg (Tuple.second compoundInit)
        , Cmd.map WrappedOracleMsg (Tuple.second oracleInit)
        , Cmd.map WrappedBNTransactionMsg (BNTransaction.newNetworkCmd newNetwork model.bnTransactionState)
        ]


newBlockCmd : Dict String String -> Maybe Network -> Int -> Maybe Int -> Model -> Cmd Msg
newBlockCmd apiBaseUrlMap maybeNetwork blockNumber previousBlockNumber ({ dataProviders, configs, page } as model) =
    case maybeNetwork of
        Just network ->
            case ( getConfig configs network, getProvider dataProviders network ) of
                ( Just config, Just endpoint ) ->
                    let
                        pageCmds =
                            case page of
                                Home ->
                                    [ Cmd.map WrappedGovernanceMsg (Eth.Governance.newBlockCmd config blockNumber model.account Nothing)
                                    ]

                                Vote ->
                                    [ Vote.getVoteDashboardData configs (Just network) (Just blockNumber) model.account
                                    , Cmd.map WrappedGovernanceMsg (Eth.Governance.newBlockCmd config blockNumber model.account Nothing)
                                    ]

                                _ ->
                                    []

                        compAllowanceCmd =
                            case ( config.maybeCompToken, config.maybeCrowdProposalFactory, model.account ) of
                                ( Just compToken, Just capFactory, Acct customerAddress _ ) ->
                                    Cmd.map WrappedTokenMsg (Eth.Token.askCompCapFactoryAllowance blockNumber customerAddress capFactory compToken.address)

                                _ ->
                                    Cmd.none
                    in
                    Cmd.batch <|
                        pageCmds
                            ++ [ Cmd.map WrappedTransactionMsg (Transaction.newBlockCmd blockNumber network model.transactionState)
                               , Cmd.map WrappedTokenMsg (tokenNewBlockCmd config model.tokenState blockNumber model.account)
                               , Cmd.map WrappedCompoundMsg (compoundNewBlockCmd blockNumber apiBaseUrlMap network config.comptroller model.account config)
                               , Cmd.map WrappedOracleMsg (oracleNewBlockCmd model.oracleState blockNumber config.priceOracle model.tokenState config.compoundLens)
                               , compAllowanceCmd
                               ]

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


handleUpdatesFromEthConnectedWallet : Maybe Config -> ConnectedEthWallet.InternalMsg -> Model -> ( Model, Cmd Msg )
handleUpdatesFromEthConnectedWallet maybeConfig connectedEthWalletMsg model =
    case connectedEthWalletMsg of
        ConnectedEthWallet.SetNetwork (Just newNetwork) ->
            let
                maybeNewConfig =
                    getConfig model.configs newNetwork

                ( newTokenState, newTokenCmd ) =
                    case maybeNewConfig of
                        Just config ->
                            tokenInit config

                        Nothing ->
                            ( Eth.Token.emptyState, Cmd.none )

                ( newLiquidateModel, newLiquidateCmd ) =
                    case maybeNewConfig of
                        Just config ->
                            Liquidate.init (model.page == Liquidate) config model.account model.blockNumber model.apiBaseUrlMap newNetwork

                        Nothing ->
                            ( Liquidate.emptyState, Cmd.none )

                newVoteModel =
                    if model.network /= Just newNetwork then
                        Vote.emptyState

                    else
                        model.voteModel

                setBlockNativeCmd =
                    askSetBlockNativeNetworkPort
                        { networkId = networkId newNetwork
                        }

                cmd =
                    Cmd.batch
                        ([ setBlockNativeCmd
                         , refreshLatestGasPrice model.apiBaseUrlMap newNetwork
                         , Cmd.map WrappedTokenMsg newTokenCmd
                         , Cmd.map liquidateTranslator newLiquidateCmd
                         ]
                            ++ (case model.blockNumber of
                                    Just blockNumber ->
                                        [ newNetworkCmd newNetwork model
                                        , newBlockCmd model.apiBaseUrlMap (Just newNetwork) blockNumber Nothing model
                                        ]

                                    Nothing ->
                                        [ newNetworkCmd newNetwork model ]
                               )
                        )
            in
            ( { model
                | network = Just newNetwork
                , tokenState = newTokenState
                , liquidateModel = newLiquidateModel
                , voteModel = newVoteModel
              }
            , cmd
            )

        ConnectedEthWallet.SetNetwork Nothing ->
            ( { model | network = Nothing }, Cmd.none )

        ConnectedEthWallet.SetAccount Nothing ->
            ( { model | account = NoAccount, compoundState = clearCompoundState model.compoundState }, Cmd.none )

        ConnectedEthWallet.SetAccount (Just newAccount) ->
            let
                -- If this is an account switching and we have an existing block and network we want to clear some
                -- internal models and trigger a newBlockCmd to refresh everything immediately.
                ( isAccountSwitch, updatedModelFromSetAccount ) =
                    let
                        newAccountModel =
                            { model | account = Acct newAccount Nothing }
                    in
                    case ( model.account, model.blockNumber ) of
                        ( Acct existingAccount _, Just currentBlockNumber ) ->
                            if Ethereum.getCustomerAddressString existingAccount /= Ethereum.getCustomerAddressString newAccount then
                                ( True, { newAccountModel | compoundState = clearCompoundState model.compoundState, tokenState = clearTokenState model.tokenState } )

                            else
                                ( False, newAccountModel )

                        ( NoAccount, _ ) ->
                            ( True, { newAccountModel | compoundState = clearCompoundState model.compoundState, tokenState = clearTokenState model.tokenState } )

                        _ ->
                            ( False, newAccountModel )

                pageCmd =
                    case model.page of
                        Home ->
                            case ( maybeConfig, model.blockNumber ) of
                                ( Just config, Just blockNumber ) ->
                                    Cmd.batch
                                        [ Cmd.map WrappedGovernanceMsg (Eth.Governance.newBlockCmd config blockNumber model.account Nothing)
                                        ]

                                _ ->
                                    Cmd.none

                        Vote ->
                            case ( maybeConfig, model.blockNumber ) of
                                ( Just config, Just blockNumber ) ->
                                    Cmd.batch
                                        [ Vote.getVoteDashboardData model.configs model.network (Just blockNumber) model.account
                                        , Cmd.map WrappedGovernanceMsg (Eth.Governance.newBlockCmd config blockNumber model.account Nothing)
                                        ]

                                _ ->
                                    Cmd.none

                        Admin ->
                            Admin.getQueuedTransactions model.configs model.network

                        _ ->
                            Cmd.none

                updatedBorrowingContainerState =
                    if isAccountSwitch then
                        DappInterface.Container.handleAccountSwitch model.borrowingContainerState

                    else
                        model.borrowingContainerState

                syncBlockCmd =
                    case ( isAccountSwitch, model.blockNumber ) of
                        ( True, Just blockNumber ) ->
                            newBlockCmd model.apiBaseUrlMap model.network blockNumber Nothing updatedModelFromSetAccount

                        _ ->
                            Cmd.none

                ( updatedRepl, replCmd ) =
                    Repl.update (Repl.SetAccount newAccount) model.repl

                allCmd =
                    Cmd.batch
                        [ syncBlockCmd
                        , Cmd.map replTranslator replCmd
                        , pageCmd
                        ]
            in
            ( { updatedModelFromSetAccount
                | repl = updatedRepl
                , borrowingContainerState = updatedBorrowingContainerState
              }
            , allCmd
            )

        ConnectedEthWallet.ResetToChooseProvider ->
            let
                updatedCommonViewsModel =
                    CommonViews.closeDropdownSelectors model.commonViewsModel
            in
            ( { model
                | commonViewsModel = updatedCommonViewsModel
              }
            , Cmd.none
            )

        ConnectedEthWallet.RequestShowTerms ->
            let
                updatedConnectedEthWalletModel =
                    ConnectedEthWallet.resetModel model.connectedEthWalletModel
            in
            ( { model
                | connectedEthWalletModel = updatedConnectedEthWalletModel
                , page = TermsOfService
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ page, configs, apiBaseUrlMap, account, transactionState, bnTransactionState, tokenState, compoundState, oracleState, network } as model) =
    let
        maybeConfig =
            getCurrentConfig model
    in
    case msg of
        UrlChange Nothing ->
            ( model, Cmd.none )

        UrlChange (Just location) ->
            let
                newPage =
                    getPage location

                newPageCmds =
                    [ setTitle (getPageTitle model.userLanguage newPage)
                    , case newPage of
                        Admin ->
                            Admin.getQueuedTransactions configs network

                        Vote ->
                            case ( maybeConfig, model.blockNumber ) of
                                ( Just config, Just blockNumber ) ->
                                    Cmd.batch
                                        [ Vote.getVoteDashboardData configs network model.blockNumber account
                                        , Cmd.map WrappedGovernanceMsg (Eth.Governance.newBlockCmd config blockNumber model.account Nothing)
                                        ]

                                _ ->
                                    Vote.getVoteDashboardData configs network model.blockNumber account

                        _ ->
                            Cmd.none
                    ]
                        |> Cmd.batch
            in
            ( { model
                | page = getPage location
              }
            , newPageCmds
            )

        ConnectedEthWalletMsg connectedEthWalletMsg ->
            let
                ( updatedConnectedEthWalletModel, connectedEthWalletCmd ) =
                    ConnectedEthWallet.update connectedEthWalletMsg model.connectedEthWalletModel

                intermediateModel =
                    { model
                        | connectedEthWalletModel = updatedConnectedEthWalletModel
                    }

                ( updatedModel, cmdsToRun ) =
                    handleUpdatesFromEthConnectedWallet maybeConfig connectedEthWalletMsg intermediateModel
            in
            ( updatedModel
            , Cmd.batch
                [ Cmd.map connectedEthWalletTranslator connectedEthWalletCmd
                , cmdsToRun
                ]
            )

        ClaimCompModalMsg claimCompModalInternalMsg ->
            let
                ( updatedClaimCompModalState, updatedClaimCompModalCmd ) =
                    ClaimCompModal.update claimCompModalInternalMsg maybeConfig account model.compoundState model.claimCompModalState
            in
            ( { model | claimCompModalState = updatedClaimCompModalState }, Cmd.map claimCompModalTranslator updatedClaimCompModalCmd )

        CommonViewsMsg commonViewsInternalMsg ->
            let
                ( updatedCommonViewsModel, updatedCommonViewsCmd ) =
                    CommonViews.update commonViewsInternalMsg model.commonViewsModel
            in
            ( { model | commonViewsModel = updatedCommonViewsModel }, Cmd.map commonViewsTranslator updatedCommonViewsCmd )

        ContainerInternalMsg containerInternalMsg ->
            let
                updatedBorrowingContainerState =
                    DappInterface.Container.update containerInternalMsg model.borrowingContainerState
            in
            ( { model | borrowingContainerState = updatedBorrowingContainerState }, Cmd.none )

        ContainerMsg DappInterface.Container.AccountAddressClicked ->
            let
                updatedCommonViewsModel =
                    CommonViews.closeDropdownSelectors model.commonViewsModel

                oldConnectedEthWalletModal =
                    model.connectedEthWalletModel

                updatedConnectedEthWalletModel =
                    { oldConnectedEthWalletModal | chooseWalletState = ConnectedEthWallet.ChooseProvider }
            in
            ( { model
                | connectedEthWalletModel = updatedConnectedEthWalletModel
                , commonViewsModel = updatedCommonViewsModel
              }
            , Cmd.none
            )

        ContainerMsg DappInterface.Container.CompButtonClicked ->
            let
                updatedCommonViewsModel =
                    CommonViews.closeDropdownSelectors model.commonViewsModel

                nextModalStep =
                    case ( network, account ) of
                        ( Just actualNetwork, Acct customer _ ) ->
                            case ClaimCompModal.getPendingClaimCompTransaction actualNetwork customer transactionState of
                                Just _ ->
                                    ClaimCompModal.AwaitingClaimTransactionMined

                                _ ->
                                    ClaimCompModal.ShowClaimModal

                        _ ->
                            ClaimCompModal.ShowClaimModal
            in
            ( { model
                | claimCompModalState = ClaimCompModal.updateClaimModalStep nextModalStep model.claimCompModalState
                , commonViewsModel = updatedCommonViewsModel
              }
            , Cmd.none
            )

        ContainerMsg (DappInterface.Container.UseAsCollateralToggleClicked cToken) ->
            let
                updatedCollateralToggleModal =
                    maybeConfig
                        |> Maybe.map
                            (\config ->
                                CollateralToggleModal.prepareToShowModal config model cToken
                            )
            in
            ( { model
                | collateralToggleModalState = updatedCollateralToggleModal
              }
            , Cmd.none
            )

        ContainerMsg protocolActionRequest ->
            let
                ( newPrimaryActionModalState, cmd ) =
                    DappInterface.PrimaryActionModal.prepareToShowModal maybeConfig protocolActionRequest model

                updatedCommonViewsModel =
                    CommonViews.closeDropdownSelectors model.commonViewsModel
            in
            ( { model
                | primaryActionModalState = newPrimaryActionModalState
                , commonViewsModel = updatedCommonViewsModel
              }
            , Cmd.map primaryActionModalTranslator cmd
            )

        HideAndResetConnectModal loadTerms ->
            let
                newPage =
                    if loadTerms then
                        TermsOfService

                    else
                        page

                updatedConnectedEthWalletModel =
                    ConnectedEthWallet.resetModel model.connectedEthWalletModel
            in
            ( { model
                | connectedEthWalletModel = updatedConnectedEthWalletModel
                , page = newPage
              }
            , Cmd.none
            )

        ConnectModalBack ->
            let
                updatedConnectedEthWalletModel =
                    ConnectedEthWallet.handleBack model.connectedEthWalletModel
            in
            ( { model | connectedEthWalletModel = updatedConnectedEthWalletModel }, Cmd.none )

        CollateralToggleModalParentMsg CollateralToggleModal.DismissAndResetCollateralModal ->
            ( { model | collateralToggleModalState = Nothing }
            , Cmd.none
            )

        PrimaryActionModalMsg internal ->
            let
                ( updatedPrimaryActionModal, cmd ) =
                    case model.primaryActionModalState of
                        Just activeActionModalState ->
                            DappInterface.PrimaryActionModal.update model.account compoundState tokenState oracleState internal activeActionModalState
                                |> Tuple.mapBoth Just (Cmd.map primaryActionModalTranslator)

                        Nothing ->
                            ( Nothing, Cmd.none )
            in
            ( { model | primaryActionModalState = updatedPrimaryActionModal }, cmd )

        PrimaryActionModalParentMsg DappInterface.PrimaryActionModal.DismissAndResetActionModal ->
            ( { model | primaryActionModalState = Nothing }, Cmd.none )

        SetAccountBalance balance ->
            case model.account of
                Acct account_ _ ->
                    ( { model | account = Acct account_ (Just balance) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Tick time ->
            let
                updatedBorrowingContainerState =
                    case model.currentTime of
                        Just actualTime ->
                            DappInterface.Container.handleTimeTick model.borrowingContainerState model actualTime

                        Nothing ->
                            model.borrowingContainerState

                ( updatedVoteModel, loadDelegateeCmd ) =
                    Vote.handleShouldLoadCurrentDelegatee apiBaseUrlMap model.network model.account model.governanceState model.voteModel

                updateCompoundState =
                    Eth.Compound.handleAccountLiquidityCalculation oracleState compoundState
            in
            ( { model
                | currentTime = Just time
                , borrowingContainerState = updatedBorrowingContainerState
                , voteModel = updatedVoteModel
                , compoundState = updateCompoundState
              }
            , Cmd.map voteTranslator loadDelegateeCmd
            )

        CheckVersion time ->
            let
                versionUrl =
                    ".__v.json"

                versionDecoder =
                    Json.Decode.field "version" Json.Decode.float
            in
            ( model, Http.send CheckedVersion (Http.get versionUrl versionDecoder) )

        SetTimeZone timeZone ->
            ( { model | currentTimeZone = timeZone }, Cmd.none )

        CheckedVersion (Ok latestVersion) ->
            case model.appVersion of
                Just currentVersion ->
                    if latestVersion /= currentVersion then
                        ( model, Browser.Navigation.reload )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( { model | appVersion = Just latestVersion }, Cmd.none )

        CheckedVersion (Err e) ->
            ( model, Cmd.none )

        SetBlockNumber blockNumber ->
            ( { model | blockNumber = Just blockNumber }, newBlockCmd apiBaseUrlMap model.network blockNumber model.blockNumber model )

        WrappedTransactionMsg transactionMsg ->
            let
                ( updatedTransactionState, transactionCmd ) =
                    Transaction.update network account bnTransactionState transactionMsg transactionState

                mainModelWithTransactionStateUpdated =
                    { model | transactionState = updatedTransactionState }

                updatedCollateralToggleModal =
                    case model.collateralToggleModalState of
                        Just activeCollateralToggleModal ->
                            CollateralToggleModal.handleNonBNTransactionUpdate maybeConfig transactionMsg activeCollateralToggleModal mainModelWithTransactionStateUpdated

                        Nothing ->
                            Nothing

                -- Let the primary action modal handle any updates it needs for new transactions
                updatedPrimaryActionModal =
                    case model.primaryActionModalState of
                        Just activeActionModalState ->
                            DappInterface.PrimaryActionModal.handleTransactionUpdate maybeConfig transactionMsg activeActionModalState mainModelWithTransactionStateUpdated
                                |> Just

                        Nothing ->
                            Nothing

                updatedVoteModel =
                    Vote.handleTransactionUpdate maybeConfig network account updatedTransactionState transactionMsg model.voteModel

                updatedClaimCompModalState =
                    ClaimCompModal.handleTransactionUpdate maybeConfig network account updatedTransactionState transactionMsg model.claimCompModalState
            in
            ( { mainModelWithTransactionStateUpdated
                | claimCompModalState = updatedClaimCompModalState
                , collateralToggleModalState = updatedCollateralToggleModal
                , primaryActionModalState = updatedPrimaryActionModal
                , voteModel = updatedVoteModel
              }
            , Cmd.map WrappedTransactionMsg transactionCmd
            )

        WrappedBNTransactionMsg bnTransactionMsg ->
            let
                ( updatedBNTransactionState, bnTransactionCmd ) =
                    BNTransaction.update network account bnTransactionMsg bnTransactionState

                mainModelWithTransactionStateUpdated =
                    { model | bnTransactionState = updatedBNTransactionState }

                updatedCollateralToggleModal =
                    case model.collateralToggleModalState of
                        Just activeCollateralToggleModal ->
                            CollateralToggleModal.handleBNTransactionUpdate maybeConfig bnTransactionMsg activeCollateralToggleModal mainModelWithTransactionStateUpdated

                        Nothing ->
                            Nothing

                updatedPrimaryActionModal =
                    case model.primaryActionModalState of
                        Just activeActionModalState ->
                            DappInterface.PrimaryActionModal.handleBNTransactionUpdate maybeConfig bnTransactionMsg activeActionModalState mainModelWithTransactionStateUpdated
                                |> Just

                        Nothing ->
                            Nothing

                -- if we get a transaction update on a future block then jumpstart the new block cmd
                -- so we can quickly refresh the UI.
                triggerImmediateNewBlockCmd =
                    case ( model.blockNumber, bnTransactionMsg ) of
                        ( Just currentBlock, BNTransaction.TransactionStateChange ( txModule, _, statusUpdate ) ) ->
                            statusUpdate.maybeBlockNumber
                                |> Maybe.andThen
                                    (\trxBlockNumber ->
                                        if BNTransaction.getUserTxModule model.network model.account == txModule && trxBlockNumber > currentBlock then
                                            Task.perform SetBlockNumber (Task.succeed trxBlockNumber)
                                                |> Just

                                        else
                                            Nothing
                                    )
                                |> Maybe.withDefault Cmd.none

                        _ ->
                            Cmd.none
            in
            ( { mainModelWithTransactionStateUpdated
                | collateralToggleModalState = updatedCollateralToggleModal
                , primaryActionModalState = updatedPrimaryActionModal
              }
            , Cmd.batch
                [ Cmd.map WrappedBNTransactionMsg bnTransactionCmd
                , triggerImmediateNewBlockCmd
                ]
            )

        WrappedTokenMsg tokenMsg ->
            let
                ( ( updatedTokenState, tokenCmd ), ( updatedBNState, bnCmd ) ) =
                    case maybeConfig of
                        Just config ->
                            tokenUpdate config tokenMsg ( tokenState, bnTransactionState )

                        Nothing ->
                            ( ( model.tokenState, Cmd.none )
                            , ( model.bnTransactionState, Cmd.none )
                            )

                -- Let the primary action modal handle any updates it needs for confirm enabling a market.
                updatedPrimaryActionModal =
                    case model.primaryActionModalState of
                        Just activeActionModalState ->
                            DappInterface.PrimaryActionModal.handleTokenUpdate tokenMsg activeActionModalState
                                |> Just

                        Nothing ->
                            Nothing

                -- Let the primary action modal handle any updates it needs for confirm enabling a market.
                updatedVoteModal =
                    case maybeConfig of
                        Just config ->
                            Vote.handleTokenUpdate config tokenMsg model.voteModel

                        Nothing ->
                            model.voteModel
            in
            ( { model
                | tokenState = updatedTokenState
                , primaryActionModalState = updatedPrimaryActionModal
                , voteModel = updatedVoteModal
                , bnTransactionState = updatedBNState
              }
            , Cmd.batch
                [ Cmd.map WrappedTokenMsg tokenCmd
                , bnCmd
                ]
            )

        WrappedCompoundMsg compoundMsg ->
            let
                ( ( updatedCompoundState, compoundCmd ), ( updatedBNState, bnCmd ) ) =
                    case maybeConfig of
                        Just config ->
                            compoundUpdate config tokenState oracleState compoundMsg ( compoundState, bnTransactionState )

                        Nothing ->
                            ( ( model.compoundState, Cmd.none )
                            , ( model.bnTransactionState, Cmd.none )
                            )

                updatedCollateralToggleModal =
                    case model.collateralToggleModalState of
                        Just activeCollateralToggleModal ->
                            CollateralToggleModal.handleCompoundUpdate compoundMsg activeCollateralToggleModal
                                |> Just

                        Nothing ->
                            Nothing

                -- Let the primary action modal handle any updates it needs for showing confirm web3 view.
                updatedPrimaryActionModal =
                    case model.primaryActionModalState of
                        Just activeActionModalState ->
                            DappInterface.PrimaryActionModal.handleCompoundUpdate compoundMsg activeActionModalState
                                |> Just

                        Nothing ->
                            Nothing

                ( updatedTransactionState, transactionCmds ) =
                    case compoundMsg of
                        SetAccountLimits _ ->
                            Transaction.handleTrxCountPruning network account updatedCompoundState.maybeTrxCount transactionState

                        _ ->
                            ( transactionState, Cmd.none )
            in
            ( { model
                | compoundState = updatedCompoundState
                , collateralToggleModalState = updatedCollateralToggleModal
                , primaryActionModalState = updatedPrimaryActionModal
                , transactionState = updatedTransactionState
                , bnTransactionState = updatedBNState
              }
            , Cmd.batch
                [ Cmd.map WrappedCompoundMsg compoundCmd
                , Cmd.map WrappedTransactionMsg transactionCmds
                , bnCmd
                ]
            )

        WrappedOracleMsg oracleMsg ->
            let
                ( updatedOracleState, oracleCmd ) =
                    oracleUpdate maybeConfig tokenState oracleMsg oracleState
            in
            ( { model
                | oracleState = updatedOracleState
              }
            , Cmd.map WrappedOracleMsg oracleCmd
            )

        AdminMsg internal ->
            let
                ( updatedAdmin, cmd ) =
                    Admin.update internal model.adminModel
            in
            ( { model | adminModel = updatedAdmin }, Cmd.map adminTranslator cmd )

        ProposeMsg internal ->
            let
                ( updatedProposeModel, cmd ) =
                    Propose.update model.userLanguage internal model.proposeModel
            in
            ( { model | proposeModel = updatedProposeModel }, Cmd.map proposeTranslator cmd )

        VoteMsg internal ->
            let
                ( updatedVote, cmd ) =
                    Vote.update internal maybeConfig network apiBaseUrlMap account model.voteModel
            in
            ( { model | voteModel = updatedVote }, Cmd.map voteTranslator cmd )

        WrappedGovernanceMsg governanceMsg ->
            let
                ( updatedGovernanceState, governanceCmd ) =
                    case maybeConfig of
                        Just _ ->
                            Eth.Governance.update governanceMsg model.governanceState

                        Nothing ->
                            ( model.governanceState, Cmd.none )

                updatedClaimCompModalState =
                    ClaimCompModal.handleGovernanceMsgUpdate governanceMsg model.claimCompModalState
            in
            ( { model
                | governanceState = updatedGovernanceState
                , claimCompModalState = updatedClaimCompModalState
              }
            , Cmd.map WrappedGovernanceMsg governanceCmd
            )

        ReplMsg internal ->
            let
                ( updatedRepl, cmd ) =
                    Repl.update internal model.repl
            in
            ( { model | repl = updatedRepl }, Cmd.map replTranslator cmd )

        WrappedPreferencesMsg preferencesMsg ->
            let
                ( updatedPreferences, preferencesCmd ) =
                    preferencesUpdate preferencesMsg model.preferences

                updatedCommonViewsModel =
                    case preferencesMsg of
                        SetDisplayCurrency _ ->
                            CommonViews.closeDropdownSelectors model.commonViewsModel

                        SetUserLanguage _ ->
                            CommonViews.closeDropdownSelectors model.commonViewsModel

                        _ ->
                            model.commonViewsModel

                updatedUserLanguage =
                    updatedPreferences.userLanguage
            in
            ( { model
                | preferences = updatedPreferences
                , commonViewsModel = updatedCommonViewsModel
                , userLanguage = updatedUserLanguage
              }
            , Cmd.map WrappedPreferencesMsg preferencesCmd
            )

        LiquidateMsg liquidateMsg ->
            let
                ( updatedLiquidateModel, liquidateCmd ) =
                    case maybeConfig of
                        Just config ->
                            Liquidate.update liquidateMsg model.liquidateModel config account tokenState oracleState

                        Nothing ->
                            ( model.liquidateModel, Cmd.none )
            in
            ( { model | liquidateModel = updatedLiquidateModel }, Cmd.map liquidateTranslator liquidateCmd )

        RefreshGasPrice result ->
            case result of
                Ok { average, fast } ->
                    let
                        maybeGasPriceAverage =
                            Decimal.fromInt 2
                                |> Decimal.fastdiv (Decimal.add average fast)
                    in
                    case maybeGasPriceAverage of
                        Just gasPriceAverage ->
                            ( { model | maybeGasPrice = Just gasPriceAverage }, setGasPrice gasPriceAverage )

                        Nothing ->
                            ( model, Console.error ("Error calculating average gas price using fast: " ++ Decimal.toString fast ++ " and average: " ++ Decimal.toString average) )

                Err gasPriceResponseError ->
                    ( model, Console.error ("Error getting gas price from Gas Service API: " ++ Utils.Http.showError gasPriceResponseError) )

        KeyPress "`" ->
            update (ReplMsg Repl.Toggle) model

        KeyPress _ ->
            ( model, Cmd.none )

        Error error ->
            ( { model | errors = model.errors }, Console.log error )



---- VIEW ----


view : Model -> Html Msg
view model =
    viewFull model
        |> Html.div [ id "main" ]


viewFull : Model -> List (Html Msg)
viewFull ({ page, liquidateModel, transactionState, compoundState, tokenState, oracleState, configs, configAbis, network, preferences, account, blockNumber, userLanguage } as model) =
    let
        maybeConfig =
            getCurrentConfig model

        header =
            Html.map commonViewsTranslator (pageHeader userLanguage page model.connectedEthWalletModel account model.preferences model.governanceState model.commonViewsModel)

        replFooter =
            Html.map replTranslator (Repl.view model.repl)

        footer =
            Html.map commonViewsTranslator (pageFooter userLanguage blockNumber model.preferences model.commonViewsModel)

        claimCompView =
            Html.map claimCompModalTranslator (ClaimCompModal.view userLanguage maybeConfig network account tokenState oracleState transactionState model.governanceState preferences model.claimCompModalState)
    in
    case page of
        Liquidate ->
            [ header
            , Html.map liquidateTranslator (Liquidate.view userLanguage model.currentTimeZone maybeConfig network account compoundState tokenState oracleState preferences transactionState liquidateModel)
            , chooseWalletModal userLanguage model
            , claimCompView
            , footer
            , replFooter
            ]

        Admin ->
            [ alertView model
            , header
            , Html.map adminTranslator (Admin.view userLanguage configs configAbis account network model.currentTimeZone model.currentTime model.adminModel)
            , chooseWalletModal userLanguage model
            , claimCompView
            , footer
            , replFooter
            ]

        Home ->
            [ div [ id "borrow-interface-root" ]
                [ alertView model
                , header
                , Html.map containerTranslator (DappInterface.Container.view model)
                , chooseWalletModal userLanguage model
                , claimCompView
                , footer
                , Html.map collateralToggleModalTranslator (CollateralToggleModal.view model)
                , Html.map primaryActionModalTranslator (DappInterface.PrimaryActionModal.view model)
                , replFooter
                ]
            ]

        Propose ->
            [ alertView model
            , header
            , Html.map proposeTranslator (Propose.view userLanguage False configs configAbis account network model.proposeModel)
            , footer
            , chooseWalletModal userLanguage model
            , claimCompView
            , replFooter
            ]

        CrowdPropose ->
            [ alertView model
            , header
            , Html.map proposeTranslator (Propose.view userLanguage True configs configAbis account network model.proposeModel)
            , footer
            , chooseWalletModal userLanguage model
            , claimCompView
            , replFooter
            ]

        TermsOfService ->
            [ header
            , DappTerms.view userLanguage
            , chooseWalletModal userLanguage model
            , claimCompView
            , footer
            , replFooter
            ]

        Vote ->
            [ alertView model
            , header
            , Html.map voteTranslator (Vote.view userLanguage maybeConfig network model.currentTimeZone model.currentTime account model.transactionState model.governanceState model.tokenState model.voteModel)
            , footer
            , chooseWalletModal userLanguage model
            , claimCompView
            , replFooter
            ]


alertView : Model -> Html Msg
alertView ({ account, maybeGasPrice, network, userLanguage } as model) =
    if invalidNetwork model.network model.configs then
        badNetworkAlert userLanguage

    else
        case account of
            UnknownAcct ->
                text ""

            NoAccount ->
                text ""

            Acct (Customer address) maybeBalance ->
                let
                    hasZeroEthBalance =
                        case maybeBalance of
                            Just balance ->
                                Decimal.eq balance Decimal.zero

                            Nothing ->
                                False
                in
                case ( network, hasZeroEthBalance, maybeGasPrice ) of
                    ( Just MainNet, _, Just gasPrice ) ->
                        highGasAlert userLanguage gasPrice

                    ( Just testNet, True, _ ) ->
                        testNetworkNoEtherAlert userLanguage (Network.networkName testNet) address

                    ( Just testNet, _, _ ) ->
                        testNetworkAlert userLanguage (Network.networkName testNet)

                    ( Nothing, True, _ ) ->
                        text ""

                    _ ->
                        text ""


chooseWalletModal : Translations.Lang -> Model -> Html Msg
chooseWalletModal userLanguage ({ connectedEthWalletModel } as model) =
    let
        headerHasBack =
            case connectedEthWalletModel.chooseWalletState of
                ConnectedEthWallet.ChooseProvider ->
                    False

                ConnectedEthWallet.ChooseLedgerAccount ->
                    True

                ConnectedEthWallet.LoadingLegerAccounts ->
                    True

                ConnectedEthWallet.AttemptingConnectToWallet ->
                    True

                ConnectedEthWallet.LedgerConnectionError ->
                    True

                ConnectedEthWallet.WalletConnectedChooseHidden ->
                    False

                ConnectedEthWallet.FirstTimeAutoconnecting ->
                    False

        headerView =
            div [ class "header" ]
                ((if headerHasBack then
                    [ a [ class "back-arrow", onClick ConnectModalBack ]
                        [ span [ class "icon" ] []
                        ]
                    ]

                  else
                    []
                 )
                    ++ [ div [ class "close-x" ]
                            [ button [ onClick (HideAndResetConnectModal False) ] []
                            ]
                       ]
                )
    in
    if
        connectedEthWalletModel.chooseWalletState
            /= ConnectedEthWallet.WalletConnectedChooseHidden
            && connectedEthWalletModel.chooseWalletState
            /= ConnectedEthWallet.FirstTimeAutoconnecting
    then
        div [ class "connect-wallet modal" ]
            [ div [ class "cover active" ] []
            , div [ class "container-small" ]
                [ div [ class "accent neutral" ] []
                , div [ class "legacy-panel" ]
                    [ headerView
                    , Html.map connectedEthWalletTranslator (ConnectedEthWallet.chooseWalletView userLanguage False connectedEthWalletModel)
                    ]
                ]
            ]

    else
        text ""


badNetworkAlert : Translations.Lang -> Html msg
badNetworkAlert userLanguage =
    div [ class "alert caution" ]
        [ text (Translations.bad_network_alert userLanguage)
        ]


highGasAlert : Translations.Lang -> Decimal -> Html msg
highGasAlert userLanguage gasPrice =
    let
        hundredGwei =
            Decimal.fromInt 100000000000
    in
    if Decimal.gt gasPrice hundredGwei then
        div [ class "alert alert--dark2" ] [ text (Translations.high_gas_alert_1 userLanguage), span [] [ a ([ class "inline", target "__blank" ] ++ href External "https://www.blocknative.com/gas-estimator") [ text (Translations.high_gas_alert_link userLanguage) ] ], text (Translations.high_gas_alert_2 userLanguage) ]

    else
        text ""


testNetworkAlert : Translations.Lang -> String -> Html msg
testNetworkAlert userLanguage network =
    div [ class "alert" ]
        [ text (Translations.testnet_alert userLanguage network)
        ]


testNetworkNoEtherAlert : Translations.Lang -> String -> String -> Html msg
testNetworkNoEtherAlert userLanguage network address =
    div [ class "alert caution" ]
        [ div []
            [ text (Translations.testnet_alert userLanguage network) ]
        , div
            []
            [ text (Translations.no_ether_alert userLanguage) ]
        ]


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



-- Gas Price


refreshLatestGasPrice : Dict String String -> Network -> Cmd Msg
refreshLatestGasPrice apiBaseUrlMap network =
    let
        maybeGasPriceUrl =
            CompoundApi.GasService.Urls.getGasPriceUrl apiBaseUrlMap network

        getGasPriceCmd =
            case maybeGasPriceUrl of
                Just getGasPriceUrl ->
                    Http.send RefreshGasPrice (Http.get getGasPriceUrl CompoundApi.GasService.Decoders.gasPriceResponseDecoder)

                _ ->
                    Cmd.none
    in
    getGasPriceCmd



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyPress (Json.Decode.map KeyPress (Json.Decode.field "key" Json.Decode.string))
        , giveError Error
        , giveAccountBalance (handleError (Json.Decode.errorToString >> Error) SetAccountBalance)
        , giveNewBlock (handleError (Json.Decode.errorToString >> Error) SetBlockNumber)
        , Sub.map connectedEthWalletTranslator ConnectedEthWallet.subscriptions
        , Sub.map WrappedTransactionMsg (Transaction.subscriptions model.transactionState)
        , Sub.map WrappedBNTransactionMsg BNTransaction.subscriptions
        , Sub.map WrappedTokenMsg (tokenSubscriptions model.tokenState)
        , Sub.map WrappedCompoundMsg compoundSubscriptions
        , Sub.map WrappedOracleMsg (oracleSubscriptions model.oracleState)
        , Sub.map WrappedPreferencesMsg (preferencesSubscriptions model.preferences)
        , Sub.map liquidateTranslator Liquidate.subscriptions
        , Sub.map replTranslator (Repl.subscriptions model.repl)
        , Sub.map adminTranslator (Admin.subscriptions model.adminModel)
        , Sub.map proposeTranslator (Propose.subscriptions model.proposeModel)
        , Sub.map voteTranslator (Vote.subscriptions model.voteModel)
        , Sub.map WrappedGovernanceMsg Eth.Governance.subscriptions
        , Time.every (1000.0 * 1.0 * toFloat CompoundComponents.Utils.Time.seconds) Tick
        , Time.every (1000.0 * 4.0 * toFloat CompoundComponents.Utils.Time.hours) CheckVersion
        , onUrlChange (Url.fromString >> UrlChange)
        ]



---- NAVIGATION ----


port onUrlChange : (String -> msg) -> Sub msg



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
