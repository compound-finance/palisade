port module DappInterface.Propose exposing
    ( InternalMsg
    , Model
    , Translator
    , emptyState
    , init
    , submitProposal
    , subscriptions
    , translator
    , update
    , view
    )

import Array exposing (Array)
import CompoundComponents.Console as Console
import CompoundComponents.Eth.Ethereum exposing (Account(..), AssetAddress(..), ContractAddress(..), CustomerAddress(..), getContractAddressString, isValidAddress)
import CompoundComponents.Eth.Network exposing (Network(..), networkName)
import CompoundComponents.Functions exposing (handleError)
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (HrefLinkType(..), class, id, onClickStopPropagation, placeholder, style, type_, value)
import CompoundComponents.Utils.Markup exposing (disabled)
import Dict exposing (Dict)
import Eth.Config exposing (Config)
import Eth.Contract exposing (contractList)
import Eth.FunctionArg exposing (FunctionArg, buildFunctionArg, setArg)
import Html exposing (Html, button, div, h2, h3, input, label, p, section, span, text, textarea)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (field, list)
import Json.Encode
import Port exposing (encodeParameters, giveEncodedParameters)
import Strings.Translations as Translations
import Utils.ABIHelper exposing (ABIValue)
import Utils.ArrayHelper exposing (updateArray)
import Utils.GovernanceHelper exposing (abiDecoder, governanceHelperDataFromConfig, humanReadableTimelockAction)


type alias Model =
    { maybeTarget : Maybe ( String, String )
    , maybeFunction : Maybe ABIValue
    , maybeDataArg : Maybe String
    , functionArgs : Array FunctionArg
    , contractsDropdownActive : Bool
    , delayDropdownActive : Bool
    , functionsDropdownActive : Bool
    , value : String
    , actions : List Action
    , actionModalActive : Bool
    , actionModalStep : Int
    , actionModalUseExternalTarget : Bool
    , actionModalMaybeExternalTarget : Maybe String
    , actionModalMaybeExternalABI : Maybe String
    , maybeProposalTitle : Maybe String
    , maybeProposalOverview : Maybe String
    , maybeSelectedAction : Maybe Int
    , errors : List String
    }


type alias Action =
    { target : ( String, String )
    , function : ABIValue
    , dataArg : String
    , functionArgs : Array FunctionArg
    , value : String
    , isExternalTarget : Bool
    , maybeExternalABI : Maybe String
    }


type InternalMsg
    = SetTarget ( String, String )
    | SetFunction ABIValue
    | SetArg Int String
    | SetValue String
    | ToggleContractDropdown Bool
    | ToggleFunctionDropdown Bool
    | EncodeParametersResult String
    | AskSubmitProposal CustomerAddress ( ContractAddress, Bool )
    | AskSubmitCrowdProposal CustomerAddress ContractAddress ContractAddress ContractAddress
    | AddAction ( String, String ) ABIValue String
    | SetActionModal Bool (Maybe Int)
    | SetActionModalStep Int
    | SetActionModalUseExternalTarget Bool
    | SetActionModalExternalTarget String
    | SetActionModalExternalABI String
    | SetProposalTitle String
    | SetProposalOverview String
    | Error String


type Msg
    = ForSelf InternalMsg


type alias TranslationDictionary msg =
    { onInternalMessage : InternalMsg -> msg
    }


type alias Translator msg =
    Msg -> msg


translator : TranslationDictionary msg -> Translator msg
translator { onInternalMessage } msg =
    case msg of
        ForSelf internal ->
            onInternalMessage internal



-- Delay in Days


emptyState : Model
emptyState =
    { maybeTarget = Nothing
    , maybeFunction = Nothing
    , maybeDataArg = Nothing
    , functionArgs = Array.empty
    , contractsDropdownActive = False
    , delayDropdownActive = False
    , functionsDropdownActive = False
    , value = "0"
    , actions = []
    , actionModalActive = False
    , actionModalStep = 0
    , actionModalUseExternalTarget = False
    , actionModalMaybeExternalTarget = Nothing
    , actionModalMaybeExternalABI = Nothing
    , maybeSelectedAction = Nothing
    , maybeProposalTitle = Nothing
    , maybeProposalOverview = Nothing
    , errors = []
    }


init : ( Model, Cmd Msg )
init =
    let
        initState =
            emptyState
    in
    ( initState, Cmd.none )


update : Translations.Lang -> InternalMsg -> Model -> ( Model, Cmd Msg )
update userLanguage internalMsg model =
    case internalMsg of
        AddAction target function data ->
            let
                action =
                    { target = target
                    , function = function
                    , dataArg = data
                    , functionArgs = model.functionArgs
                    , value = model.value
                    , isExternalTarget = model.actionModalUseExternalTarget
                    , maybeExternalABI = model.actionModalMaybeExternalABI
                    }

                newActions =
                    case model.maybeSelectedAction of
                        Just index ->
                            model.actions
                                |> Array.fromList
                                |> Array.set index action
                                |> Array.toList

                        Nothing ->
                            model.actions ++ [ action ]
            in
            ( { model
                | actions = newActions
                , maybeTarget = Nothing
                , maybeFunction = Nothing
                , maybeDataArg = Nothing
                , functionArgs = Array.empty
                , value = "0"
                , actionModalActive = False
                , actionModalStep = 0
                , actionModalUseExternalTarget = False
                , actionModalMaybeExternalTarget = Nothing
                , actionModalMaybeExternalABI = Nothing
                , maybeSelectedAction = Nothing
              }
            , Cmd.none
            )

        SetActionModal active maybeActionIndex ->
            let
                newModel =
                    case maybeActionIndex of
                        Just index ->
                            case Array.get index (Array.fromList model.actions) of
                                Just action ->
                                    let
                                        maybeExternalTarget =
                                            if action.isExternalTarget then
                                                Just (Tuple.second action.target)

                                            else
                                                Nothing
                                    in
                                    { model
                                        | maybeTarget = Just action.target
                                        , maybeFunction = Just action.function
                                        , maybeDataArg = Just action.dataArg
                                        , functionArgs = action.functionArgs
                                        , value = action.value
                                        , maybeSelectedAction = Just index
                                        , actionModalUseExternalTarget = action.isExternalTarget
                                        , actionModalMaybeExternalTarget = maybeExternalTarget
                                        , actionModalMaybeExternalABI = action.maybeExternalABI
                                    }

                                Nothing ->
                                    model

                        Nothing ->
                            { model
                                | maybeTarget = Nothing
                                , actionModalUseExternalTarget = False
                                , actionModalMaybeExternalTarget = Nothing
                                , actionModalMaybeExternalABI = Nothing
                            }
            in
            ( { newModel
                | actionModalActive = active
                , contractsDropdownActive = False
                , delayDropdownActive = False
                , functionsDropdownActive = False
                , actionModalStep = 0
              }
            , Cmd.none
            )

        SetActionModalStep step ->
            let
                ( maybeFunction, maybeDataArg ) =
                    if model.actionModalStep == 1 && step == 0 then
                        ( Nothing, Nothing )

                    else
                        ( model.maybeFunction, model.maybeDataArg )
            in
            ( { model | actionModalStep = step, maybeFunction = maybeFunction, maybeDataArg = maybeDataArg }, Cmd.none )

        SetActionModalUseExternalTarget use ->
            ( { model | actionModalUseExternalTarget = use, maybeTarget = Nothing }, Cmd.none )

        SetActionModalExternalTarget target ->
            let
                maybeTarget =
                    if isValidAddress target then
                        Just ( Translations.other userLanguage, target )

                    else
                        Nothing
            in
            ( { model | actionModalMaybeExternalTarget = Just target, maybeTarget = maybeTarget }, Cmd.none )

        SetActionModalExternalABI abi ->
            ( { model | actionModalMaybeExternalABI = Just abi }, Cmd.none )

        SetProposalTitle title ->
            ( { model | maybeProposalTitle = Just title }, Cmd.none )

        SetProposalOverview overview ->
            ( { model | maybeProposalOverview = Just overview }, Cmd.none )

        EncodeParametersResult dataString ->
            ( { model | maybeDataArg = Just dataString }, Cmd.none )

        SetTarget tuple ->
            ( { model
                | maybeTarget = Just tuple
                , maybeFunction = Nothing
                , maybeDataArg = Nothing
                , value = "0"
                , actionModalUseExternalTarget = False
                , actionModalMaybeExternalTarget = Nothing
                , actionModalMaybeExternalABI = Nothing
                , actionModalStep = model.actionModalStep + 1
              }
            , Cmd.none
            )

        SetFunction abiValue ->
            let
                fnArgs =
                    List.map buildFunctionArg abiValue.inputs
                        |> Array.fromList

                dataArg =
                    if Array.length fnArgs == 0 then
                        Just "0x0"

                    else
                        Nothing
            in
            ( { model | maybeFunction = Just abiValue, maybeDataArg = dataArg, functionArgs = fnArgs, value = "0" }, Cmd.none )

        SetArg index value ->
            let
                functionArgs =
                    updateArray index (setArg value) model.functionArgs

                allValid =
                    functionArgs
                        |> Array.map .valid
                        |> Array.foldl (&&) True

                ( dataArg, encodeParamsCmd ) =
                    if allValid then
                        let
                            functionTypes =
                                functionArgs
                                    |> Array.map .rawType
                                    |> Array.toList

                            encodedArgs =
                                functionArgs
                                    |> Array.map .encoded
                                    |> Array.toList
                        in
                        ( model.maybeDataArg, encodeParameters functionTypes encodedArgs )

                    else
                        ( Nothing, Cmd.none )
            in
            ( { model | functionArgs = functionArgs, maybeDataArg = dataArg }, encodeParamsCmd )

        SetValue value ->
            ( { model | value = value }, Cmd.none )

        AskSubmitProposal adminAddress ( governorAddress, isBravo ) ->
            let
                { targets, values, signatures, calldatas, description } =
                    getProposalDetails model
            in
            ( model, submitProposal adminAddress governorAddress isBravo targets values signatures calldatas description )

        AskSubmitCrowdProposal adminAddress crowdFactoryAddress compAddress governorAddress ->
            let
                { targets, values, signatures, calldatas, description } =
                    getProposalDetails model
            in
            ( model, submitCrowdProposal adminAddress crowdFactoryAddress targets values signatures calldatas description compAddress governorAddress )

        ToggleContractDropdown isActive ->
            ( { model | contractsDropdownActive = isActive, delayDropdownActive = False, functionsDropdownActive = False }, Cmd.none )

        ToggleFunctionDropdown isActive ->
            ( { model | contractsDropdownActive = False, delayDropdownActive = False, functionsDropdownActive = isActive }, Cmd.none )

        Error error ->
            ( { model | errors = error :: model.errors }, Console.error error )


view : Translations.Lang -> Bool -> Dict String Config -> Json.Encode.Value -> Account -> Maybe Network -> Model -> Html Msg
view userLanguage isCrowdProposal configs abiFilesRaw account maybeNetwork model =
    let
        nameOfNetwork =
            case maybeNetwork of
                Just network ->
                    String.toLower (networkName network)

                Nothing ->
                    ""

        maybeNetworkConfig =
            Dict.get nameOfNetwork configs

        adminView =
            case maybeNetworkConfig of
                Just config ->
                    case ( config.maybeGovernor, config.maybeCompToken ) of
                        ( Just governor, Just compToken ) ->
                            governanceView userLanguage isCrowdProposal config governor compToken.address abiFilesRaw account nameOfNetwork config.maybeCrowdProposalFactory model

                        _ ->
                            noGovernanceView userLanguage

                Nothing ->
                    noGovernanceView userLanguage
    in
    div [ id "Admin" ] [ adminView ]


noGovernanceView : Translations.Lang -> Html Msg
noGovernanceView userLanguage =
    div [ class "container" ] [ text (Translations.no_governance_description userLanguage) ]


governanceView : Translations.Lang -> Bool -> Config -> ( ContractAddress, Bool ) -> ContractAddress -> Json.Encode.Value -> Account -> String -> Maybe ContractAddress -> Model -> Html Msg
governanceView userLanguage isCrowdProposal config ( governorAddress, isBravo ) compAddress abiFilesRaw account nameOfNetwork maybeCrowdFactoryAddress model =
    let
        goverananceHelperData =
            governanceHelperDataFromConfig (Just config)

        ( proposalTypeText, defaultSubmitButtonText ) =
            if isCrowdProposal && maybeCrowdFactoryAddress /= Nothing then
                ( Translations.create_autonomous_proposal userLanguage, Translations.submit_autonomous_proposal userLanguage )

            else
                ( Translations.create_proposal userLanguage, Translations.submit_proposal userLanguage )

        contracts =
            contractList (Just config)

        titleValue =
            case model.maybeProposalTitle of
                Just title ->
                    title

                Nothing ->
                    ""

        overviewValue =
            case model.maybeProposalOverview of
                Just overview ->
                    overview

                Nothing ->
                    ""

        proposalActionButton =
            case account of
                Acct adminAddress _ ->
                    case ( model.maybeProposalTitle, List.length model.actions > 0 ) of
                        ( Just _, True ) ->
                            case ( isCrowdProposal, maybeCrowdFactoryAddress ) of
                                ( True, Just crowdFactoryAddress ) ->
                                    button
                                        [ class "button main", onClick (ForSelf (AskSubmitCrowdProposal adminAddress crowdFactoryAddress compAddress governorAddress)) ]
                                        [ text (Translations.submit_autonomous_proposal userLanguage) ]

                                _ ->
                                    button
                                        [ class "button main", onClick (ForSelf (AskSubmitProposal adminAddress ( governorAddress, isBravo ))) ]
                                        [ text (Translations.submit_proposal userLanguage) ]

                        _ ->
                            button
                                [ disabled, class "button main" ]
                                [ text defaultSubmitButtonText ]

                _ ->
                    button
                        [ disabled, class "button main" ]
                        [ text defaultSubmitButtonText ]
    in
    div []
        [ section []
            [ div [ class "container" ]
                [ h2 []
                    [ text proposalTypeText
                    ]
                , div [ class "separator" ] []
                ]
            ]
        , section []
            [ div [ class "container" ]
                [ div [ class "row" ]
                    [ div [ class "col-sm-6 col-xs-12 description-section" ]
                        [ h3 [] [ text (Translations.proposal_description userLanguage) ]
                        , div [ class "separator" ] []
                        , div [ class "description-section__subsection" ]
                            [ label [] [ text (Translations.title userLanguage) ]
                            , input [ class "description-section__input", type_ "text", placeholder (Translations.placeholder_title userLanguage), onInput (ForSelf << SetProposalTitle), value titleValue ] []
                            ]
                        , div [ class "description-section__subsection" ]
                            [ label [] [ text (Translations.overview userLanguage) ]
                            , textarea [ class "description-section__text-area", placeholder (Translations.placeholder_description userLanguage), onInput (ForSelf << SetProposalOverview), value overviewValue ] []
                            ]
                        ]
                    , div [ class "col-sm-6 col-xs-12 actions-section" ]
                        [ h3 [] [ text (Translations.actions userLanguage) ]
                        , div [ class "separator" ] []
                        , div [ class "actions-section__list" ]
                            ([]
                                ++ (model.actions
                                        |> List.indexedMap
                                            (\index action ->
                                                let
                                                    ( _, target ) =
                                                        action.target

                                                    functionName =
                                                        action.function.name

                                                    functionArgsText =
                                                        Array.toList action.functionArgs
                                                            |> List.map .text

                                                    -- TODO: .text?
                                                    ( isHumanReadable, headerText, subHeaderText ) =
                                                        humanReadableTimelockAction userLanguage goverananceHelperData target action.value functionName functionArgsText

                                                    actionText =
                                                        if isHumanReadable then
                                                            headerText

                                                        else
                                                            subHeaderText
                                                in
                                                div [ class "actions-section__action", onClick <| ForSelf (SetActionModal True (Just index)) ]
                                                    [ text actionText
                                                    , div [ class "line-icon line-icon--edit" ] []
                                                    ]
                                            )
                                   )
                                ++ [ button [ class "button main actions-section__button", onClick <| ForSelf (SetActionModal True Nothing) ]
                                        [ text (Translations.add_action userLanguage)
                                        , div [ class "line-icon line-icon--plus" ] []
                                        ]
                                   ]
                            )
                        ]
                    ]
                ]
            ]
        , section []
            [ div [ class "container" ]
                [ div [ class "separator" ] [] ]
            ]
        , section []
            [ div [ class "container submit-proposal" ]
                [ proposalActionButton ]
            ]
        , actionModalView userLanguage model nameOfNetwork contracts abiFilesRaw
        ]


actionModalView : Translations.Lang -> Model -> String -> List ( String, String ) -> Json.Encode.Value -> Html Msg
actionModalView userLanguage model nameOfNetwork contracts abiFilesRaw =
    if model.actionModalActive then
        let
            ( found, targetContract ) =
                case model.maybeTarget of
                    Just ( name, _ ) ->
                        ( True, name )

                    Nothing ->
                        if model.actionModalUseExternalTarget then
                            ( True, Translations.other userLanguage )

                        else
                            ( False, Translations.select_a_contract userLanguage )

            abi =
                case model.actionModalMaybeExternalABI of
                    Just externalABI ->
                        case Json.Decode.decodeString (list abiDecoder) externalABI of
                            Ok decodedAbi ->
                                decodedAbi

                            Err _ ->
                                []

                    Nothing ->
                        if found then
                            case
                                Json.Decode.decodeValue
                                    (field nameOfNetwork (field targetContract (list abiDecoder)))
                                    abiFilesRaw
                            of
                                Ok decodedAbi ->
                                    decodedAbi

                                Err _ ->
                                    []

                        else
                            []

            backButton =
                if model.actionModalStep > 0 then
                    let
                        previousStep =
                            model.actionModalStep - 1
                    in
                    div [ class "back-arrow" ]
                        [ button [ onClick (ForSelf (SetActionModalStep previousStep)) ]
                            []
                        ]

                else
                    text ""

            currentModalView =
                if model.actionModalStep == 0 then
                    addActionStep1 userLanguage model contracts targetContract

                else
                    addActionStep2 userLanguage model abi
        in
        div [ class "modal action-modal" ]
            [ div [ class "cover active", onClick (ForSelf (SetActionModal False Nothing)) ] []
            , div [ class "legacy-panel action-modal__background" ]
                [ div [ class "header" ]
                    [ backButton
                    , div [ class "title" ]
                        [ span [] [ text (Translations.add_action userLanguage) ]
                        ]
                    , div [ class "close-x" ]
                        [ button [ onClick (ForSelf (SetActionModal False Nothing)) ]
                            []
                        ]
                    ]
                , div [ class "action-modal__body" ]
                    [ currentModalView ]
                ]
            ]

    else
        text ""


addActionStep1 : Translations.Lang -> Model -> List ( String, String ) -> String -> Html Msg
addActionStep1 userLanguage model contracts targetContract =
    let
        targetDropdownClass =
            if model.contractsDropdownActive then
                " active"

            else
                ""

        externalContractViews =
            if model.actionModalUseExternalTarget then
                let
                    externalTarget =
                        model.actionModalMaybeExternalTarget
                            |> Maybe.map identity
                            |> Maybe.withDefault ""

                    externalABI =
                        model.actionModalMaybeExternalABI
                            |> Maybe.map identity
                            |> Maybe.withDefault ""
                in
                [ div [ class "action-modal__input-group" ]
                    [ label [] [ text (Translations.address userLanguage) ]
                    , input [ class "action-modal__input-group__input", type_ "text", placeholder (Translations.placeholder_address userLanguage), onInput (ForSelf << SetActionModalExternalTarget), value externalTarget ] []
                    ]
                , div [ class "action-modal__input-group" ]
                    [ label [] [ text (Translations.abi userLanguage) ]
                    , textarea [ class "action-modal__input-group__text-area", onInput (ForSelf << SetActionModalExternalABI), value externalABI ] [ text externalABI ]
                    ]
                ]

            else
                [ text "" ]

        actionButton =
            case model.maybeTarget of
                Just _ ->
                    let
                        nextStep =
                            model.actionModalStep + 1
                    in
                    button
                        [ class "button main action-modal__action-button", onClick (ForSelf (SetActionModalStep nextStep)) ]
                        [ text (Translations.continue userLanguage) ]

                Nothing ->
                    button
                        [ disabled, class "button main action-modal__action-button" ]
                        [ text (Translations.continue userLanguage) ]
    in
    div [ class "action-modal__step1" ]
        (div [ class "action-modal__input-group" ]
            [ label [ style "margin-bottom" "0.5rem" ] [ text (Translations.select_target userLanguage) ]
            , div [ class "dropdown dropdown--full", onClickStopPropagation (ForSelf (ToggleContractDropdown (not model.contractsDropdownActive))) ]
                [ div [ class "dropdown__selected dropdown__selected--light action-modal__input-group__dropdown__option" ]
                    [ p [ class "small" ] [ text targetContract ]
                    ]
                , div [ class ("dropdown__options dropdown__options--light" ++ targetDropdownClass) ]
                    ((contracts
                        |> List.map
                            (\( name, address ) ->
                                div [ class "dropdown__option dropdown__option--light action-modal__input-group__dropdown__option", onClick (ForSelf (SetTarget ( name, address ))) ]
                                    [ p [ class "small" ] [ text name ] ]
                            )
                     )
                        ++ [ div [ class "dropdown__option dropdown__option--light", onClick (ForSelf (SetActionModalUseExternalTarget True)) ]
                                [ p [ class "small" ] [ text (Translations.other userLanguage) ] ]
                           ]
                    )
                ]
            ]
            :: externalContractViews
            ++ [ actionButton ]
        )


addActionStep2 : Translations.Lang -> Model -> List ABIValue -> Html Msg
addActionStep2 userLanguage model abi =
    let
        functionsDropdownClass =
            if model.functionsDropdownActive then
                " active"

            else
                ""

        functions =
            abi
                |> List.filterMap
                    (\fn ->
                        if fn.type_ == "function" && (fn.stateMutability == "payayble" || fn.stateMutability == "nonpayable") then
                            Just fn

                        else
                            Nothing
                    )

        ( selectedFunctionName, inputs, functionSignature ) =
            case model.maybeFunction of
                Just fn ->
                    let
                        argTypes =
                            List.map .type_ fn.inputs
                                |> String.join ","

                        sig =
                            fn.name ++ "(" ++ argTypes ++ ")"
                    in
                    ( fn.name, fn.inputs, sig )

                Nothing ->
                    ( Translations.select_a_function userLanguage, [], "" )

        dataArgDiv =
            case model.maybeDataArg of
                Just data ->
                    label [ class "data-arg" ] [ text (Translations.data userLanguage data) ]

                Nothing ->
                    text ""

        selectFunctionDiv =
            if List.length functions > 0 then
                div [ class "action-modal__input-group" ]
                    [ label [] [ text (Translations.select_function userLanguage) ]
                    , div [ class "dropdown dropdown--full", onClickStopPropagation (ForSelf (ToggleFunctionDropdown (not model.functionsDropdownActive))) ]
                        [ div [ class "dropdown__selected dropdown__selected--light action-modal__input-group__dropdown__option" ]
                            [ p [ class "small" ] [ text selectedFunctionName ]
                            ]
                        , div [ class ("dropdown__options dropdown__options--light" ++ functionsDropdownClass) ]
                            (functions
                                |> List.map
                                    (\fn ->
                                        div [ class "dropdown__option dropdown__option--light action-modal__input-group__dropdown__option", onClick (ForSelf (SetFunction fn)) ]
                                            [ p [ class "small" ] [ text fn.name ] ]
                                    )
                            )
                        ]
                    , label [] [ text (Translations.signature userLanguage functionSignature) ]
                    ]

            else
                div [] []

        enterValueDiv =
            let
                valueDiv =
                    div [ class "action-modal__input-group" ]
                        [ label [] [ text (Translations.enter_value userLanguage) ]
                        , input [ type_ "text", placeholder "value", onInput (ForSelf << SetValue), value model.value ] []
                        ]
            in
            case ( model.maybeFunction, model.actionModalMaybeExternalABI ) of
                ( Just fn, _ ) ->
                    if fn.stateMutability == "payable" then
                        valueDiv

                    else
                        div [] []

                ( Nothing, Nothing ) ->
                    valueDiv

                ( _, _ ) ->
                    div [] []

        actionButton =
            let
                callToActionText =
                    case model.maybeSelectedAction of
                        Just _ ->
                            Translations.update_action userLanguage

                        Nothing ->
                            Translations.add_action userLanguage

                activeButton target function data =
                    button
                        [ class "button main action-modal__action-button", onClick (ForSelf (AddAction target function data)) ]
                        [ text callToActionText ]

                disabledButton =
                    button
                        [ disabled, class "button main action-modal__action-button" ]
                        [ text callToActionText ]
            in
            case ( model.maybeTarget, model.maybeFunction, model.maybeDataArg ) of
                ( Just target, Just function, Just data ) ->
                    activeButton target function data

                ( Just target, Nothing, Nothing ) ->
                    case ( model.actionModalUseExternalTarget, model.actionModalMaybeExternalABI ) of
                        ( True, Nothing ) ->
                            activeButton target { inputs = [], name = "", outputs = [], payable = True, stateMutability = "payable", type_ = "function", signature = "" } ""

                        _ ->
                            disabledButton

                _ ->
                    disabledButton
    in
    div [ class "action-modal__step2" ]
        (selectFunctionDiv
            :: (inputs
                    |> List.indexedMap
                        (\index fnInput ->
                            let
                                labelText =
                                    fnInput.name ++ " (" ++ fnInput.type_ ++ ")"

                                inputValue =
                                    case Array.get index model.functionArgs of
                                        Just functionArg ->
                                            functionArg.text

                                        Nothing ->
                                            ""

                                hasErrorClass =
                                    case Array.get index model.functionArgs of
                                        Just functionArg ->
                                            if functionArg.valid then
                                                ""

                                            else
                                                " has-error"

                                        Nothing ->
                                            ""
                            in
                            div [ class "action-modal__input-group" ]
                                [ label [] [ text labelText ]
                                , input [ class ("action-modal__input-group__input" ++ hasErrorClass), type_ "text", placeholder labelText, onInput (ForSelf << SetArg index), value inputValue ] []
                                ]
                        )
               )
            ++ [ dataArgDiv
               , enterValueDiv
               , actionButton
               ]
        )



-- Helpers


getProposalDetails : Model -> { targets : List String, values : List String, signatures : List String, calldatas : List String, description : String }
getProposalDetails model =
    let
        empty : { targets : List String, values : List String, signatures : List String, calldatas : List String, description : String }
        empty =
            { targets = []
            , values = []
            , signatures = []
            , calldatas = []
            , description = ""
            }

        proposalDetailsMinusDescription =
            model.actions
                |> List.foldl
                    (\action accum ->
                        let
                            target =
                                Tuple.second action.target

                            signature =
                                let
                                    argTypes =
                                        List.map .type_ action.function.inputs
                                            |> String.join ","
                                in
                                action.function.name ++ "(" ++ argTypes ++ ")"
                        in
                        { accum
                            | targets = accum.targets ++ [ target ]
                            , values = accum.values ++ [ action.value ]
                            , signatures = accum.signatures ++ [ signature ]
                            , calldatas = accum.calldatas ++ [ action.dataArg ]
                            , description = ""
                        }
                    )
                    empty

        description =
            case ( model.maybeProposalTitle, model.maybeProposalOverview ) of
                ( Just title, Just overview ) ->
                    "# " ++ title ++ "\n" ++ overview

                ( Just title, _ ) ->
                    "# " ++ title

                _ ->
                    ""
    in
    { proposalDetailsMinusDescription | description = description }



-- Ports


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ giveEncodedParameters (handleError (ForSelf << Error << Json.Decode.errorToString) (ForSelf << EncodeParametersResult))
        ]


port adminDashboardSubmitProposalPort : { adminAddress : String, governorAddress : String, isBravo : Bool, targets : List String, values : List String, signatures : List String, calldatas : List String, description : String } -> Cmd msg


submitProposal : CustomerAddress -> ContractAddress -> Bool -> List String -> List String -> List String -> List String -> String -> Cmd msg
submitProposal (Customer adminAddress) (Contract governorAddress) isBravo targets values signatures calldatas description =
    adminDashboardSubmitProposalPort
        { adminAddress = adminAddress
        , governorAddress = governorAddress
        , isBravo = isBravo
        , targets = targets
        , values = values
        , signatures = signatures
        , calldatas = calldatas
        , description = description
        }


port adminDashboardSubmitCrowdProposalPort : { adminAddress : String, crowdFactoryAddress : String, targets : List String, values : List String, signatures : List String, calldatas : List String, description : String, compAddress : String, governorAddress : String } -> Cmd msg


submitCrowdProposal : CustomerAddress -> ContractAddress -> List String -> List String -> List String -> List String -> String -> ContractAddress -> ContractAddress -> Cmd msg
submitCrowdProposal (Customer adminAddress) (Contract crowdFactoryAddress) targets values signatures calldatas description (Contract compAddress) (Contract governorAddress) =
    adminDashboardSubmitCrowdProposalPort
        { adminAddress = adminAddress
        , crowdFactoryAddress = crowdFactoryAddress
        , targets = targets
        , values = values
        , signatures = signatures
        , calldatas = calldatas
        , description = description
        , compAddress = compAddress
        , governorAddress = governorAddress
        }
