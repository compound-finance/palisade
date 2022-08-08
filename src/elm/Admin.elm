port module Admin exposing
    ( InternalMsg
    , Model
    , Translator
    , emptyState
    , getQueuedTransactions
    , init
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
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (HrefLinkType(..), class, id, onClickStopPropagation, placeholder, type_, value)
import CompoundComponents.Utils.Markup exposing (disabled)
import CompoundComponents.Utils.Time
import Decimal
import Dict exposing (Dict)
import Eth.Config exposing (Config)
import Eth.Transaction exposing (transactionDateFormatter)
import Html exposing (Html, button, div, h2, input, label, p, section, text)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (field, list)
import Json.Encode
import Port exposing (encodeParameters, giveEncodedParameters)
import Regex exposing (contains)
import Strings.Translations as Translations
import Time
import Utils.ABIHelper exposing (ABIValue)
import Utils.GovernanceHelper exposing (TimelockTransaction, abiDecoder, timelockTrxDecoder)


type alias Model =
    { maybeTarget : Maybe ( String, String )
    , maybeFunction : Maybe ABIValue
    , maybeDataArg : Maybe String
    , functionTypes : List String
    , functionArgs : Array (Maybe String)
    , argsValid : Array Bool
    , contractsDropdownActive : Bool
    , delayDropdownActive : Bool
    , functionsDropdownActive : Bool
    , delayOptions : List Int
    , delay : Int
    , value : String
    , queuedTransactions : List TimelockTransaction
    , errors : List String
    }


type InternalMsg
    = SetTarget ( String, String )
    | SetFunction ABIValue
    | SetArg Int String String
    | SetValue String
    | SetDelay Int
    | AskQueueTransaction CustomerAddress String String String String
    | AskExecuteTransaction CustomerAddress String String String String String String
    | ToggleContractDropdown Bool
    | ToggleDelayDropdown Bool
    | ToggleFunctionDropdown Bool
    | EncodeParametersResult String
    | QueueTransactionsResult (List TimelockTransaction)
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
    , functionTypes = []
    , functionArgs = Array.empty
    , argsValid = Array.empty
    , contractsDropdownActive = False
    , delayDropdownActive = False
    , functionsDropdownActive = False
    , value = "0"
    , delayOptions = List.range 2 30
    , delay = 0
    , queuedTransactions = []
    , errors = []
    }


init : ( Model, Cmd Msg )
init =
    let
        initState =
            emptyState
    in
    ( initState, Cmd.none )


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update internalMsg model =
    case internalMsg of
        EncodeParametersResult dataString ->
            ( { model | maybeDataArg = Just dataString }, Cmd.none )

        QueueTransactionsResult adminTransactions ->
            let
                newQueuedTransactions =
                    List.append model.queuedTransactions adminTransactions
            in
            ( { model | queuedTransactions = newQueuedTransactions }, Cmd.none )

        SetTarget tuple ->
            ( { model
                | maybeTarget = Just tuple
                , maybeFunction = Nothing
                , maybeDataArg = Nothing
                , value = "0"
              }
            , Cmd.none
            )

        SetFunction abiValue ->
            let
                fnTypes =
                    List.map .type_ abiValue.inputs

                fnArgs =
                    Array.repeat (List.length fnTypes) Nothing

                argsValid =
                    Array.repeat (List.length fnTypes) True

                dataArg =
                    if List.length fnTypes == 0 then
                        Just "0x0"

                    else
                        Nothing
            in
            ( { model | maybeFunction = Just abiValue, maybeDataArg = dataArg, functionTypes = fnTypes, functionArgs = fnArgs, argsValid = argsValid, value = "0" }, Cmd.none )

        SetArg index valueType value ->
            let
                newArgValid =
                    case valueType of
                        "uint256" ->
                            case Decimal.fromString value of
                                Just decimal ->
                                    Decimal.eq decimal (Decimal.truncate 0 decimal)

                                Nothing ->
                                    False

                        "address" ->
                            isValidAddress value

                        "bytes" ->
                            let
                                isValidBytes =
                                    Maybe.withDefault Regex.never (Regex.fromString "^(0x)")
                            in
                            contains isValidBytes value

                        _ ->
                            True

                newArgsValid =
                    Array.set index newArgValid model.argsValid

                newArgs =
                    let
                        sanitizeValue =
                            case valueType of
                                "uint256" ->
                                    Maybe.withDefault Decimal.zero (Decimal.fromString value)
                                        |> Decimal.truncate 0
                                        |> Decimal.toString

                                _ ->
                                    value
                    in
                    Array.set index (Just sanitizeValue) model.functionArgs

                newArgsList =
                    newArgs
                        |> Array.toList
                        |> List.filterMap identity
                        |> List.map Json.Encode.string

                argsCompleted : Bool
                argsCompleted =
                    newArgsList
                        |> List.length
                        |> (==) (Array.length newArgs)

                argsValid : Bool
                argsValid =
                    newArgsValid
                        |> Array.foldl (&&) True

                ( dataArg, encodeParamsCmd ) =
                    if argsCompleted && argsValid then
                        ( model.maybeDataArg, encodeParameters model.functionTypes newArgsList )

                    else
                        ( Nothing, Cmd.none )
            in
            ( { model | functionArgs = newArgs, argsValid = newArgsValid, maybeDataArg = dataArg }, encodeParamsCmd )

        SetValue value ->
            ( { model | value = value }, Cmd.none )

        SetDelay delay ->
            ( { model | delay = delay }, Cmd.none )

        AskQueueTransaction adminAddress timelockAddress target signature data ->
            let
                delayInSeconds =
                    String.fromInt model.delay
            in
            ( model, queueTransaction adminAddress timelockAddress target model.value signature data delayInSeconds )

        AskExecuteTransaction adminAddress timelockAddress target value signature data eta ->
            ( model, executeTransaction adminAddress timelockAddress target value signature data eta )

        ToggleContractDropdown isActive ->
            ( { model | contractsDropdownActive = isActive, delayDropdownActive = False, functionsDropdownActive = False }, Cmd.none )

        ToggleDelayDropdown isActive ->
            ( { model | contractsDropdownActive = False, delayDropdownActive = isActive, functionsDropdownActive = False }, Cmd.none )

        ToggleFunctionDropdown isActive ->
            ( { model | contractsDropdownActive = False, delayDropdownActive = False, functionsDropdownActive = isActive }, Cmd.none )

        Error error ->
            ( { model | errors = error :: model.errors }, Console.error error )


view : Translations.Lang -> Dict String Config -> Json.Encode.Value -> Account -> Maybe Network -> Time.Zone -> Maybe Time.Posix -> Model -> Html Msg
view userLanguage configs abiFilesRaw account maybeNetwork currentTimeZone maybeCurrentTime model =
    let
        nameOfNetwork =
            case maybeNetwork of
                Just MainNet ->
                    String.toLower (networkName MainNet)

                Just network ->
                    String.toLower (networkName network)

                Nothing ->
                    ""

        maybeNetworkConfig =
            Dict.get nameOfNetwork configs

        adminView =
            case maybeNetworkConfig of
                Just config ->
                    case config.maybeTimelock of
                        Just _ ->
                            timelockView configs abiFilesRaw account maybeNetwork currentTimeZone maybeCurrentTime model

                        Nothing ->
                            noGovernanceView

                Nothing ->
                    noGovernanceView
    in
    div [ id "Admin" ] [ adminView ]


noGovernanceView : Html Msg
noGovernanceView =
    div [ class "container" ] [ text "The current selected network doesn't have governance" ]


timelockView : Dict String Config -> Json.Encode.Value -> Account -> Maybe Network -> Time.Zone -> Maybe Time.Posix -> Model -> Html Msg
timelockView configs abiFilesRaw account maybeNetwork timezone maybeCurrentTime model =
    let
        dropdownActiveClass dropdown =
            if dropdown model then
                " active"

            else
                ""

        ( nameOfNetwork, delayType, delayUnit ) =
            case maybeNetwork of
                Just MainNet ->
                    ( String.toLower (networkName MainNet), CompoundComponents.Utils.Time.days, " days" )

                Just network ->
                    ( String.toLower (networkName network), CompoundComponents.Utils.Time.minutes, " minutes" )

                Nothing ->
                    ( "", 1, "" )

        maybeNetworkConfig =
            Dict.get nameOfNetwork configs

        contracts : List ( String, String )
        contracts =
            case maybeNetworkConfig of
                Just config ->
                    let
                        timelock : List ( String, String )
                        timelock =
                            case config.maybeTimelock of
                                Just timelockEntry ->
                                    [ ( "Timelock", getContractAddressString timelockEntry ) ]

                                Nothing ->
                                    []

                        cErc20Delegate : List ( String, String )
                        cErc20Delegate =
                            case config.maybeCErc20Delegate of
                                Just cErc20DelegateEntry ->
                                    [ ( "CErc20Delegate", getContractAddressString cErc20DelegateEntry ) ]

                                Nothing ->
                                    []

                        cDaiDelegate : List ( String, String )
                        cDaiDelegate =
                            case config.maybeCDaiDelegate of
                                Just cDaiDelegateEntry ->
                                    [ ( "CDaiDelegate", getContractAddressString cDaiDelegateEntry ) ]

                                Nothing ->
                                    []
                    in
                    [ ( "Comptroller", getContractAddressString config.comptroller )
                    , ( "PriceOracle", getContractAddressString config.priceOracle )
                    , ( "lETH", getContractAddressString config.cEtherToken.address )
                    ]
                        ++ (config.cTokens
                                |> Dict.toList
                                |> List.map
                                    (\( cTokenSymbol, cTokenConfig ) ->
                                        let
                                            addressString =
                                                getContractAddressString cTokenConfig.address

                                            underlyingAddress =
                                                getContractAddressString cTokenConfig.underlying.address
                                        in
                                        [ ( cTokenSymbol, addressString ), ( cTokenConfig.underlying.symbol, underlyingAddress ) ]
                                    )
                                |> List.concat
                           )
                        ++ timelock
                        ++ cErc20Delegate
                        ++ cDaiDelegate

                Nothing ->
                    []

        ( targetContract, targetAddress ) =
            case model.maybeTarget of
                Just ( name, address ) ->
                    ( name, address )

                Nothing ->
                    ( "Select a contract", "" )

        abi =
            if targetContract /= "Select a contract" then
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
                    ( "Select a function", [], "" )

        selectFunctionDiv =
            if List.length functions > 0 then
                div [ class "container" ]
                    [ label [ class "medium" ] [ text "Select A Function" ]
                    , div [ class "dropdown dropdown--big", onClickStopPropagation (ForSelf (ToggleFunctionDropdown (not model.functionsDropdownActive))) ]
                        [ div [ class "dropdown__selected dropdown__selected--light" ]
                            [ p [ class "small" ] [ text selectedFunctionName ]
                            ]
                        , div [ class ("dropdown__options dropdown__options--light" ++ dropdownActiveClass .functionsDropdownActive) ]
                            (functions
                                |> List.map
                                    (\fn ->
                                        div [ class "dropdown__option dropdown__option--light", onClick (ForSelf (SetFunction fn)) ]
                                            [ p [ class "small" ] [ text fn.name ] ]
                                    )
                            )
                        ]
                    , label [] [ text ("Signature: " ++ functionSignature) ]
                    ]

            else
                div [] []

        dataArgDiv =
            case model.maybeDataArg of
                Just data ->
                    label [] [ text ("Data: " ++ data) ]

                Nothing ->
                    text ""

        delayString =
            String.fromInt (model.delay // delayType)

        actionButton =
            case ( account, maybeNetworkConfig ) of
                ( Acct adminAddress _, Just config ) ->
                    case config.maybeTimelock of
                        Just timelock ->
                            case ( model.maybeTarget, model.maybeFunction, model.maybeDataArg ) of
                                ( Just _, Just _, Just data ) ->
                                    button
                                        [ class "button main", onClick <| ForSelf (AskQueueTransaction adminAddress (getContractAddressString timelock) targetAddress functionSignature data) ]
                                        [ text "Enqueue Transaction" ]

                                _ ->
                                    button
                                        [ disabled, class "button main" ]
                                        [ text "Enqueue Transaction" ]

                        _ ->
                            text ""

                _ ->
                    button
                        [ disabled, class "button main" ]
                        [ text "Enqueue Transaction" ]
    in
    div []
        [ section []
            [ div [ class "container" ]
                [ h2 [] [ text "Testnet Admin Dashboard" ]
                ]
            ]
        , section []
            [ div [ class "container" ]
                [ label [ class "medium" ] [ text "Select Contract" ]
                , div [ class "dropdown dropdown--big", onClickStopPropagation (ForSelf (ToggleContractDropdown (not model.contractsDropdownActive))) ]
                    [ div [ class "dropdown__selected dropdown__selected--light" ]
                        [ p [ class "small" ] [ text targetContract ]
                        ]
                    , div [ class ("dropdown__options dropdown__options--light" ++ dropdownActiveClass .contractsDropdownActive) ]
                        (contracts
                            |> List.map
                                (\( name, address ) ->
                                    div [ class "dropdown__option dropdown__option--light", onClick (ForSelf (SetTarget ( name, address ))) ]
                                        [ p [ class "small" ] [ text name ] ]
                                )
                        )
                    ]
                , label [] [ text ("Target: " ++ targetAddress) ]
                ]
            ]
        , section []
            [ selectFunctionDiv
            ]
        , section []
            [ div [ class "container" ]
                (inputs
                    |> List.indexedMap
                        (\index fnInput ->
                            let
                                labelText =
                                    fnInput.name ++ " (" ++ fnInput.type_ ++ ")"

                                inputValue =
                                    case Array.get index model.functionArgs of
                                        Just maybeVal ->
                                            case maybeVal of
                                                Just val ->
                                                    val

                                                _ ->
                                                    ""

                                        Nothing ->
                                            ""

                                hasErrorClass =
                                    case Array.get index model.argsValid of
                                        Just isValid ->
                                            if isValid then
                                                ""

                                            else
                                                "has-error"

                                        Nothing ->
                                            ""
                            in
                            div []
                                [ label [] [ text labelText ]
                                , input [ class hasErrorClass, type_ "text", placeholder labelText, onInput (ForSelf << SetArg index fnInput.type_), value inputValue ] []
                                ]
                        )
                )
            , div [ class "container" ]
                [ dataArgDiv ]
            ]
        , section []
            [ div [ class "container" ]
                [ label [] [ text "Enter Value" ]
                , input [ type_ "text", placeholder "value", onInput (ForSelf << SetValue), value model.value ] []
                ]
            ]
        , section []
            [ div [ class "container" ]
                [ label [ class "medium" ] [ text "Select When To Execute" ]
                , div [ class "dropdown dropdown--big", onClickStopPropagation (ForSelf (ToggleDelayDropdown (not model.delayDropdownActive))) ]
                    [ div [ class "dropdown__selected dropdown__selected--light" ]
                        [ p [ class "small" ] [ text (delayString ++ delayUnit) ]
                        ]
                    , div [ class ("dropdown__options dropdown__options--light" ++ dropdownActiveClass .delayDropdownActive) ]
                        (model.delayOptions
                            |> List.map
                                (\delay ->
                                    let
                                        delay_ =
                                            String.fromInt delay

                                        delayToSet =
                                            delay * delayType
                                    in
                                    div [ class "dropdown__option dropdown__option--light", onClick (ForSelf (SetDelay delayToSet)) ]
                                        [ p [ class "small" ] [ text (delay_ ++ delayUnit) ] ]
                                )
                        )
                    ]
                ]
            ]
        , section []
            [ div [ class "container" ]
                [ actionButton ]
            ]
        , section []
            [ div [ class "container" ]
                (model.queuedTransactions
                    |> List.map
                        (\queuedTx ->
                            let
                                maybeContractInfo =
                                    contracts
                                        |> List.filter
                                            (\( _, addressString ) ->
                                                String.toLower addressString == String.toLower queuedTx.target
                                            )
                                        |> List.head

                                contractCall =
                                    case maybeContractInfo of
                                        Just ( name, _ ) ->
                                            name ++ "." ++ queuedTx.transactionData.functionCall

                                        Nothing ->
                                            queuedTx.target ++ "." ++ queuedTx.transactionData.functionCall

                                dateString =
                                    case String.toInt queuedTx.eta of
                                        Just eta ->
                                            let
                                                etaInTimePosix =
                                                    Time.millisToPosix (eta * 1000)

                                                formattedDate =
                                                    transactionDateFormatter timezone etaInTimePosix
                                            in
                                            queuedTx.eta ++ " (" ++ formattedDate ++ ")"

                                        Nothing ->
                                            queuedTx.eta

                                actionButtonExecute =
                                    case ( maybeCurrentTime, String.toInt queuedTx.eta ) of
                                        ( Just time, Just eta ) ->
                                            if CompoundComponents.Utils.Time.posixToSeconds time > eta then
                                                case ( account, maybeNetworkConfig ) of
                                                    ( Acct adminAddress _, Just config ) ->
                                                        case config.maybeTimelock of
                                                            Just timelock ->
                                                                button
                                                                    [ class "button main", onClick <| ForSelf (AskExecuteTransaction adminAddress (getContractAddressString timelock) queuedTx.target queuedTx.value queuedTx.signature queuedTx.data queuedTx.eta) ]
                                                                    [ text "Execute Transaction" ]

                                                            _ ->
                                                                text ""

                                                    _ ->
                                                        button
                                                            [ disabled, class "button main" ]
                                                            [ text "Execute Transaction" ]

                                            else
                                                button
                                                    [ disabled, class "button main" ]
                                                    [ text "Delay Not Met" ]

                                        _ ->
                                            text ""
                            in
                            div []
                                [ label [] [ text contractCall ]
                                , p [ class "small" ] [ text ("target: " ++ String.toLower queuedTx.target) ]
                                , p [ class "small" ] [ text ("value: " ++ queuedTx.value) ]
                                , p [ class "small" ] [ text ("signature: " ++ queuedTx.signature) ]
                                , p [ class "small" ] [ text ("data: " ++ queuedTx.data) ]
                                , p [ class "small" ] [ text ("eta: " ++ dateString) ]
                                , actionButtonExecute
                                ]
                        )
                )
            ]
        ]



-- Ports


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ giveEncodedParameters (handleError (ForSelf << Error << Json.Decode.errorToString) (ForSelf << EncodeParametersResult))
        , giveQueuedTransactions (handleError (ForSelf << Error << Json.Decode.errorToString) (ForSelf << QueueTransactionsResult))
        ]


port adminDashboardGetQueuedTransactionsPort : { timelockAddress : String, initialBlockNumber : Int } -> Cmd msg


getQueuedTransactions : Dict String Config -> Maybe Network -> Cmd msg
getQueuedTransactions configs maybeNetwork =
    let
        nameOfNetwork =
            case maybeNetwork of
                Just network ->
                    String.toLower (networkName network)

                Nothing ->
                    ""

        getTrxsCmd =
            case Dict.get nameOfNetwork configs of
                Just config ->
                    case ( config.maybeTimelock, Dict.get "Timelock" config.blocks ) of
                        ( Just timelock, Just blockNumber ) ->
                            adminDashboardGetQueuedTransactionsPort
                                { timelockAddress = getContractAddressString timelock
                                , initialBlockNumber = blockNumber
                                }

                        _ ->
                            Cmd.none

                Nothing ->
                    Cmd.none
    in
    getTrxsCmd


port adminDashboardQueueTransactionPort : { adminAddress : String, timelockAddress : String, target : String, value : String, signature : String, data : String, delay : String } -> Cmd msg


queueTransaction : CustomerAddress -> String -> String -> String -> String -> String -> String -> Cmd msg
queueTransaction (Customer adminAddress) timelockAddress target value signature data delay =
    adminDashboardQueueTransactionPort
        { adminAddress = adminAddress
        , timelockAddress = timelockAddress
        , target = target
        , value = value
        , signature = signature
        , data = data
        , delay = delay
        }


port adminDashboardExecuteTransactionPort : { adminAddress : String, timelockAddress : String, target : String, value : String, signature : String, data : String, eta : String } -> Cmd msg


executeTransaction : CustomerAddress -> String -> String -> String -> String -> String -> String -> Cmd msg
executeTransaction (Customer adminAddress) timelockAddress target value signature data eta =
    adminDashboardExecuteTransactionPort
        { adminAddress = adminAddress
        , timelockAddress = timelockAddress
        , target = target
        , value = value
        , signature = signature
        , data = data
        , eta = eta
        }


port giveQueuedTransactionsPort : (Json.Decode.Value -> msg) -> Sub msg


giveQueuedTransactions : (Result Json.Decode.Error (List TimelockTransaction) -> msg) -> Sub msg
giveQueuedTransactions wrapper =
    let
        decoder =
            list
                timelockTrxDecoder
    in
    giveQueuedTransactionsPort
        (Json.Decode.decodeValue decoder >> wrapper)
