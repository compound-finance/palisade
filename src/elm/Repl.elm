port module Repl exposing
    ( InternalMsg(..)
    , Model
    , Translator
    , emptyState
    , init
    , subscriptions
    , translator
    , update
    , view
    )

import Browser.Dom as Dom
import CompoundComponents.Eth.Ethereum exposing (CustomerAddress, getCustomerAddressString)
import CompoundComponents.Eth.Network exposing (Network(..), networkName)
import CompoundComponents.Functions exposing (handleError)
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (HrefLinkType(..), class, disabled, id, type_, value)
import Html exposing (Html, button, div, h4, input, p, span, text)
import Html.Events exposing (keyCode, on, onClick, onInput, onSubmit)
import Json.Decode exposing (field)
import Json.Encode
import Markdown.Parser as Markdown
import Markdown.Renderer as Renderer exposing (defaultHtmlRenderer)
import Task



-- Log message to repl


port replEval : String -> Cmd msg


port replInit : Bool -> Cmd msg


port replPrint : (String -> msg) -> Sub msg


port replMarkdown : (String -> msg) -> Sub msg


port replEnable : (Bool -> msg) -> Sub msg


port replSetAccount : String -> Cmd msg


type alias Model =
    { open : Bool
    , enabled : Bool
    , line : String
    , history : List (List (Html Msg))
    , errors : List String
    }


type InternalMsg
    = NoOp
    | Toggle
    | ReadLine String
    | Eval
    | Enable Bool
    | Print String
    | Markdown String
    | Error String
    | SetAccount CustomerAddress


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


emptyState : Model
emptyState =
    { open = False
    , enabled = True
    , line = ""
    , history = []
    , errors = []
    }


init : ( Model, Cmd Msg )
init =
    ( emptyState, Cmd.none )


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update internalMsg model =
    case internalMsg of
        NoOp ->
            ( model, Cmd.none )

        Toggle ->
            let
                nextOpen =
                    not model.open

                cmd =
                    if nextOpen then
                        Cmd.batch
                            [ replInit True
                            , Task.attempt (\_ -> ForSelf NoOp) (Dom.focus "repl-input")
                            ]

                    else
                        Cmd.none
            in
            ( { model | open = nextOpen }, cmd )

        ReadLine line ->
            if String.contains "`" line then
                ( model, Cmd.none )

            else
                ( { model | line = line }, Cmd.none )

        Eval ->
            if model.enabled then
                case String.toLower (String.trim model.line) of
                    "clear" ->
                        ( { model | line = "", history = [] }, Cmd.none )

                    _ ->
                        ( { model | line = "", enabled = False }, replEval model.line )

            else
                ( model, Cmd.none )

        Print str ->
            ( { model | history = [ text str ] :: model.history }, Cmd.none )

        Markdown str ->
            let
                deadEndsToString deadEnds =
                    deadEnds
                        |> List.map Markdown.deadEndToString
                        |> String.join "\n"

                history =
                    case
                        str
                            |> Markdown.parse
                            |> Result.mapError deadEndsToString
                            |> Result.andThen (\ast -> Renderer.render defaultHtmlRenderer ast)
                    of
                        Ok rendered ->
                            rendered

                        Err _ ->
                            [ text str ]
            in
            ( { model | history = history :: model.history }, Cmd.none )

        Enable enable ->
            ( { model | enabled = enable }, Cmd.none )

        SetAccount account ->
            ( model, replSetAccount (getCustomerAddressString account) )

        Error error ->
            ( { model | errors = error :: model.errors }, Cmd.none )


onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.Decode.succeed msg

            else
                Json.Decode.fail ""
    in
    on "keydown" (Json.Decode.andThen isEnter keyCode)


historyView : List (Html Msg) -> Html Msg
historyView html =
    div [ class "row" ]
        [ div [ class "col-xs-12 " ] [ p [] html ]
        ]


view : Model -> Html Msg
view { open, enabled, history, line } =
    if open then
        div [ class "container-large main", id "repl" ]
            [ div [ class "legacy-panel" ]
                [ div [ class "row header" ]
                    [ div [ class "col-xs-12" ] [ h4 [] [ text "Compound Console" ] ]
                    ]
                , div [] (List.map historyView (List.reverse history))
                , div [ class "row" ]
                    [ span [ class "col-xs-10" ]
                        [ input [ type_ "tex", id "repl-input", value line, onInput (ForSelf << ReadLine), onEnter <| ForSelf Eval ] []
                        ]
                    , span [ class "col-xs-2" ]
                        [ button
                            [ onClick <| ForSelf Eval
                            , disabled
                                (if enabled then
                                    False

                                 else
                                    True
                                )
                            ]
                            [ text "Eval" ]
                        ]
                    ]
                ]
            ]

    else
        span [] []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ replPrint (ForSelf << Print)
        , replMarkdown (ForSelf << Markdown)
        , replEnable (ForSelf << Enable)
        ]
