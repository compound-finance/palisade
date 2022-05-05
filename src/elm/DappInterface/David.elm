module DappInterface.David exposing
    ( InternalMsg(..)
    , Model
    , Translator
    , emptyState
    , init
    , translator
    , update
    , view
    )

import CompoundComponents.Utils.CompoundHtmlAttributes exposing (HrefLinkType(..), class, href, id, src)
import Html exposing (Html, a, div, h1, header, img, input, label, span, text)
import Html.Attributes exposing (checked, classList, type_)
import Html.Events exposing (onClick)


type alias Model =
    { lightTheme : Bool
    }


type InternalMsg
    = NoOp
    | ToggleTheme


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
    { lightTheme = False
    }


init : ( Model, Cmd Msg )
init =
    ( emptyState, Cmd.none )


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update internalMsg model =
    case internalMsg of
        NoOp ->
            ( model, Cmd.none )

        ToggleTheme ->
            ( { model
                | lightTheme = not model.lightTheme
              }
            , Cmd.none
            )


view : Model -> Html Msg
view { lightTheme } =
    div
        [ id "david-wrapper"
        , classList
            [ ( "dark-theme", not lightTheme )
            , ( "light-theme", lightTheme )
            ]
        ]
        [ div [ class "dot-grid" ]
            [ span [ class "dot-grid-commet" ] []
            ]
        , header [ class "david-header" ]
            [ div [ class "david-container" ]
                [ div [ class "david-header-row" ]
                    [ a (class "david-brand" :: href External "https://compound.finance") []
                    , span [ class "david-theme-toggle" ]
                        [ label [ class "david-theme-toggle-label" ]
                            [ span [ class "theme-toggle-bar" ]
                                [ input [ type_ "checkbox", onClick (ForSelf <| ToggleTheme), checked False, class "theme-toggle-input" ] []
                                , span [ class "theme-toggle-thumb" ]
                                    [ img [ src "/images/icn-moon.svg", class "theme-toggle-thumb-image moon-icon" ] []
                                    , img [ src "/images/icn-sun.svg", class "theme-toggle-thumb-image sun-icon" ] []
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , div [ class "david-masthead" ]
            [ div [ class "david-container" ]
                [ h1 [ class "david-headline" ]
                    [ text "The Compound protocol currently has "
                    , span [ class "david-headline-accent-text" ] [ text "$8,192,169,373.95 " ]
                    , text "of assets earning interest across "
                    , span [ class "david-headline-accent-text" ] [ text "20 markets" ]
                    ]
                ]
            ]
        ]