port module Preferences exposing
    ( Preferences
    , PreferencesMsg(..)
    , preferencesInit
    , preferencesSubscriptions
    , preferencesUpdate
    )

import CompoundComponents.DisplayCurrency as DisplayCurrency exposing (DisplayCurrency(..))
import CompoundComponents.Eth.Decoders exposing (forceMaybe)
import CompoundComponents.Functions exposing (handleError)
import Dict exposing (Dict)
import Json.Decode exposing (Value)
import Json.Decode.Pipeline exposing (optional, required)
import Result exposing (Result)
import Strings.Translations as Translations



{- Preferences is a complete module that tracks a user's preferences,
   e.g. whether to display values in Eth or USD. This module is meant
   to be expanded to cover use cases such as order of assets, etc.,
   over time. Note: we store the user's preferences in local storage.
-}


type alias Preferences =
    { displayCurrency : DisplayCurrency
    , userLanguage : Translations.Lang
    , supplyPaneOpen : Bool
    , borrowPaneOpen : Bool
    }


type PreferencesMsg
    = SetDisplayCurrency DisplayCurrency
    | SetUserLanguage Translations.Lang
    | SetSupplyPaneOpen Bool
    | SetBorrowPaneOpen Bool
    | SetPreferences Preferences
    | ClearStoredPreferences


preferencesInit : Translations.Lang -> ( Preferences, Cmd PreferencesMsg )
preferencesInit initialLanguage =
    ( { displayCurrency = USD
      , userLanguage = initialLanguage
      , supplyPaneOpen = True
      , borrowPaneOpen = True
      }
    , askStoredPreferences
    )


preferencesUpdate : PreferencesMsg -> Preferences -> ( Preferences, Cmd PreferencesMsg )
preferencesUpdate msg state =
    case msg of
        SetDisplayCurrency displayCurrency ->
            let
                updatedState =
                    { state | displayCurrency = displayCurrency }
            in
            ( updatedState, storePreferences updatedState )

        SetUserLanguage userLanguage ->
            let
                updatedState =
                    { state | userLanguage = userLanguage }
            in
            ( updatedState, storePreferences updatedState )

        SetSupplyPaneOpen isOpen ->
            let
                updatedState =
                    { state | supplyPaneOpen = isOpen }
            in
            ( updatedState, storePreferences updatedState )

        SetBorrowPaneOpen isOpen ->
            let
                updatedState =
                    { state | borrowPaneOpen = isOpen }
            in
            ( updatedState, storePreferences updatedState )

        SetPreferences preferences ->
            ( preferences, Cmd.none )

        ClearStoredPreferences ->
            ( state, askClearPreferences )


preferencesSubscriptions : Preferences -> Sub PreferencesMsg
preferencesSubscriptions state =
    Sub.batch
        [ giveStoredPreferences (handleError (\_ -> ClearStoredPreferences) SetPreferences)
        ]



-- HELPERS


readSavedLanguage : String -> Maybe Translations.Lang
readSavedLanguage =
    Dict.fromList [ ( "en", Translations.En ), ( "es", Translations.Es ), ( "zh", Translations.Zh ), ( "fr", Translations.Fr ), ( "ko", Translations.Ko ) ]
        |> (\b a -> Dict.get a b)


savedLanguageToString : Translations.Lang -> String
savedLanguageToString userLanguage =
    case userLanguage of
        Translations.En ->
            "en"

        Translations.Es ->
            "es"

        Translations.Zh ->
            "zh"

        Translations.Fr ->
            "fr"

        Translations.Ko ->
            "ko"



-- PORTS


port storePreferencesPort : { displayCurrency : String, userLanguage : String, supplyPaneOpen : Bool, borrowPaneOpen : Bool } -> Cmd msg


storePreferences : Preferences -> Cmd msg
storePreferences { displayCurrency, userLanguage, supplyPaneOpen, borrowPaneOpen } =
    storePreferencesPort
        { displayCurrency = DisplayCurrency.displayCurrencyToString displayCurrency
        , userLanguage = savedLanguageToString userLanguage
        , supplyPaneOpen = supplyPaneOpen
        , borrowPaneOpen = borrowPaneOpen
        }


port askStoredPreferencesPort : {} -> Cmd msg


askStoredPreferences : Cmd msg
askStoredPreferences =
    askStoredPreferencesPort {}


port giveStoredPreferencesPort : (Value -> msg) -> Sub msg


giveStoredPreferences : (Result Json.Decode.Error Preferences -> msg) -> Sub msg
giveStoredPreferences wrapper =
    let
        decoder =
            Json.Decode.succeed Preferences
                |> required "displayCurrency"
                    (Json.Decode.string
                        |> Json.Decode.map DisplayCurrency.readDisplayCurrency
                        |> Json.Decode.andThen (forceMaybe "invalid display currency")
                    )
                |> required "userLanguage"
                    (Json.Decode.oneOf
                        [ Json.Decode.null Translations.En
                        , Json.Decode.string
                            |> Json.Decode.map readSavedLanguage
                            |> Json.Decode.andThen (forceMaybe "invalid saved language")
                        ]
                    )
                |> optional "supplyPaneOpen" Json.Decode.bool True
                |> optional "borrowPaneOpen" Json.Decode.bool True
    in
    giveStoredPreferencesPort
        (Json.Decode.decodeValue decoder >> wrapper)


port askClearPreferencesPort : {} -> Cmd msg


askClearPreferences : Cmd msg
askClearPreferences =
    askClearPreferencesPort {}
