module Utils.BrowserInfo exposing (BrowserType(..), detectBrowser)

import Regex exposing (Regex)


type BrowserType
    = Apple_iOS
    | Android
    | Desktop


regex_iOS : Regex
regex_iOS =
    Regex.fromString "iPad|iPhone|iPod"
        |> Maybe.withDefault Regex.never


regexAndroid : Regex
regexAndroid =
    Regex.fromString "Android"
        |> Maybe.withDefault Regex.never


detectBrowser : String -> BrowserType
detectBrowser userAgent =
    if Regex.contains regex_iOS userAgent then
        Apple_iOS

    else if Regex.contains regexAndroid userAgent then
        Android

    else
        Desktop
