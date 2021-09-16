module Utils.Http exposing (showError)

import Http


showError : Http.Error -> String
showError err =
    case err of
        Http.BadUrl url ->
            "BadUrl: " ++ url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus resp ->
            "BadStatus " ++ resp.status.message

        Http.BadPayload info resp ->
            "BadPayload: " ++ info ++ "; " ++ resp.status.message
