module FromLanding.Governance.CommonViews exposing
    ( loadingProposalRow
    , proposalContentView
    )

import CompoundApi.Governance.Common.Models exposing (ProposalAction)
import CompoundApi.Governance.ProposalService.Models exposing (Proposal, ProposalStateEnum(..))
import CompoundApi.Governance.ProposalVoteReceiptService.Models exposing (ProposalVoteReceipt)
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (HrefLinkType(..), class, href, id, onError, src, style, target)
import CompoundComponents.Utils.Time exposing (differenceInSeconds)
import DateFormat
import Html exposing (Html, a, div, img, label, p, span, text)
import Strings.Translations as Translations
import Time
import Utils.TimeWords exposing (distanceTimeInWords)


proposalContentView : Translations.Lang -> Time.Zone -> Maybe Time.Posix -> Proposal a -> Bool -> Html msg
proposalContentView userLanguage timezone maybeCurrentTime { id, title, states } asHeader =
    let
        maybeCurrentState =
            states |> List.reverse |> List.head

        ( headerContentClass, headerTitleClass, headerDetailsClass ) =
            if asHeader then
                ( " proposal__content--header", " proposal__content__description__title--header", " proposal__content__description__details--large" )

            else
                ( "", "", "" )

        stateTimeFormatter =
            DateFormat.format
                [ DateFormat.monthNameFull
                , DateFormat.text " "
                , DateFormat.dayOfMonthSuffix
                , DateFormat.text ", "
                , DateFormat.yearNumber
                ]
                timezone
    in
    case maybeCurrentState of
        Just currentState ->
            let
                prependZeros : String -> String
                prependZeros value =
                    if String.length value < 3 then
                        value
                            |> String.cons '0'
                            |> prependZeros

                    else
                        value

                ( pulsatingDot, tag, detailsText ) =
                    case currentState.state of
                        Active ->
                            let
                                pulse =
                                    if asHeader then
                                        text ""

                                    else
                                        div [ class "pulsating-dot proposal__content__pulsating-dot mobile-hide" ] []

                                timeString =
                                    case ( maybeCurrentTime, currentState.end_time ) of
                                        ( Just currentTime, Just endTime ) ->
                                            let
                                                difference =
                                                    differenceInSeconds endTime currentTime
                                            in
                                            Translations.time_left userLanguage (distanceTimeInWords userLanguage difference)

                                        ( _, Just endTime ) ->
                                            Translations.ends_on userLanguage (stateTimeFormatter endTime)

                                        ( _, _ ) ->
                                            ""
                            in
                            ( pulse
                            , div [ class "proposal__content__description__details__tag proposal__content__description__details__tag--active" ] [ text (Translations.proposal_state_active userLanguage) ]
                            , div [ class "proposal__content__description__details__text proposal__content__description__details__text--active" ]
                                [ span [] [ text (prependZeros (String.fromInt id)) ]
                                , span [] [ text "•" ]
                                , span [] [ text timeString ]
                                ]
                            )

                        _ ->
                            let
                                states_ =
                                    List.map .state states

                                tag_ =
                                    if List.member Succeeded states_ then
                                        div [ class "proposal__content__description__details__tag proposal__content__description__details__tag--passed" ] [ text (Translations.proposal_state_passed userLanguage) ]

                                    else if List.member Failed states_ then
                                        div [ class "proposal__content__description__details__tag proposal__content__description__details__tag--not-passed" ] [ text (Translations.proposal_state_failed userLanguage) ]

                                    else if List.member Canceled states_ then
                                        div [ class "proposal__content__description__details__tag proposal__content__description__details__tag--not-passed" ] [ text (Translations.proposal_state_failed userLanguage) ]

                                    else
                                        div [ class "proposal__content__description__details__tag proposal__content__description__details__tag--not-passed" ] [ text (Translations.proposal_state_pending userLanguage) ]

                                detailsText_ =
                                    case currentState.state of
                                        Pending ->
                                            case ( maybeCurrentTime, currentState.end_time ) of
                                                ( Just currentTime, Just endTime ) ->
                                                    let
                                                        difference =
                                                            differenceInSeconds endTime currentTime
                                                    in
                                                    Translations.time_until_voting userLanguage (distanceTimeInWords userLanguage difference)

                                                ( Just currentTime, _ ) ->
                                                    let
                                                        difference =
                                                            differenceInSeconds currentTime currentState.start_time
                                                    in
                                                    Translations.time_ago userLanguage (distanceTimeInWords userLanguage difference)

                                                _ ->
                                                    Translations.ends_on userLanguage (stateTimeFormatter currentState.start_time)

                                        Succeeded ->
                                            Translations.succeeded_time userLanguage (stateTimeFormatter currentState.start_time)

                                        Failed ->
                                            Translations.failed_time userLanguage (stateTimeFormatter currentState.start_time)

                                        Queued ->
                                            Translations.queued_time userLanguage (stateTimeFormatter currentState.start_time)

                                        Executed ->
                                            Translations.executed_time userLanguage (stateTimeFormatter currentState.start_time)

                                        Canceled ->
                                            Translations.canceled_time userLanguage (stateTimeFormatter currentState.start_time)

                                        Expired ->
                                            Translations.expired_time userLanguage (stateTimeFormatter currentState.start_time)

                                        _ ->
                                            ""
                            in
                            ( text ""
                            , tag_
                            , div [ class "proposal__content__description__details__text proposal__content__description__details__text" ]
                                [ span [] [ text (prependZeros (String.fromInt id)) ]
                                , span [] [ text "•" ]
                                , span [] [ text detailsText_ ]
                                ]
                            )
            in
            div [ class ("proposal__content" ++ headerContentClass) ]
                [ pulsatingDot
                , div [ class "proposal__content__description" ]
                    [ div [ class ("proposal__content__description__title" ++ headerTitleClass) ] [ text title ]
                    , div [ class ("proposal__content__description__details" ++ headerDetailsClass) ]
                        [ tag
                        , detailsText
                        ]
                    ]
                ]

        Nothing ->
            text ""


loadingProposalRow : Html msg
loadingProposalRow =
    div [ class "proposal proposal--loading" ]
        [ div [ class "proposal__content" ]
            [ div [ class "proposal__content__description" ]
                [ div [ class "proposal__content__description__title proposal__content__description__title--loading" ] []
                , div [ class "proposal__content__description__details proposal__content__description__details--loading" ]
                    [ div [ class "proposal__content__description__details__tag proposal__content__description__details__tag--loading" ] []
                    , div [ class "proposal__content__description__details__text" ]
                        [ div [ class "proposal__content__description__details__text__time--loading" ] []
                        ]
                    ]
                ]
            ]
        , div [ class "proposal__current-state-view" ]
            [ div [ class "proposal__current-state-view__state proposal__current-state-view__state--loading" ]
                [ div [ class "proposal__current-state-view__state__text--loading" ] [] ]
            ]
        ]
