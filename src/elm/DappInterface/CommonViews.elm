module DappInterface.CommonViews exposing
    ( InternalMsg(..)
    , Model
    , Translator
    , closeDropdownSelectors
    , compOrVoteBalanceSpan
    , currencySelectorButton
    , init
    , pageFooter
    , pageHeader
    , translator
    , update
    )

import CompoundComponents.DisplayCurrency exposing (DisplayCurrency(..))
import CompoundComponents.Eth.ConnectedEthWallet as EthConnectedWallet
import CompoundComponents.Eth.Ethereum exposing (Account(..), CustomerAddress(..), shortenedAddressString)
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (HrefLinkType(..), class, href, id, onClickStopPropagation, src, style, target)
import CompoundComponents.Utils.NumberFormatter exposing (formatBlockNumber, formatCompAndVoteBalance, formatToDecimalPlaces)
import DappInterface.Page exposing (Page(..), getHrefUrl)
import Decimal exposing (Decimal)
import Eth.Governance exposing (GovernanceState, getCompAccruedBalance, getCompoundGovernanceTokenBalance)
import Html exposing (Html, a, div, footer, header, img, label, span, text)
import Html.Events exposing (onClick)
import Preferences exposing (Preferences, PreferencesMsg(..))
import Strings.Translations as Translations exposing (Lang(..))


type alias Model =
    { currencyDropdownActive : Bool
    , languageDropdownActive : Bool
    }


type OutMsg
    = WrappedPreferencesMsg PreferencesMsg
    | AccountAddressClicked
    | CompButtonClicked


type InternalMsg
    = ToggleCurrencyDropdown Bool
    | ToggleLanguageDropdown Bool


type Msg
    = ForSelf InternalMsg
    | ForParent OutMsg


type alias TranslationDictionary msg =
    { onInternalMessage : InternalMsg -> msg
    , onWrappedPreferencesdMsg : PreferencesMsg -> msg
    , onAccountButtonClicked : msg
    , onCompButtonClicked : msg
    }


type alias Translator msg =
    Msg -> msg


translator : TranslationDictionary msg -> Translator msg
translator { onInternalMessage, onWrappedPreferencesdMsg, onAccountButtonClicked, onCompButtonClicked } msg =
    case msg of
        ForSelf internal ->
            onInternalMessage internal

        ForParent (WrappedPreferencesMsg preferencesMsg) ->
            onWrappedPreferencesdMsg preferencesMsg

        ForParent AccountAddressClicked ->
            onAccountButtonClicked

        ForParent CompButtonClicked ->
            onCompButtonClicked


emptyState : Model
emptyState =
    { currencyDropdownActive = False
    , languageDropdownActive = False
    }


init : ( Model, Cmd msg )
init =
    ( emptyState, Cmd.none )


closeDropdownSelectors : Model -> Model
closeDropdownSelectors model =
    { model
        | currencyDropdownActive = False
        , languageDropdownActive = False
    }



--TODO: Load up API to get conversion rates?


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleCurrencyDropdown dropdownState ->
            ( { model | currencyDropdownActive = dropdownState }, Cmd.none )

        ToggleLanguageDropdown dropdownState ->
            ( { model | languageDropdownActive = dropdownState }, Cmd.none )


pageHeader : Translations.Lang -> Page -> EthConnectedWallet.Model -> Account -> Preferences -> GovernanceState -> Model -> Html Msg
pageHeader userLanguage page connectedWallet account preferences governanceState model =
    let
        accountAddress =
            case account of
                UnknownAcct ->
                    ""

                NoAccount ->
                    Translations.no_account userLanguage

                Acct (Customer customerAddress) _ ->
                    shortenedAddressString 2 4 customerAddress

        connectedWalletIconClass =
            case connectedWallet.selectedProvider of
                Just EthConnectedWallet.Metamask ->
                    "icon metamask"

                Just EthConnectedWallet.WalletLink ->
                    "icon coinbase"

                Just EthConnectedWallet.Ledger ->
                    "icon ledger dark"

                _ ->
                    ""

        accountButton =
            if connectedWallet.selectedProvider == Nothing || connectedWallet.selectedProvider == Just EthConnectedWallet.None then
                a [ id "connect-wallet", class "dapp button hollow", onClick (ForParent AccountAddressClicked) ]
                    [ text (Translations.connect_wallet userLanguage) ]

            else
                a [ id "account", onClick (ForParent AccountAddressClicked) ]
                    [ span [ class connectedWalletIconClass ] []
                    , text accountAddress
                    ]

        mobileLinks =
            if connectedWallet.connectionState == Nothing || connectedWallet.connectionState == Just EthConnectedWallet.Disconnected then
                [ accountButton ]

            else
                [ compBalanceView account governanceState ]

        links =
            let
                { homeClass, voteClass } =
                    let
                        emptyClasses =
                            { homeClass = ""
                            , voteClass = ""
                            }
                    in
                    case page of
                        Home ->
                            { emptyClasses | homeClass = "active" }

                        Vote ->
                            { emptyClasses | voteClass = "active" }

                        _ ->
                            emptyClasses
            in
            [ a (class homeClass :: href PageNavigation (getHrefUrl Home)) [ text (Translations.dashboard userLanguage) ]
            , a (class voteClass :: href PageNavigation (getHrefUrl Vote)) [ text (Translations.vote userLanguage) ]
            ]
    in
    header [ class "dapp" ]
        [ div [ class "container-large" ]
            [ div [ class "row align-middle" ]
                [ div [ class "col-xs-3" ]
                    [ a (class "brand" :: href External "https://compound.finance") []
                    ]
                , div [ class "col-xs-6 mobile-hide text-center links" ] links
                , div [ class "col-xs-9 col-sm-3 text-right actions" ]
                    [ compBalanceView account governanceState
                    , accountButton
                    ]
                , div [ class "col-xs-9 mobile-links actions" ] mobileLinks
                ]
            ]
        ]


pageFooter : Translations.Lang -> Maybe Int -> Preferences -> Model -> Html Msg
pageFooter userLanguage maybeBlockNumber preferences model =
    let
        indicatorColorClass =
            case maybeBlockNumber of
                Just _ ->
                    " green"

                Nothing ->
                    " red"
    in
    footer [ class "dapp" ]
        [ div [ class "container-large" ]
            [ div [ class "row top" ]
                [ div [ class "col-xs-12 col-sm-2" ]
                    [ a ([ class "brand" ] |> List.append (href PageNavigation "/")) [] ]
                , div [ class "col-xs-12 col-sm-10 links" ]
                    [ a (target "_blank" :: href External "https://compound.finance/markets") [ text (Translations.markets userLanguage) ]
                    , a (target "_blank" :: href External "https://compound.finance/governance") [ text (Translations.governance userLanguage) ]
                    , a (target "_blank" :: href External "https://compound.finance/governance/comp") [ text (Translations.comp userLanguage) ]
                    , a (href PageNavigation (getHrefUrl TermsOfService)) [ text (Translations.terms userLanguage) ]
                    , a (target "_blank" :: href External "https://medium.com/compound-finance/the-compound-guide-to-supplying-borrowing-crypto-assets-94821f2950a0") [ text (Translations.support userLanguage) ]
                    ]
                ]
            , div [ class "bottom" ]
                [ div [ class "help" ]
                    [ div [ class "mobile-hide" ]
                        [ span [ class ("dot-indicator" ++ indicatorColorClass) ] []
                        , label [ class "small" ] [ text (Translations.latest_block userLanguage (formatBlockNumber maybeBlockNumber)) ]
                        , a (target "_blank" :: href External "https://compound.finance/markets") [ text (Translations.markets userLanguage) ]
                        , a (target "_blank" :: href External "https://compound.finance/governance") [ text (Translations.governance userLanguage) ]
                        , a (target "_blank" :: href External "https://compound.finance/governance/comp") [ text (Translations.comp userLanguage) ]
                        , a (target "_blank" :: href External "https://medium.com/compound-finance/the-compound-guide-to-supplying-borrowing-crypto-assets-94821f2950a0") [ text (Translations.support userLanguage) ]
                        , a (href PageNavigation (getHrefUrl TermsOfService)) [ text (Translations.terms userLanguage) ]
                        ]
                    ]
                , div [ class "social" ]
                    [ currencySelectorButton preferences.displayCurrency model
                    , languageSelectorView userLanguage model
                    ]
                ]
            ]
        ]


compBalanceView : Account -> GovernanceState -> Html Msg
compBalanceView account governanceState =
    let
        balanceView value attrs =
            div (class "comp-balance" :: attrs)
                [ text value
                , div [ class "icon icon--COMP" ] []
                ]
    in
    case account of
        Acct customer _ ->
            case ( getCompoundGovernanceTokenBalance customer governanceState, getCompAccruedBalance customer governanceState ) of
                ( Just balance, Just accrued ) ->
                    let
                        claimCompThreshold =
                            Decimal.fromInt 500

                        total =
                            Decimal.add balance accrued
                                |> formatToDecimalPlaces 4 False
                    in
                    if Decimal.lt accrued claimCompThreshold then
                        balanceView total [ onClick (ForParent CompButtonClicked) ]

                    else
                        balanceView "—" []

                _ ->
                    balanceView "—" []

        _ ->
            text ""


getCurrencyTextAndMsg : DisplayCurrency -> ( String, Msg )
getCurrencyTextAndMsg targetCurrency =
    case targetCurrency of
        USD ->
            ( "$ USD", (ForParent << WrappedPreferencesMsg) (SetDisplayCurrency USD) )

        Ether ->
            ( "Ξ ETH", (ForParent << WrappedPreferencesMsg) (SetDisplayCurrency Ether) )

        GBP ->
            ( "£ GBP", (ForParent << WrappedPreferencesMsg) (SetDisplayCurrency GBP) )

        EUR ->
            ( "€ EUR", (ForParent << WrappedPreferencesMsg) (SetDisplayCurrency EUR) )


currencySelectorButton : DisplayCurrency -> Model -> Html Msg
currencySelectorButton selectedCurrency model =
    let
        ( currencyText, onCurrencyClickMsg ) =
            getCurrencyTextAndMsg selectedCurrency

        ( currencyDropdownActiveClass, dropdownOptions ) =
            if model.currencyDropdownActive then
                ( " active"
                , [ Ether, EUR, GBP, USD ]
                    |> List.filterMap
                        (\currencyOption ->
                            if currencyOption == selectedCurrency then
                                Nothing

                            else
                                let
                                    ( optionText, optionMsg ) =
                                        getCurrencyTextAndMsg currencyOption
                                in
                                span [ class "dropdown__option dropdown__option--light", onClickStopPropagation optionMsg ]
                                    [ label [ class "small" ] [ text optionText ]
                                    ]
                                    |> Just
                        )
                )

            else
                ( "", [] )
    in
    div [ class "dropdown dropdown--currency", onClickStopPropagation (ForSelf (ToggleCurrencyDropdown (not model.currencyDropdownActive))) ]
        [ span [ class "dropdown__selected dropdown__selected--light-1" ]
            [ label [ class "small", style "margin-left" "0" ] [ text currencyText ]
            ]
        , div [ class ("dropdown__options dropdown__options--footer " ++ currencyDropdownActiveClass) ]
            dropdownOptions
        ]


languageSelectorView : Translations.Lang -> Model -> Html Msg
languageSelectorView userLanguage model =
    let
        selectedLanguageText =
            case userLanguage of
                En ->
                    Translations.english userLanguage

                Es ->
                    Translations.spanish userLanguage

                Zh ->
                    Translations.chinese userLanguage

                Fr ->
                    Translations.french userLanguage

                Ko ->
                    Translations.korean userLanguage

        iconClassForLanguage language =
            case language of
                En ->
                    "icn english"

                Es ->
                    "icn spanish"

                Zh ->
                    "icn chinese"

                Fr ->
                    "icn french"

                Ko ->
                    "icn korean"

        ( languageDropdownActiveClass, optionsContent ) =
            if model.languageDropdownActive then
                ( " active"
                , [ span [ class "dropdown__option dropdown__option--language", onClickStopPropagation <| (ForParent << WrappedPreferencesMsg) (SetUserLanguage En) ]
                        [ label [ class "small language" ] [ text (Translations.english En) ]
                        , span [ class (iconClassForLanguage En) ] []
                        ]
                  , span [ class "dropdown__option dropdown__option--language", onClickStopPropagation <| (ForParent << WrappedPreferencesMsg) (SetUserLanguage Es) ]
                        [ label [ class "small language" ] [ text (Translations.spanish Es) ]
                        , span [ class (iconClassForLanguage Es) ] []
                        ]
                  , span [ class "dropdown__option dropdown__option--language", onClickStopPropagation <| (ForParent << WrappedPreferencesMsg) (SetUserLanguage Zh) ]
                        [ label [ class "small language" ] [ text (Translations.chinese Zh) ]
                        , span [ class (iconClassForLanguage Zh) ] []
                        ]
                  , span [ class "dropdown__option dropdown__option--language", onClickStopPropagation <| (ForParent << WrappedPreferencesMsg) (SetUserLanguage Fr) ]
                        [ label [ class "small language" ] [ text (Translations.french Fr) ]
                        , span [ class (iconClassForLanguage Fr) ] []
                        ]
                  , span [ class "dropdown__option dropdown__option--language", onClickStopPropagation <| (ForParent << WrappedPreferencesMsg) (SetUserLanguage Ko) ]
                        [ label [ class "small language" ] [ text (Translations.korean Ko) ]
                        , span [ class (iconClassForLanguage Ko) ] []
                        ]
                  ]
                )

            else
                ( ""
                , []
                )
    in
    div [ class "dropdown dropdown--language", onClickStopPropagation (ForSelf (ToggleLanguageDropdown (not model.languageDropdownActive))) ]
        [ span [ class "dropdown__selected dropdown__selected--language" ]
            [ label [ class "small" ] [ text (Translations.language userLanguage) ]
            , span [ class (iconClassForLanguage userLanguage) ] []
            ]
        , div [ class ("dropdown__options dropdown__options--footer " ++ languageDropdownActiveClass) ]
            optionsContent
        ]


compOrVoteBalanceSpan : ( String, String ) -> Decimal -> List (Html msg)
compOrVoteBalanceSpan ( significantSpanClass, insignificantSpanClass ) balance =
    let
        fullBalanceString =
            formatCompAndVoteBalance balance

        significantPart =
            String.slice 0 -4 fullBalanceString

        insignificantPart =
            String.right 4 fullBalanceString
    in
    [ span [ class significantSpanClass ] [ text significantPart ]
    , span [ class insignificantSpanClass ] [ text insignificantPart ]
    ]
