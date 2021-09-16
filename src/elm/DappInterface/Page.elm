module DappInterface.Page exposing (Page(..), getHrefUrl, getPage, getPageTitle)

import Strings.Translations as Translations
import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, top)


type Page
    = Admin
    | Home
    | Liquidate
    | Propose
    | CrowdPropose
    | TermsOfService
    | Vote


buildTitle : Translations.Lang -> Maybe String -> String
buildTitle userLanguage subTitle =
    case subTitle of
        Nothing ->
            Translations.site_title userLanguage

        Just s ->
            Translations.site_title userLanguage ++ " | " ++ s


getPageTitle : Translations.Lang -> Page -> String
getPageTitle userLanguage page =
    case page of
        Admin ->
            buildTitle userLanguage Nothing

        Home ->
            buildTitle userLanguage (Just (Translations.site_title_dashboard userLanguage))

        Liquidate ->
            buildTitle userLanguage (Just (Translations.site_title_liquidate userLanguage))

        Propose ->
            buildTitle userLanguage (Just (Translations.site_title_propose userLanguage))

        CrowdPropose ->
            buildTitle userLanguage (Just (Translations.site_title_propose userLanguage))

        TermsOfService ->
            buildTitle userLanguage (Just (Translations.site_title_terms userLanguage))

        Vote ->
            buildTitle userLanguage (Just (Translations.site_title_vote userLanguage))


getHrefUrl : Page -> String
getHrefUrl page =
    case page of
        Home ->
            "#"

        Admin ->
            "#admin"

        Liquidate ->
            "#liquidate"

        Propose ->
            "#propose"

        CrowdPropose ->
            "#cap"

        TermsOfService ->
            "#terms"

        Vote ->
            "#vote"


getPage : Url.Url -> Page
getPage location =
    let
        fragments =
            Maybe.map (String.split "/") location.fragment
    in
    case fragments of
        Just [ "admin" ] ->
            Admin

        Just [ "liquidate" ] ->
            Liquidate

        Just [ "propose" ] ->
            Propose

        Just [ "cap" ] ->
            CrowdPropose

        Just [ "terms" ] ->
            TermsOfService

        Just [ "vote" ] ->
            Vote

        _ ->
            Home
