module FromLanding.Page exposing
    ( DocsSubPage(..)
    , LandingPage(..)
    , landingUrlForPage
    )

import CompoundComponents.Eth.Network exposing (Network(..), networkName)
import Url.Builder as UrlBuilder


type LandingPage
    = Docs DocsSubPage (Maybe String)
    | GovernanceAccountProfile String
    | GovernanceProposalDetail Int
    | GovernanceProposalOverview
    | GovernanceLeaderboard


type DocsSubPage
    = GettingStarted
    | DocsCTokens
    | Comptroller
    | Governance
    | OpenPriceFeed
    | CompoundJs
    | API
    | Security


getHrefUrl : LandingPage -> String
getHrefUrl page =
    case page of
        Docs subPage fragment ->
            let
                base =
                    "/docs"

                section =
                    case subPage of
                        GettingStarted ->
                            ""

                        DocsCTokens ->
                            "/ctokens"

                        Comptroller ->
                            "/comptroller"

                        Governance ->
                            "/governance"

                        OpenPriceFeed ->
                            "/prices"

                        CompoundJs ->
                            "/compound-js"

                        API ->
                            "/api"

                        Security ->
                            "/security"

                subsection =
                    case fragment of
                        Just s ->
                            "#" ++ s

                        Nothing ->
                            ""
            in
            base ++ section ++ subsection

        GovernanceAccountProfile address ->
            "/governance/address/" ++ address

        GovernanceProposalDetail proposalId ->
            "/governance/proposals/" ++ String.fromInt proposalId

        GovernanceProposalOverview ->
            "/governance/proposals"

        GovernanceLeaderboard ->
            "/governance/leaderboard"


landingUrlForPage : Network -> LandingPage -> String
landingUrlForPage network page =
    let
        baseUrl =
            "https://lodestarfinance.io"

        networkParam =
            String.toLower (networkName network)
    in
    baseUrl ++ UrlBuilder.relative [ getHrefUrl page ] [ UrlBuilder.string "target_network" networkParam ]
