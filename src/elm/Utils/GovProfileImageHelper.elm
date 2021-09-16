module Utils.GovProfileImageHelper exposing (ImageHelperMsg(..), ProfileImageConfig, ProfileImageSize(..), profileImage)

import CompoundComponents.Utils.CompoundHtmlAttributes exposing (class, id, onError, src)
import Html exposing (Html, div, img, span, text)


type ImageHelperMsg
    = ProfileImageError String String


type ProfileImageSize
    = Small
    | Medium
    | Large


type alias ProfileImageConfig =
    { maybeProfileImageUrl : Maybe String
    , address : String
    , size : ProfileImageSize
    , showProposerIcon : Bool
    , useLightStyle : Bool
    , showCrowdProposalIcon : Bool
    }


profileImage : ProfileImageConfig -> Html ImageHelperMsg
profileImage { maybeProfileImageUrl, address, size, showProposerIcon, useLightStyle, showCrowdProposalIcon } =
    let
        profileImageUrl =
            if showCrowdProposalIcon then
                "/images/icn-crowd-proposal.svg"

            else
                maybeProfileImageUrl
                    |> Maybe.withDefault "/images/icn-default-avatar-large.svg"

        elementId =
            "img-" ++ address

        extraClass =
            case size of
                Small ->
                    " gov-profile-image--small"

                Medium ->
                    ""

                Large ->
                    " gov-profile-image--large"

        proposerIcon =
            if showProposerIcon then
                let
                    extraProposerIconSizeClass =
                        if size == Small then
                            " gov-profile-image__proposer-icon--small"

                        else
                            ""

                    extraProposerIconLightClass =
                        if useLightStyle then
                            " gov-profile-image__proposer-icon--light"

                        else
                            ""
                in
                div [ class ("gov-profile-image__proposer-icon" ++ extraProposerIconSizeClass ++ extraProposerIconLightClass) ] []

            else
                text ""
    in
    div [ class ("gov-profile-image" ++ extraClass) ]
        [ img [ id elementId, class "gov-profile-image__raw-image", src profileImageUrl, onError (ProfileImageError elementId "/images/icn-default-avatar-large.svg") ] []
        , proposerIcon
        ]
