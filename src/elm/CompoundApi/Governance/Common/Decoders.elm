module CompoundApi.Governance.Common.Decoders exposing
    ( compAccountDecoder
    , compAccountTransactionDecoder
    , displayAccountDecoder
    , proposalActionDecoder
    )

import CompoundApi.Governance.Common.Models
    exposing
        ( CompAccount
        , CompAccountTransaction
        , CrowdProposal
        , CrowdProposalStateEnum(..)
        , DisplayAccount
        , ProposalAction
        , createCompAccount
        , createDisplayAccount
        )
import CompoundComponents.Eth.Decoders exposing (stringDecimal)
import Json.Decode exposing (Decoder, andThen, fail, field, int, list, map, map4, map5, map8, maybe, nullable, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Time


compAccountDecoder : Decoder CompAccount
compAccountDecoder =
    let
        addFakeCrowdProposal compAccount =
            let
                dummyProposal =
                    { proposal_address = "0x8169522c2c57883e8ef80c498aab7820da539806"
                    , title = "Add Maker (MKR) to Compound"
                    , author =
                        { address = "0xb0325dbe7fa891436e83a094f9f12848c78e449b"
                        , display_name = Just "Cal Beans"
                        , image_url = Just "https://profile.compound.finance/1RfADD1lVZGGDW2ucqrjakqdkMAJf7VhG/bcmxV1E4UM7CezFzrL597/+DGVqaIh/Z+GxeM5E7ABs="
                        , account_url = Nothing
                        }
                    , description = "Morbi quis justo magna. Phasellus quis purus dictum, pulvinar ex ac, maximus nibh. Nam malesuada dolor vel tellus rutrum dapibus. In hac habitasse platea dictumst. Aliquam efficitur nec dui et egestas. Suspendisse egestas dapibus mi, sed mollis orci tempus ut. Donec ac erat vestibulum, sollicitudin ex eget, pellentesque sem.\u{2028}"
                    , state = GatheringVotes
                    , actions = []
                    , create_time = Time.millisToPosix (1599242627 * 1000)
                    , propose_time = Nothing
                    }
                        |> Just
            in
            succeed { compAccount | crowd_proposal = dummyProposal }
    in
    succeed createCompAccount
        |> required "address" string
        |> required "display_name" (maybe string)
        |> required "image_url" (maybe string)
        |> required "account_url" (maybe string)
        |> required "balance" stringDecimal
        |> required "votes" stringDecimal
        |> required "vote_weight" stringDecimal
        |> required "proposals_created" int
        |> required "proposals_voted" int
        |> required "delegate" displayAccountDecoder
        |> required "rank" (maybe int)
        |> required "transactions" (maybe (list compAccountTransactionDecoder))
        |> required "total_delegates" int
        |> optional "crowd_proposal" (maybe crowdProposalDecoder) Nothing


displayAccountDecoder : Decoder (DisplayAccount {})
displayAccountDecoder =
    map4 createDisplayAccount
        (field "address" string)
        (field "display_name" (maybe string))
        (field "image_url" (maybe string))
        (field "account_url" (maybe string))


compAccountTransactionDecoder : Decoder CompAccountTransaction
compAccountTransactionDecoder =
    map4 CompAccountTransaction
        (field "title" string)
        (field "timestamp" int |> map ((*) 1000) |> map Time.millisToPosix)
        (field "trx_hash" string)
        (field "delta" stringDecimal)


proposalActionDecoder : Json.Decode.Decoder ProposalAction
proposalActionDecoder =
    map5 ProposalAction
        (field "title" string)
        (field "target" string)
        (field "value" stringDecimal)
        (field "signature" string)
        (field "data" string)


crowdProposalStateEnumDecoder : Json.Decode.Decoder CrowdProposalStateEnum
crowdProposalStateEnumDecoder =
    let
        stateStringDecoder : String -> Decoder CrowdProposalStateEnum
        stateStringDecoder stateString =
            case stateString of
                "gathering_votes" ->
                    succeed GatheringVotes

                "proposed" ->
                    succeed Proposed

                "terminated" ->
                    succeed Canceled

                _ ->
                    fail ("Unrecognized crowd proposal state: " ++ stateString)
    in
    string
        |> andThen stateStringDecoder


crowdProposalDecoder : Decoder CrowdProposal
crowdProposalDecoder =
    map8 CrowdProposal
        (field "proposal_address" string)
        (field "title" string)
        (field "author" displayAccountDecoder)
        (field "description" string)
        (field "state" crowdProposalStateEnumDecoder)
        (field "actions" (list proposalActionDecoder))
        (field "create_time" int |> map ((*) 1000) |> map Time.millisToPosix)
        (field "propose_time" (nullable (int |> map ((*) 1000) |> map Time.millisToPosix)))
