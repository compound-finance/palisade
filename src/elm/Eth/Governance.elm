port module Eth.Governance exposing
    ( DelegatedAddress(..)
    , GovernanceMsg(..)
    , GovernanceState
    , askClaimComp
    , askDelegateTo
    , clearState
    , delegateeProfilePageNavigate
    , getCompAccruedBalance
    , getCompoundGovernanceTokenBalance
    , getCurrentVotes
    , getDelegateTotalBalance
    , getDelegatedAddress
    , getDelegatedAddressString
    , init
    , newBlockCmd
    , subscriptions
    , update
    )

import CompoundComponents.Console as Console
import CompoundComponents.Eth.Decoders exposing (decimal, decodeCustomerAddress)
import CompoundComponents.Eth.Ethereum exposing (Account(..), ContractAddress(..), CustomerAddress(..), getCustomerAddressString)
import CompoundComponents.Functions exposing (handleError)
import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Eth.Config exposing (Config, TokenConfig)
import Json.Decode exposing (Value, decodeValue, field)


type alias GovernanceState =
    { governanceTokenBalances : Dict String Decimal -- key user address
    , currentVotesBalances : Dict String Decimal -- key by user address
    , compAccruedBalances : Dict String Decimal -- key by user address
    , delegatorMapping : Dict String String -- key by delegator address, value is delegatee address
    , errors : List String
    }


type GovernanceMsg
    = SetGovernanceData GovernanceData
    | DelegateTo ContractAddress CustomerAddress CustomerAddress
    | SetCompAccrued FlywheelData
    | ClaimComp ContractAddress CustomerAddress (List String)
    | QueueProposal ( ContractAddress, Bool ) CustomerAddress Int
    | ExecuteProposal ( ContractAddress, Bool ) CustomerAddress Int
    | Error String


type alias GovernanceData =
    { customer : CustomerAddress
    , compTokenBalance : Decimal
    , currentVotesBalance : Decimal
    , delegateeAddress : CustomerAddress
    }


type DelegatedAddress
    = Self
    | Delegatee CustomerAddress
    | Undelegated


type alias FlywheelData =
    { customer : CustomerAddress
    , compAccrued : Decimal
    }


emptyState : GovernanceState
emptyState =
    { governanceTokenBalances = Dict.empty
    , currentVotesBalances = Dict.empty
    , compAccruedBalances = Dict.empty
    , delegatorMapping = Dict.empty
    , errors = []
    }


init : GovernanceState
init =
    emptyState


newBlockCmd : Config -> Int -> Account -> Maybe CustomerAddress -> Cmd GovernanceMsg
newBlockCmd config blockNumber maybeAccount maybeBrowsedAddress =
    case config.maybeCompToken of
        Just compToken ->
            let
                userBalCmd =
                    case maybeAccount of
                        Acct account _ ->
                            [ askGovernanceData blockNumber config.compoundLens compToken.address account compToken.decimals
                            ]
                                ++ (if config.maybeReservoir /= Nothing then
                                        [ askCompMetadata blockNumber compToken.address config.comptroller account config.compoundLens ]

                                    else
                                        []
                                   )

                        _ ->
                            [ Cmd.none ]

                browsedAddressCmds =
                    case maybeBrowsedAddress of
                        Just browsedAddress ->
                            [ askGovernanceData blockNumber config.compoundLens compToken.address browsedAddress compToken.decimals
                            ]

                        _ ->
                            [ Cmd.none ]
            in
            Cmd.batch <|
                userBalCmd
                    ++ browsedAddressCmds

        Nothing ->
            Cmd.none


delegateeProfilePageNavigate : Config -> Maybe Int -> Maybe CustomerAddress -> Cmd GovernanceMsg
delegateeProfilePageNavigate config maybeBlockNumber maybeBrowsedAddress =
    case ( config.maybeCompToken, maybeBlockNumber ) of
        ( Just compToken, Just blockNumber ) ->
            let
                browsedAddressCmds =
                    case maybeBrowsedAddress of
                        Just browsedAddress ->
                            [ askGovernanceData blockNumber config.compoundLens compToken.address browsedAddress compToken.decimals
                            ]

                        _ ->
                            [ Cmd.none ]
            in
            Cmd.batch browsedAddressCmds

        _ ->
            Cmd.none


update : GovernanceMsg -> GovernanceState -> ( GovernanceState, Cmd GovernanceMsg )
update msg ({ governanceTokenBalances, currentVotesBalances, compAccruedBalances, delegatorMapping } as state) =
    case msg of
        SetGovernanceData { customer, compTokenBalance, currentVotesBalance, delegateeAddress } ->
            let
                lowercaseCustomerAddressString =
                    getCustomerAddressString customer
                        |> String.toLower

                updatedGoverenanceTokenBalances =
                    Dict.insert lowercaseCustomerAddressString compTokenBalance governanceTokenBalances

                updatedVotesBalances =
                    Dict.insert lowercaseCustomerAddressString currentVotesBalance currentVotesBalances

                lowercaseDelegateeAddressString =
                    getCustomerAddressString delegateeAddress
                        |> String.toLower

                updatedDelegatorMapping =
                    Dict.insert lowercaseCustomerAddressString lowercaseDelegateeAddressString delegatorMapping
            in
            ( { state
                | governanceTokenBalances = updatedGoverenanceTokenBalances
                , currentVotesBalances = updatedVotesBalances
                , delegatorMapping = updatedDelegatorMapping
              }
            , Cmd.none
            )

        DelegateTo contractAddress account targetAddress ->
            ( state
            , askDelegateTo contractAddress account targetAddress
            )

        SetCompAccrued { customer, compAccrued } ->
            let
                lowercaseCustomerAddressString =
                    getCustomerAddressString customer
                        |> String.toLower

                updatedCompAccruedBalances =
                    Dict.insert lowercaseCustomerAddressString compAccrued compAccruedBalances
            in
            ( { state
                | compAccruedBalances = updatedCompAccruedBalances
              }
            , Cmd.none
            )

        ClaimComp contractAddress account markets ->
            ( state
            , askClaimComp contractAddress account markets
            )

        QueueProposal ( governorAddress, isBravo ) account proposalId ->
            ( state
            , askQueueProposal governorAddress isBravo account proposalId
            )

        ExecuteProposal ( governorAddress, isBravo ) account proposalId ->
            ( state
            , askExecuteProposal governorAddress isBravo account proposalId
            )

        Error error ->
            ( { state | errors = error :: state.errors }, Console.error error )


subscriptions : Sub GovernanceMsg
subscriptions =
    Sub.batch
        [ giveGovernanceData (handleError (Json.Decode.errorToString >> Error) SetGovernanceData)
        , giveCompAccrued (handleError (Json.Decode.errorToString >> Error) SetCompAccrued)
        ]


clearState : GovernanceState
clearState =
    emptyState


getCompoundGovernanceTokenBalance : CustomerAddress -> GovernanceState -> Maybe Decimal
getCompoundGovernanceTokenBalance customerAddress { governanceTokenBalances } =
    let
        lowercaseCustomerAddressString =
            getCustomerAddressString customerAddress
                |> String.toLower
    in
    Dict.get lowercaseCustomerAddressString governanceTokenBalances


getCurrentVotes : CustomerAddress -> GovernanceState -> Maybe Decimal
getCurrentVotes customerAddress { currentVotesBalances } =
    let
        lowercaseCustomerAddressString =
            getCustomerAddressString customerAddress
                |> String.toLower
    in
    Dict.get lowercaseCustomerAddressString currentVotesBalances


getDelegateTotalBalance : CustomerAddress -> GovernanceState -> Maybe Decimal
getDelegateTotalBalance customerAddress { currentVotesBalances } =
    let
        lowercaseCustomerAddressString =
            getCustomerAddressString customerAddress
                |> String.toLower
    in
    Dict.get lowercaseCustomerAddressString currentVotesBalances


getDelegatedAddress : CustomerAddress -> GovernanceState -> Maybe DelegatedAddress
getDelegatedAddress customerAddress { delegatorMapping } =
    let
        lowercaseCustomerAddressString =
            getCustomerAddressString customerAddress
                |> String.toLower
    in
    Dict.get lowercaseCustomerAddressString delegatorMapping
        |> Maybe.map
            (\delegateeAddressString ->
                if delegateeAddressString == "0x0000000000000000000000000000000000000000" then
                    Undelegated

                else if String.toLower delegateeAddressString == lowercaseCustomerAddressString then
                    Self

                else
                    Delegatee (Customer delegateeAddressString)
            )


getDelegatedAddressString : CustomerAddress -> GovernanceState -> Maybe String
getDelegatedAddressString customerAddress { delegatorMapping } =
    let
        lowercaseCustomerAddressString =
            getCustomerAddressString customerAddress
                |> String.toLower
    in
    Dict.get lowercaseCustomerAddressString delegatorMapping


getCompAccruedBalance : CustomerAddress -> GovernanceState -> Maybe Decimal
getCompAccruedBalance customerAddress { compAccruedBalances } =
    let
        lowercaseCustomerAddressString =
            getCustomerAddressString customerAddress
                |> String.toLower
    in
    Dict.get lowercaseCustomerAddressString compAccruedBalances



-- PORTS


port askGovernanceDataPort : { blockNumber : Int, compoundLens : String, governanceTokenAddress : String, customerAddress : String, decimals : Int } -> Cmd msg


askGovernanceData : Int -> ContractAddress -> ContractAddress -> CustomerAddress -> Int -> Cmd msg
askGovernanceData blockNumber (Contract compoundLensAddress) (Contract contractAddress) (Customer customerAddress) decimals =
    askGovernanceDataPort
        { blockNumber = blockNumber
        , compoundLens = compoundLensAddress
        , governanceTokenAddress = contractAddress
        , customerAddress = customerAddress
        , decimals = decimals
        }


port giveGovernanceDataPort : (Value -> msg) -> Sub msg


giveGovernanceData : (Result Json.Decode.Error GovernanceData -> msg) -> Sub msg
giveGovernanceData wrapper =
    let
        decoder =
            Json.Decode.map4 GovernanceData
                (field "customerAddress" decodeCustomerAddress)
                (field "compTokenBalance" decimal)
                (field "currentVotesBalance" decimal)
                (field "delegateeAddress" decodeCustomerAddress)
    in
    giveGovernanceDataPort
        (decodeValue decoder >> wrapper)


port askDelegateToPort : { compTokenAddress : String, customerAddress : String, targetAddress : String } -> Cmd msg


askDelegateTo : ContractAddress -> CustomerAddress -> CustomerAddress -> Cmd msg
askDelegateTo (Contract compTokenAddress) (Customer customerAddress) (Customer targetAddress) =
    askDelegateToPort
        { compTokenAddress = compTokenAddress
        , customerAddress = customerAddress
        , targetAddress = targetAddress
        }


port askCompMetadataPort : { blockNumber : Int, compAddress : String, comptrollerAddress : String, customerAddress : String, compoundLens : String } -> Cmd msg


askCompMetadata : Int -> ContractAddress -> ContractAddress -> CustomerAddress -> ContractAddress -> Cmd msg
askCompMetadata blockNumber (Contract compAddress) (Contract comptrollerAddress) (Customer customerAddress) (Contract compoundLens) =
    askCompMetadataPort
        { blockNumber = blockNumber
        , compAddress = compAddress
        , comptrollerAddress = comptrollerAddress
        , customerAddress = customerAddress
        , compoundLens = compoundLens
        }


port giveCompAccruedPort : (Value -> msg) -> Sub msg


giveCompAccrued : (Result Json.Decode.Error FlywheelData -> msg) -> Sub msg
giveCompAccrued wrapper =
    let
        decoder =
            Json.Decode.map2 FlywheelData
                (field "customerAddress" decodeCustomerAddress)
                (field "compAccrued" decimal)
    in
    giveCompAccruedPort
        (decodeValue decoder >> wrapper)


port askClaimCompPort : { comptrollerAddress : String, customerAddress : String, markets : List String } -> Cmd msg


askClaimComp : ContractAddress -> CustomerAddress -> List String -> Cmd msg
askClaimComp (Contract comptrollerAddress) (Customer customerAddress) markets =
    askClaimCompPort
        { comptrollerAddress = comptrollerAddress
        , customerAddress = customerAddress
        , markets = markets
        }


port askGovernorQueueProposalPort : { governorAddress : String, isBravo : Bool, customerAddress : String, proposalId : Int } -> Cmd msg


askQueueProposal : ContractAddress -> Bool -> CustomerAddress -> Int -> Cmd msg
askQueueProposal (Contract governorAddress) isBravo (Customer customerAddress) proposalId =
    askGovernorQueueProposalPort
        { governorAddress = governorAddress
        , isBravo = isBravo
        , customerAddress = customerAddress
        , proposalId = proposalId
        }


port askGovernorExecuteProposalPort : { governorAddress : String, isBravo : Bool, customerAddress : String, proposalId : Int } -> Cmd msg


askExecuteProposal : ContractAddress -> Bool -> CustomerAddress -> Int -> Cmd msg
askExecuteProposal (Contract governorAddress) isBravo (Customer customerAddress) proposalId =
    askGovernorExecuteProposalPort
        { governorAddress = governorAddress
        , isBravo = isBravo
        , customerAddress = customerAddress
        , proposalId = proposalId
        }
