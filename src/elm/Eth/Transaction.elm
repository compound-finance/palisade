port module Eth.Transaction exposing
    ( Transaction
    , TransactionMsg(..)
    , TransactionState
    , TransactionStatus(..)
    , TransactionType(..)
    , askClearTransactions
    , askStoredTransactions
    , checkTransactions
    , containsTransactionType
    , filteredTransactionsByCToken
    , filteredTransactionsByComptroller
    , getDefaultOldestPendingTrxTime
    , getPendingTransactionsForAccount
    , giveNewTrx
    , giveStoredTransactions
    , giveUpdateTrx
    , handleTrxCountPruning
    , init
    , newBlockCmd
    , storeTransaction
    , storeTransactionUpdate
    , subscriptions
    , transactionDateFormatter
    , transactionEl
    , update
    , updateTransaction
    , view
    )

import CompoundComponents.Console as Console
import CompoundComponents.Eth.Decoders exposing (decodeAssetAddress, decodeContractAddress, decodeCustomerAddress, decodeNetwork, decodeTrxHash, forceMaybe)
import CompoundComponents.Eth.Ethereum as Ethereum exposing (Account(..), AssetAddress(..), ContractAddress(..), CustomerAddress(..), EtherscanLinkValue(..), TrxHash, etherscanLink)
import CompoundComponents.Eth.Network exposing (Network(..), networkId)
import CompoundComponents.Ether.Address
import CompoundComponents.Ether.BNTransaction as BNTransaction exposing (BNTransactionState)
import CompoundComponents.Ether.Hex as Hex exposing (Hex)
import CompoundComponents.Functions exposing (default, demaybeify, handleError)
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (class, id)
import CompoundComponents.Utils.Time
import DateFormat
import Dict exposing (Dict)
import Eth.Config exposing (Config)
import Eth.Token exposing (CToken, CTokenSet, Token)
import Eth.TrxDescriptions exposing (describeGracefulFailure, describeTransaction)
import Html exposing (Html, a, div, h4, label, li, span, text, ul)
import Json.Decode exposing (decodeValue, field, succeed)
import Strings.Translations as Translations
import Time exposing (Posix)


pendingTrxWaitTimeMs : Int
pendingTrxWaitTimeMs =
    CompoundComponents.Utils.Time.hours * 2 * 1000


type TransactionStatus
    = Pending
    | Success
    | Failure
    | AssumedFailure


type TransactionType
    = Faucet
    | Enable
    | Supply
    | Withdraw
    | Borrow
    | RepayBorrow
    | LiquidateBorrow
    | EnterAllMarkets
    | ExitMarket
    | UNKNOWN


type alias Transaction =
    { trxHash : TrxHash
    , network : Network
    , timestamp : Posix
    , contract : ContractAddress
    , customer : CustomerAddress
    , function : String
    , args : List String
    , status : TransactionStatus
    , error : Maybe String
    , expectedNonce : Maybe Int
    }


type alias TransactionState =
    { transactions : List Transaction
    , errors : List String
    }


type alias TransactionUpdate =
    { trxHash : TrxHash
    , transactionStatus : TransactionStatus
    , error : Maybe String
    , trxNonce : Int
    }


type alias NonBNTransactionUpdate =
    { txModule : String
    , txId : Int
    , txHash : Hex
    }


type TransactionMsg
    = NewTransaction Transaction
    | SetTransactions (List Transaction)
    | ClearTransactions
    | UpdateTransaction TransactionUpdate
    | NewNonBNTransaction NonBNTransactionUpdate
    | Error String


init : ( TransactionState, Cmd TransactionMsg )
init =
    ( { transactions = []
      , errors = []
      }
    , askStoredTransactions
    )


update : Maybe Network -> Account -> BNTransactionState -> TransactionMsg -> TransactionState -> ( TransactionState, Cmd TransactionMsg )
update maybeNetwork account bnTransactionState msg ({ transactions } as state) =
    case msg of
        NewTransaction transaction ->
            let
                -- The ports handle populating the expected value with userTrxCount+1 so here
                -- we should check what our local model thinks is the highest nonce and use that instead when
                -- we store the transaction.
                nonceUpdatedTransaction =
                    case ( transaction.expectedNonce, getHighestExpectedNonceFromPendings maybeNetwork account state ) of
                        ( Just trxExpectedNonce, Just currentHighestNonce ) ->
                            if currentHighestNonce >= trxExpectedNonce then
                                { transaction | expectedNonce = Just (currentHighestNonce + 1) }

                            else
                                transaction

                        _ ->
                            transaction
            in
            ( { state | transactions = appendTransaction nonceUpdatedTransaction transactions }, storeTransaction nonceUpdatedTransaction )

        SetTransactions newTransactions ->
            ( { state | transactions = sortedTransactions newTransactions }, Cmd.none )

        UpdateTransaction { trxHash, transactionStatus, error, trxNonce } ->
            ( { state | transactions = updateTransaction trxHash transactionStatus error trxNonce transactions }, storeTransactionUpdate trxHash transactionStatus error )

        ClearTransactions ->
            ( { state | transactions = [] }, askClearTransactions )

        NewNonBNTransaction { txModule, txId, txHash } ->
            let
                maybeFoundBNTransaction =
                    bnTransactionState.transactions
                        |> List.filterMap
                            (\trx ->
                                if txModule == trx.txModuleId && txId == trx.txId then
                                    Just { trx | txHash = Just txHash }

                                else
                                    Nothing
                            )
                        |> List.head
                        |> Maybe.map convertBNTransactionToTransaction
                        |> demaybeify
            in
            case maybeFoundBNTransaction of
                Just foundBNTransaction ->
                    update maybeNetwork account bnTransactionState (NewTransaction foundBNTransaction) state

                Nothing ->
                    ( state, Cmd.none )

        Error error ->
            ( { state | errors = error :: state.errors }, Console.error error )


subscriptions : TransactionState -> Sub TransactionMsg
subscriptions state =
    Sub.batch
        [ giveNewTrx (handleError (Json.Decode.errorToString >> Error) NewTransaction)
        , giveUpdateTrx (handleError (Json.Decode.errorToString >> Error) UpdateTransaction)
        , giveStoredTransactions (handleError (\_ -> ClearTransactions) SetTransactions)
        , giveNewNonBNTrx (handleError (Json.Decode.errorToString >> Error) NewNonBNTransaction)
        ]


newBlockCmd : Int -> Network -> TransactionState -> Cmd TransactionMsg
newBlockCmd blockNumber network { transactions } =
    Cmd.batch (checkTransactions blockNumber network transactions)



-- PORTS


port giveNewTrxPort : (Json.Decode.Value -> msg) -> Sub msg


port giveUpdateTrxPort : (Json.Decode.Value -> msg) -> Sub msg


giveNewTrx : (Result Json.Decode.Error Transaction -> msg) -> Sub msg
giveNewTrx wrapper =
    let
        stage1 =
            Json.Decode.map7 Transaction
                (field "trxHash" decodeTrxHash)
                (field "network" decodeNetwork)
                (field "timestamp" Json.Decode.int |> Json.Decode.map Time.millisToPosix)
                (field "contract" decodeContractAddress)
                (field "customer" decodeCustomerAddress)
                (field "function" Json.Decode.string)
                (field "args" (Json.Decode.list Json.Decode.string))

        decoder =
            Json.Decode.map4
                (<|)
                stage1
                (succeed Pending)
                (succeed Nothing)
                (field "expectedNonce" (Json.Decode.nullable Json.Decode.int))
    in
    giveNewTrxPort
        (decodeValue decoder >> wrapper)


giveUpdateTrx : (Result Json.Decode.Error TransactionUpdate -> msg) -> Sub msg
giveUpdateTrx wrapper =
    let
        decoder =
            Json.Decode.map4 TransactionUpdate
                (field "trxHash" decodeTrxHash)
                (field "status" decodeTransactionStatus)
                (field "error" (Json.Decode.maybe Json.Decode.string))
                (field "trxNonce" Json.Decode.int)
    in
    giveUpdateTrxPort
        (decodeValue decoder >> wrapper)


port giveNewNonBNTrxPort : (Json.Decode.Value -> msg) -> Sub msg


giveNewNonBNTrx : (Result Json.Decode.Error NonBNTransactionUpdate -> msg) -> Sub msg
giveNewNonBNTrx wrapper =
    -- If we get a new trx that is not being watch by BN then let's
    -- create a new non-BNTransaction type so it can be watched
    -- on every new block.
    let
        decoder =
            Json.Decode.map3 NonBNTransactionUpdate
                (field "txModule" Json.Decode.string)
                (field "txId" Json.Decode.int)
                (field "txHash" Hex.decoder)
    in
    giveNewNonBNTrxPort
        (decodeValue decoder >> wrapper)


port checkTrxStatusPort : { blockNumber : Int, trxHash : String } -> Cmd msg


checkTrxStatus : Int -> TrxHash -> Cmd msg
checkTrxStatus blockNumber trxHash =
    checkTrxStatusPort
        { blockNumber = blockNumber
        , trxHash = trxHash
        }


port storeTransactionPort : { trxHash : String, networkId : Int, timestamp : Int, contractAddress : String, customerAddress : String, fun : String, args : List String, status : Maybe Int, error : Maybe String, expectedNonce : Maybe Int } -> Cmd msg


storeTransaction : Transaction -> Cmd msg
storeTransaction { trxHash, network, timestamp, contract, customer, function, args, status, error, expectedNonce } =
    case ( contract, customer ) of
        ( Contract contractAddress, Customer customerAddress ) ->
            storeTransactionPort
                { trxHash = trxHash
                , networkId = networkId network
                , timestamp = Time.posixToMillis timestamp
                , contractAddress = contractAddress
                , customerAddress = customerAddress
                , fun = function
                , args = args
                , status = transactionStatusId status
                , error = error
                , expectedNonce = expectedNonce
                }


port storeTransactionUpdatePort : { trxHash : String, status : Maybe Int, error : Maybe String } -> Cmd msg


storeTransactionUpdate : TrxHash -> TransactionStatus -> Maybe String -> Cmd msg
storeTransactionUpdate trxHash status error =
    storeTransactionUpdatePort
        { trxHash = trxHash
        , status = transactionStatusId status
        , error = error
        }


port askStoredTransactionsPort : {} -> Cmd msg


askStoredTransactions : Cmd msg
askStoredTransactions =
    askStoredTransactionsPort {}


port giveStoredTransactionsPort : (Json.Decode.Value -> msg) -> Sub msg


giveStoredTransactions : (Result Json.Decode.Error (List Transaction) -> msg) -> Sub msg
giveStoredTransactions wrapper =
    let
        decoder =
            Json.Decode.list
                (let
                    stage1 =
                        Json.Decode.map7 Transaction
                            (field "trxHash" decodeTrxHash)
                            (field "network" decodeNetwork)
                            (field "timestamp" Json.Decode.int |> Json.Decode.map Time.millisToPosix)
                            (field "contractAddress" decodeContractAddress)
                            (field "customer" decodeCustomerAddress)
                            (field "fun" Json.Decode.string)
                            (field "args" (Json.Decode.list Json.Decode.string))

                    transactionDecoder =
                        Json.Decode.map4
                            (<|)
                            stage1
                            (field "status" decodeTransactionStatus)
                            (field "error" (Json.Decode.maybe Json.Decode.string))
                            (field "expectedNonce" (Json.Decode.nullable Json.Decode.int))
                 in
                 transactionDecoder
                )
    in
    giveStoredTransactionsPort
        (decodeValue decoder >> wrapper)


port askClearTransactionsPort : {} -> Cmd msg


askClearTransactions : Cmd msg
askClearTransactions =
    askClearTransactionsPort {}



-- FUNCTIONS


appendTransaction : Transaction -> List Transaction -> List Transaction
appendTransaction transaction listOfTransactions =
    transaction :: listOfTransactions


getDefaultOldestPendingTrxTime : Maybe Time.Posix -> Maybe Time.Posix
getDefaultOldestPendingTrxTime maybeCurrentTime =
    maybeCurrentTime
        |> Maybe.map (\time -> CompoundComponents.Utils.Time.subtractMilliFromPosix time pendingTrxWaitTimeMs)


convertBNTransactionToTransaction : BNTransaction.Transaction -> Maybe Transaction
convertBNTransactionToTransaction bnTransaction =
    let
        --TODO: When we convert completely to BNTransaction then we can support the
        --      expanded states from BN in our UI.
        txStatusFromBNTxStatus =
            case bnTransaction.txStatus of
                BNTransaction.AwaitingSig ->
                    Pending

                BNTransaction.Pending ->
                    Pending

                BNTransaction.Sent ->
                    Pending

                BNTransaction.SpeedUp ->
                    Failure

                BNTransaction.Cancel ->
                    Failure

                BNTransaction.Confirmed ->
                    Success

                BNTransaction.Failed ->
                    Failure

                BNTransaction.Dropped ->
                    Failure

                BNTransaction.Rejected ->
                    Failure
    in
    case ( bnTransaction.txHash, bnTransaction.timestamp ) of
        ( Just actualTXHash, Just actualTime ) ->
            { trxHash = Hex.toString actualTXHash
            , network = bnTransaction.network
            , timestamp = actualTime
            , contract = Contract (CompoundComponents.Ether.Address.toString bnTransaction.toAddress)
            , customer = Customer (CompoundComponents.Ether.Address.toString bnTransaction.fromAddress)
            , function = bnTransaction.function
            , args = bnTransaction.args
            , status = txStatusFromBNTxStatus
            , error = Nothing
            , expectedNonce = Nothing
            }
                |> Just

        _ ->
            Nothing


getPendingTransactionsForAccount : Maybe Network -> Account -> Maybe Time.Posix -> Maybe BNTransactionState -> List Transaction -> List Transaction
getPendingTransactionsForAccount maybeNetwork account maybeOldestTrxTimestamp maybeBNTransactionState transactions =
    let
        pendingBNTransactions =
            case maybeBNTransactionState of
                Just bnTransactionState ->
                    BNTransaction.getPendingBNTransactionsForAccount maybeNetwork account bnTransactionState
                        |> List.filterMap convertBNTransactionToTransaction

                _ ->
                    []

        allPendingTransactions =
            pendingBNTransactions ++ transactions
    in
    case ( maybeNetwork, account, maybeOldestTrxTimestamp ) of
        ( Just network, Acct customerAddress _, Just oldestTrxTimeStamp ) ->
            allPendingTransactions
                |> List.filter (.status >> (==) Pending)
                |> List.filter (.network >> (==) network)
                |> List.filter (.customer >> (==) customerAddress)
                |> List.filter (\trx -> Time.posixToMillis trx.timestamp >= Time.posixToMillis oldestTrxTimeStamp)

        ( Just network, Acct customerAddress _, Nothing ) ->
            allPendingTransactions
                |> List.filter (.status >> (==) Pending)
                |> List.filter (.network >> (==) network)
                |> List.filter (.customer >> (==) customerAddress)

        ( _, _, _ ) ->
            []


getAllPendingTransactions : Maybe Network -> List Transaction -> List Transaction
getAllPendingTransactions maybeNetwork transactions =
    case maybeNetwork of
        Just network ->
            transactions
                |> List.filter (.status >> (==) Pending)
                |> List.filter (.network >> (==) network)

        Nothing ->
            []


checkTransactions : Int -> Network -> List Transaction -> List (Cmd msg)
checkTransactions blockNumber network transactions =
    transactions
        |> getAllPendingTransactions (Just network)
        |> List.map (.trxHash >> checkTrxStatus blockNumber)


updateTransaction : TrxHash -> TransactionStatus -> Maybe String -> Int -> List Transaction -> List Transaction
updateTransaction trxHash status error trxNonce transactions =
    transactions
        |> List.map
            (\trx ->
                if trxHash == trx.trxHash then
                    { trx | status = status, error = error }

                else
                    trx
            )


getHighestExpectedNonceFromPendings : Maybe Network -> Account -> TransactionState -> Maybe Int
getHighestExpectedNonceFromPendings maybeNetwork account ({ transactions } as transactionState) =
    getPendingTransactionsForAccount maybeNetwork account Nothing Nothing transactions
        |> List.foldl
            (\trx acc ->
                case ( trx.expectedNonce, acc ) of
                    ( Just computedNonce, Just currentLargestNonce ) ->
                        if computedNonce > currentLargestNonce then
                            Just computedNonce

                        else
                            Just currentLargestNonce

                    ( Just computedNonce, Nothing ) ->
                        Just computedNonce

                    _ ->
                        acc
            )
            Nothing


handleTrxCountPruning : Maybe Network -> Account -> Maybe Int -> TransactionState -> ( TransactionState, Cmd TransactionMsg )
handleTrxCountPruning maybeNetwork account maybeAccountTrxNonce ({ transactions } as transactionState) =
    let
        pendingTransactions =
            getPendingTransactionsForAccount maybeNetwork account Nothing Nothing transactions

        trxHashesAssumedFailedDict =
            pendingTransactions
                |> List.foldl
                    (\trx acc ->
                        case ( trx.expectedNonce, maybeAccountTrxNonce ) of
                            ( Just calculatedNonce, Just accountNonce ) ->
                                if calculatedNonce <= accountNonce then
                                    Dict.insert trx.trxHash 1 acc

                                else
                                    acc

                            _ ->
                                acc
                    )
                    Dict.empty

        updatedTransactions =
            transactions
                |> List.map
                    (\trx ->
                        if Dict.member trx.trxHash trxHashesAssumedFailedDict then
                            { trx | status = AssumedFailure }

                        else
                            trx
                    )

        updateCmds =
            updatedTransactions
                |> List.map
                    (\trx ->
                        let
                            updateMsg =
                                UpdateTransaction
                                    { trxHash = trx.trxHash
                                    , transactionStatus = trx.status
                                    , error = trx.error
                                    , trxNonce = trx.expectedNonce |> Maybe.withDefault 1
                                    }
                        in
                        Cmd.map (\_ -> updateMsg) Cmd.none
                    )
    in
    ( { transactionState | transactions = updatedTransactions }
    , Cmd.batch updateCmds
    )


getTransactionStatus : Maybe Int -> TransactionStatus
getTransactionStatus status =
    case status of
        Just 10 ->
            AssumedFailure

        Just 0 ->
            Failure

        Just 1 ->
            Success

        _ ->
            Pending


transactionStatusId : TransactionStatus -> Maybe Int
transactionStatusId status =
    case status of
        AssumedFailure ->
            Just 10

        Failure ->
            Just 0

        Success ->
            Just 1

        Pending ->
            Nothing


getTransactionType : Transaction -> TransactionType
getTransactionType transaction =
    case transaction.function of
        "allocate" ->
            Faucet

        "drip" ->
            Faucet

        "approve" ->
            Enable

        "mint" ->
            Supply

        "redeem" ->
            Withdraw

        "redeemUnderlying" ->
            Withdraw

        "borrow" ->
            Borrow

        "repayBorrow" ->
            RepayBorrow

        "repayBehalf" ->
            RepayBorrow

        "liquidateBorrow" ->
            LiquidateBorrow

        "enterMarkets" ->
            EnterAllMarkets

        "exitMarket" ->
            ExitMarket

        _ ->
            UNKNOWN


containsTransactionType : TransactionType -> List Transaction -> Bool
containsTransactionType transactionType transactions =
    transactions
        |> List.filter (\t -> getTransactionType t == transactionType)
        |> List.isEmpty
        |> not


decodeTransactionStatus : Json.Decode.Decoder TransactionStatus
decodeTransactionStatus =
    Json.Decode.maybe Json.Decode.int |> Json.Decode.map getTransactionStatus


sortedTransactions : List Transaction -> List Transaction
sortedTransactions transactions =
    List.sortBy (.timestamp >> Time.posixToMillis) transactions
        |> List.reverse


filteredTransactionsByComptroller : List Transaction -> Config -> Network -> CustomerAddress -> List Transaction
filteredTransactionsByComptroller transactions config network customerAddress =
    transactions
        |> List.filter
            (\transaction ->
                (transaction.customer == customerAddress)
                    && (transaction.network == network)
                    && (transaction.contract == config.comptroller)
            )


filteredTransactionsByCToken : List Transaction -> Config -> Network -> CustomerAddress -> CTokenSet -> CToken -> List Transaction
filteredTransactionsByCToken transactions config network customerAddress cTokens selectedToken =
    transactions
        |> List.filter
            (\transaction ->
                (transaction.customer == customerAddress)
                    && (transaction.network == network)
                    && (transaction.function /= "liquidateBorrow")
                    && (let
                            underlyingAddress =
                                Ethereum.assetAddressToContractAddress selectedToken.underlying.assetAddress

                            isCEther =
                                selectedToken.contractAddress == config.cEtherToken.address

                            cTokenAddressString =
                                Ethereum.getContractAddressString selectedToken.contractAddress

                            isFaucetingTrx =
                                case config.maybeFauceteer of
                                    Just fauceteer ->
                                        transaction.contract == fauceteer && Just cTokenAddressString == List.head transaction.args

                                    Nothing ->
                                        False
                        in
                        (transaction.contract == selectedToken.contractAddress)
                            || (transaction.contract == underlyingAddress)
                            || (isCEther && transaction.contract == config.maximillion)
                            || (transaction.function == "exitMarket" && [ cTokenAddressString ] == transaction.args)
                            || (transaction.function == "enterMarkets" && [ cTokenAddressString ] == transaction.args)
                            || isFaucetingTrx
                       )
            )



-- VIEW FUNCTIONS


transactionDateFormatter : Time.Zone -> Time.Posix -> String
transactionDateFormatter timeZone =
    DateFormat.format
        [ DateFormat.monthNameFull
        , DateFormat.text " "
        , DateFormat.dayOfMonthSuffix
        , DateFormat.text ", "
        , DateFormat.hourNumber
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        , DateFormat.amPmLowercase
        ]
        timeZone


view : Translations.Lang -> List Transaction -> Time.Zone -> Maybe Config -> Maybe Network -> CustomerAddress -> CTokenSet -> Maybe CToken -> Html TransactionMsg
view userLanguage transactions timezone maybeConfig maybeNetwork customerAddress cTokens maybeSelectedToken =
    case ( maybeSelectedToken, maybeNetwork, maybeConfig ) of
        ( Just selectedToken, Just network, Just config ) ->
            let
                filteredTransactions =
                    filteredTransactionsByCToken transactions config network customerAddress cTokens selectedToken
            in
            if List.isEmpty filteredTransactions then
                text ""

            else
                div [ class "transactions", class "legacy-panel" ]
                    [ div [ class "header" ] [ h4 [] [ text "Transactions" ] ]
                    , div [ class "" ] (List.map (transactionEl userLanguage timezone maybeNetwork cTokens) filteredTransactions)
                    ]

        _ ->
            text ""


loader : Html msg
loader =
    div [ class "lds-ring" ]
        [ div [] []
        , div [] []
        , div [] []
        , div [] []
        ]


iconClassForTransaction : Transaction -> String
iconClassForTransaction transaction =
    case transaction.function of
        "mint" ->
            "supply"

        "redeem" ->
            "withdraw"

        "redeemUnderlying" ->
            "withdraw"

        "borrow" ->
            "borrow"

        "repayBorrow" ->
            "repay"

        _ ->
            "success"


transactionEl : Translations.Lang -> Time.Zone -> Maybe Network -> CTokenSet -> Transaction -> Html msg
transactionEl userLanguage timezone maybeNetwork cTokens transaction =
    let
        timestamp =
            transactionDateFormatter timezone transaction.timestamp

        description =
            default (describeTransaction userLanguage cTokens transaction.contract transaction.function transaction.args) ""

        borrowBackgroundClass =
            case getTransactionType transaction of
                Borrow ->
                    " borrow"

                RepayBorrow ->
                    " borrow"

                _ ->
                    ""

        ( className, statusEl ) =
            case ( transaction.status, transaction.error ) of
                ( Success, Nothing ) ->
                    ( "success"
                    , span [ class ("status " ++ iconClassForTransaction transaction) ] []
                    )

                ( Pending, _ ) ->
                    ( "pending", span [ class ("status accent-bg" ++ borrowBackgroundClass) ] [ loader ] )

                ( _, _ ) ->
                    ( "failure"
                    , span [ class "status fail" ] []
                    )

        errorEl =
            case transaction.error of
                Just error ->
                    div [ class "transaction-error" ]
                        [ text (describeGracefulFailure error)
                        ]

                Nothing ->
                    text ""
    in
    etherscanLink maybeNetwork
        (TransactionHash transaction.trxHash)
        [ class ("asset " ++ className) ]
        [ div [ class "details" ]
            [ div [ class "identity" ]
                [ statusEl
                , text description
                ]
            , div [ class "timestamp text-right" ] [ text timestamp ]
            ]
        , errorEl
        ]
