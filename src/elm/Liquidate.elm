port module Liquidate exposing
    ( InternalMsg
    , Model
    , Msg
    , Translator
    , emptyState
    , init
    , loadAtRiskAccounts
    , subscriptions
    , translator
    , update
    , view
    )

import CompoundApi.Presidio.Accounts.Decoders exposing (accountsResponseDecoder)
import CompoundApi.Presidio.Accounts.Models exposing (AccountResponse)
import CompoundApi.Presidio.Accounts.Urls
import CompoundComponents.Console as Console
import CompoundComponents.DisplayCurrency as DisplayCurrency exposing (DisplayCurrency)
import CompoundComponents.Eth.Decoders exposing (decimal, decodeAssetAddress, decodeCustomerAddress)
import CompoundComponents.Eth.Ethereum as Ethereum exposing (Account(..), AssetAddress(..), ContractAddress(..), CustomerAddress(..), EtherscanLinkValue(..), etherscanLink, getAssetAddressString, getContractAddressString, getCustomerAddressString)
import CompoundComponents.Eth.Network exposing (Network(..))
import CompoundComponents.Eth.TokenMath as TokenMath
import CompoundComponents.Functions as Functions
import CompoundComponents.Utils.CompoundHtmlAttributes exposing (class, id, placeholder, type_, value)
import CompoundComponents.Utils.Markup exposing (disabled, selected)
import CompoundComponents.Utils.NumberFormatter as NumberFormatter
import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Eth.Compound exposing (CompoundState)
import Eth.Config exposing (Config, getCTokenAddresses)
import Eth.Oracle exposing (OracleState)
import Eth.Token exposing (CTokenSet, Token, TokenMsg, TokenState, getCTokenByAddress, getUnderlyingTokenDecimals, getUnderlyingTokenSymbol)
import Eth.Transaction exposing (Transaction, TransactionState)
import Eth.TrxDescriptions
import Html exposing (Html, button, div, h4, input, label, option, p, select, span, text)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode
import Maybe
import Preferences exposing (Preferences)
import Strings.Translations as Translations
import Time
import Utils.Http


minBorrowEthAmount : Decimal
minBorrowEthAmount =
    Decimal.fromString "0.2"
        |> Maybe.withDefault Decimal.zero


maxHealthAmount : Decimal
maxHealthAmount =
    Decimal.fromString "1.00"
        |> Maybe.withDefault Decimal.one


type LiquidateBorrowInput
    = Normal ( String, Maybe Decimal )
    | Empty


type alias BorrowerCToken =
    { address : Ethereum.ContractAddress
    , underlyingSymbol : String
    , underlyingBorrowBalance : Decimal
    , underlyingSupplyBalance : Decimal
    }


type alias BorrowerDetail =
    { borrowerAddress : Ethereum.CustomerAddress
    , maybeAccountLiquidityValueEth : Maybe Decimal
    , maybeSumCollateralEth : Maybe Decimal
    , maybeSumBorrowsEth : Maybe Decimal
    , assetsBorrowed : List BorrowerCToken
    , availableCollateral : List BorrowerCToken
    , liquidateDetails : LiquidateBorrowDetails
    , liquidateResponseDetails : Maybe LiquidateBorrowDetails
    }


type alias LiquidateBorrowDetails =
    { borrowerAddress : Ethereum.CustomerAddress
    , maybeBorrowToCloseAddress : Maybe AssetAddress
    , borrowCloseAmountInput : LiquidateBorrowInput
    , desiredCollateralAddress : Maybe AssetAddress
    }


type alias LiquidationPanelDetails =
    { borrowerAddress : Ethereum.CustomerAddress
    , maybeLiquidationIncentive : Maybe Decimal
    , maybeCloseFactor : Maybe Decimal
    , maybeSumCollateralEth : Maybe Decimal
    , maybeSumBorrowsEth : Maybe Decimal
    , maybeBorrowAddress : Maybe AssetAddress
    , maybeBorrowCurrent : Maybe Decimal
    , maybeBorrowAssetPriceUsd : Maybe Decimal
    , maybeCollateralAddress : Maybe AssetAddress
    , maybeCollateralCurrent : Maybe Decimal
    , maybeCollateralAssetPriceUsd : Maybe Decimal
    , displayCurrency : DisplayCurrency
    }


type alias Model =
    { outstandingBorrows : Dict String BorrowerDetail
    , maybeSelectedBorrow : Maybe BorrowerDetail
    , errors : List String
    }


type OutMsg
    = WrappedTokenMsg TokenMsg


type InternalMsg
    = SelectBorrow BorrowerDetail
    | SetBorrowedAsset String
    | SetDesiredCollateral String
    | LiquidateAmountInputChanged String
    | AskLiquidate CTokenSet LiquidateBorrowDetails
    | LiquidateResponse LiquidateBorrowDetails
    | PresidioAccountsResponse (Result Http.Error AccountResponse)
    | Error String


type Msg
    = ForSelf InternalMsg
    | ForParent OutMsg


type alias TranslationDictionary msg =
    { onInternalMessage : InternalMsg -> msg
    , onWrappedTokenMsg : TokenMsg -> msg
    }


type alias Translator msg =
    Msg -> msg


translator : TranslationDictionary msg -> Translator msg
translator { onInternalMessage, onWrappedTokenMsg } msg =
    case msg of
        ForSelf internal ->
            onInternalMessage internal

        ForParent (WrappedTokenMsg tokenMsg) ->
            onWrappedTokenMsg tokenMsg


emptyState : Model
emptyState =
    { outstandingBorrows = Dict.empty
    , maybeSelectedBorrow = Nothing
    , errors = []
    }


init : Bool -> Config -> Account -> Maybe Int -> Dict String String -> Network -> ( Model, Cmd Msg )
init shouldLoadAtRiskAccounts config account maybeBlockNumber apiBaseUrlMap network =
    let
        initCmd =
            if shouldLoadAtRiskAccounts then
                loadAtRiskAccounts config account maybeBlockNumber apiBaseUrlMap network

            else
                Cmd.none
    in
    ( emptyState
    , initCmd
    )


getCloseAmouuntPanelInput : String -> LiquidateBorrowInput
getCloseAmouuntPanelInput newText =
    if String.isEmpty newText then
        Empty

    else
        Normal ( newText, Decimal.fromString newText )


handleSelectedBorrowUpdate : Model -> InternalMsg -> Model
handleSelectedBorrowUpdate currentModel msg =
    let
        updatedSelectedBorrow =
            currentModel.maybeSelectedBorrow
                |> Maybe.andThen
                    (\selectedBorrow ->
                        let
                            currentLiquidateDetails =
                                selectedBorrow.liquidateDetails

                            updatedLiquidateDetails =
                                case msg of
                                    SetBorrowedAsset assetAddress ->
                                        { currentLiquidateDetails | maybeBorrowToCloseAddress = Just (Asset assetAddress) }

                                    LiquidateAmountInputChanged newText ->
                                        { currentLiquidateDetails | borrowCloseAmountInput = getCloseAmouuntPanelInput newText }

                                    SetDesiredCollateral assetAddress ->
                                        { currentLiquidateDetails | desiredCollateralAddress = Just (Asset assetAddress) }

                                    _ ->
                                        currentLiquidateDetails
                        in
                        Just { selectedBorrow | liquidateDetails = updatedLiquidateDetails }
                    )
    in
    { currentModel | maybeSelectedBorrow = updatedSelectedBorrow }


buildLiquidateRequestCmd : Config -> Account -> CTokenSet -> LiquidateBorrowDetails -> InternalMsg -> Cmd Msg
buildLiquidateRequestCmd config account cTokens liquidateDetails msg =
    let
        maybeDappUserAddress =
            case account of
                Ethereum.Acct customerAddress _ ->
                    Just customerAddress

                _ ->
                    Nothing

        maybeBorrowCloseAmount =
            case liquidateDetails.borrowCloseAmountInput of
                Normal ( _, maybeInputAmount ) ->
                    maybeInputAmount

                Empty ->
                    Nothing

        requestCmd =
            Maybe.withDefault Cmd.none
                (Functions.map4
                    maybeDappUserAddress
                    liquidateDetails.maybeBorrowToCloseAddress
                    liquidateDetails.desiredCollateralAddress
                    maybeBorrowCloseAmount
                    (\dappUserAddress borrowToCloseAddress desiredCollateralAddress borrowCloseAmount ->
                        let
                            maybeBorrowedCToken =
                                getCTokenByAddress cTokens (getAssetAddressString borrowToCloseAddress)

                            desiredTokenDecimals =
                                getUnderlyingTokenDecimals cTokens (getAssetAddressString desiredCollateralAddress)
                        in
                        case ( maybeBorrowedCToken, desiredTokenDecimals, msg ) of
                            ( Just borrowedCToken, Just desiredDecimals, AskLiquidate _ _ ) ->
                                askLiquidate
                                    borrowedCToken.contractAddress
                                    dappUserAddress
                                    liquidateDetails.borrowerAddress
                                    (TokenMath.getTokenWeiStr borrowCloseAmount borrowedCToken.underlying.decimals)
                                    borrowedCToken.underlying.decimals
                                    desiredCollateralAddress
                                    desiredDecimals
                                    (Eth.Token.isCEtherToken config borrowedCToken)

                            _ ->
                                Cmd.none
                    )
                )
    in
    requestCmd


update : InternalMsg -> Model -> Config -> Account -> TokenState -> OracleState -> ( Model, Cmd Msg )
update msg model config account tokenState oracleState =
    case msg of
        SelectBorrow borrowerDetail ->
            ( { model | maybeSelectedBorrow = Just borrowerDetail }, Cmd.none )

        SetBorrowedAsset _ ->
            ( handleSelectedBorrowUpdate model msg, Cmd.none )

        LiquidateAmountInputChanged _ ->
            ( handleSelectedBorrowUpdate model msg, Cmd.none )

        SetDesiredCollateral _ ->
            ( handleSelectedBorrowUpdate model msg, Cmd.none )

        AskLiquidate cTokens liquidateBorrowDetails ->
            ( model, buildLiquidateRequestCmd config account cTokens liquidateBorrowDetails msg )

        LiquidateResponse _ ->
            --TODO: Need to figure out what if anything
            -- that we can actually show here. Likely would wanna show recent
            -- liquidates maybe like we do with recent transactions?
            ( model, Cmd.none )

        PresidioAccountsResponse result ->
            case result of
                Ok accountResponse ->
                    let
                        updatedOutstandingBorrows =
                            List.foldl (updateBorrowerDetailWithPresidioAccount config tokenState oracleState) Dict.empty accountResponse.accounts
                    in
                    ( { model | outstandingBorrows = updatedOutstandingBorrows }, Cmd.none )

                Err errMsg ->
                    ( model, Console.error ("Error getting account values from Risk API, " ++ Utils.Http.showError errMsg) )

        Error error ->
            ( { model | errors = error :: model.errors }, Console.error error )


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ giveLiquidate (Functions.handleError (ForSelf << Error << Json.Decode.errorToString) (ForSelf << LiquidateResponse))
        ]


view : Translations.Lang -> Time.Zone -> Maybe Config -> Maybe Network -> Account -> CompoundState -> TokenState -> OracleState -> Preferences -> TransactionState -> Model -> Html Msg
view userLanguage currentTimeZone maybeConfig maybeNetwork account compoundState tokenState oracleState preferences transactionState model =
    let
        maybeEtherUsdPrice =
            maybeConfig
                |> Maybe.andThen (\config -> Eth.Oracle.getEtherPrice config tokenState oracleState)
    in
    div [ id "dApplication", class "liquidate-container" ]
        [ div [ id "top", class "container" ]
            [ outstandingBorrowsView maybeEtherUsdPrice preferences.displayCurrency model
            ]
        , div [ class "container" ]
            [ selectedBorrowerView maybeEtherUsdPrice compoundState.maybeCloseFactor compoundState.maybeLiquidationIncentive tokenState oracleState preferences.displayCurrency model.maybeSelectedBorrow
            , recentActivityView userLanguage currentTimeZone transactionState.transactions maybeNetwork account tokenState.cTokens
            ]
        ]



-- Get accounts


loadPresidioBorrows : Dict String String -> Network -> Cmd Msg
loadPresidioBorrows apiBaseUrlMap network =
    let
        maybeAccountsUrl =
            { addresses = []
            , min_borrow_value_in_eth = Just minBorrowEthAmount
            , max_health = Just maxHealthAmount
            , block_number = 0
            , page_size = 100
            , page_number = 1
            }
                |> CompoundApi.Presidio.Accounts.Urls.accountsRequestUrl apiBaseUrlMap network

        accountsRequestCmd =
            case maybeAccountsUrl of
                Just accountsUrl ->
                    let
                        accountsRequestHttpGet =
                            Http.get accountsUrl accountsResponseDecoder
                    in
                    Http.send (ForSelf << PresidioAccountsResponse) accountsRequestHttpGet

                _ ->
                    Cmd.none
    in
    accountsRequestCmd


loadAtRiskAccounts : Config -> Account -> Maybe Int -> Dict String String -> Network -> Cmd Msg
loadAtRiskAccounts config account maybeBlockNumber apiBaseUrlMap network =
    loadPresidioBorrows apiBaseUrlMap network


updateBorrowerDetailWithPresidioAccount : Config -> TokenState -> OracleState -> CompoundApi.Presidio.Accounts.Models.Account -> Dict String BorrowerDetail -> Dict String BorrowerDetail
updateBorrowerDetailWithPresidioAccount config tokenState oracleState apiAccount borrowersDict =
    let
        borrowerAddressString =
            apiAccount.address

        currentBorrowerDetail =
            case Dict.get borrowerAddressString borrowersDict of
                Just existingBorrowerDetail ->
                    existingBorrowerDetail

                _ ->
                    BorrowerDetail
                        (Ethereum.Customer borrowerAddressString)
                        Nothing
                        Nothing
                        Nothing
                        []
                        []
                        (LiquidateBorrowDetails (Ethereum.Customer borrowerAddressString) Nothing Empty Nothing)
                        Nothing

        accountLiquidityEth =
            Decimal.sub apiAccount.total_collateral_value_in_eth apiAccount.total_borrow_value_in_eth

        getUnderlyingSymbol cTokenAddress =
            getUnderlyingTokenSymbol tokenState.cTokens cTokenAddress
                |> Maybe.withDefault "UNKNOWN"

        cTokensBorrowed =
            apiAccount.tokens
                |> List.filterMap
                    (\accountCToken ->
                        if Decimal.gt accountCToken.borrow_balance_underlying Decimal.zero then
                            BorrowerCToken
                                (Contract accountCToken.address)
                                (getUnderlyingSymbol accountCToken.address)
                                accountCToken.borrow_balance_underlying
                                accountCToken.supply_balance_underlying
                                |> Just

                        else
                            Nothing
                    )

        cTokensAsCollateral =
            apiAccount.tokens
                |> List.filterMap
                    (\accountCToken ->
                        if Decimal.gt accountCToken.supply_balance_underlying Decimal.zero then
                            BorrowerCToken
                                (Contract accountCToken.address)
                                (getUnderlyingSymbol accountCToken.address)
                                accountCToken.borrow_balance_underlying
                                accountCToken.supply_balance_underlying
                                |> Just

                        else
                            Nothing
                    )

        borrowerDetailUpdatedTokens =
            { currentBorrowerDetail | assetsBorrowed = cTokensBorrowed, availableCollateral = cTokensAsCollateral }

        updatedBorrowerDetail =
            { borrowerDetailUpdatedTokens
                | maybeAccountLiquidityValueEth = Just accountLiquidityEth
                , maybeSumCollateralEth = Just apiAccount.total_collateral_value_in_eth
                , maybeSumBorrowsEth = Just apiAccount.total_borrow_value_in_eth
            }
    in
    Dict.insert borrowerAddressString updatedBorrowerDetail borrowersDict


getBorrowOverCollateral : BorrowerDetail -> Maybe Decimal
getBorrowOverCollateral borrowerDetail =
    case ( borrowerDetail.maybeSumCollateralEth, borrowerDetail.maybeSumBorrowsEth ) of
        ( Just sumCollateralEth, Just sumBorrowsEth ) ->
            if Decimal.gt sumCollateralEth Decimal.zero then
                Decimal.fastdiv sumBorrowsEth sumCollateralEth

            else
                Nothing

        _ ->
            Nothing


outstandingBorrowsView : Maybe Decimal -> DisplayCurrency -> Model -> Html Msg
outstandingBorrowsView maybeEtherUsdPrice displayCurrency { outstandingBorrows, maybeSelectedBorrow } =
    let
        outstandingBorrowsComparison : BorrowerDetail -> BorrowerDetail -> Order
        outstandingBorrowsComparison a b =
            let
                maybeCollateralizationRatioA =
                    getBorrowOverCollateral a

                maybeCollateralizationRatioB =
                    getBorrowOverCollateral b
            in
            case ( maybeCollateralizationRatioA, maybeCollateralizationRatioB ) of
                ( Just _, Nothing ) ->
                    LT

                ( Nothing, Just _ ) ->
                    GT

                ( Just collateralizationA, Just collateralizationB ) ->
                    Decimal.compare collateralizationB collateralizationA

                ( Nothing, Nothing ) ->
                    EQ
    in
    div [ class "panel" ]
        ([ div [ class "panel__labels" ]
            [ div [ class "col-xs-3" ] [ label [] [ text "Total Borrowed" ] ]
            , div [ class "col-xs-2" ] [ label [] [ text "Borrow / Collateral" ] ]
            , div [ class "col-xs-7" ] [ label [] [ text "Borrower Address" ] ]
            ]
         ]
            ++ (outstandingBorrows
                    |> Dict.values
                    |> List.sortWith outstandingBorrowsComparison
                    |> List.map (oustandingBorrowRow maybeEtherUsdPrice displayCurrency maybeSelectedBorrow)
               )
        )


oustandingBorrowRow : Maybe Decimal -> DisplayCurrency -> Maybe BorrowerDetail -> BorrowerDetail -> Html Msg
oustandingBorrowRow maybeEtherUsdPrice displayCurrency maybeSelectedBorrow borrowerDetail =
    let
        borrowerAddressString =
            getCustomerAddressString borrowerDetail.borrowerAddress

        ( collateralRatioString, extraCollateralRatioClass, totalBorrowsString ) =
            case ( borrowerDetail.maybeSumCollateralEth, borrowerDetail.maybeSumBorrowsEth ) of
                ( Just sumCollateralEth, Just _ ) ->
                    let
                        totalCollateralEquivalentString =
                            DisplayCurrency.formatDisplayCurrencyInNumberSpec
                                displayCurrency
                                maybeEtherUsdPrice
                                (DisplayCurrency.EthValue sumCollateralEth)

                        maybeHealthRatio =
                            getBorrowOverCollateral borrowerDetail
                    in
                    ( maybeHealthRatio
                        |> Maybe.map NumberFormatter.formatCollateralRatio
                        |> Maybe.withDefault "-"
                    , maybeHealthRatio
                        |> Maybe.map
                            (\health ->
                                if Decimal.gt health Decimal.one then
                                    " warning"

                                else
                                    ""
                            )
                        |> Maybe.withDefault ""
                    , totalCollateralEquivalentString
                    )

                _ ->
                    ( "-", "-", "-" )

        isSelected =
            case maybeSelectedBorrow of
                Just selectedBorrow ->
                    borrowerAddressString == getCustomerAddressString selectedBorrow.borrowerAddress

                Nothing ->
                    False

        rowStyling =
            if isSelected then
                "account row active"

            else
                "account row"
    in
    div [ class rowStyling, onClick <| ForSelf (SelectBorrow borrowerDetail) ]
        [ div [ class "col-sm-3" ] [ text totalBorrowsString ]
        , div [ class ("col-sm-2" ++ extraCollateralRatioClass) ] [ text collateralRatioString ]
        , div [ class "col-sm-7 address" ] [ text borrowerAddressString ]
        ]


getAssetPriceSymbolAndBalance : Bool -> BorrowerDetail -> TokenState -> OracleState -> Maybe AssetAddress -> ( Maybe Decimal, Maybe String, Maybe BorrowerCToken )
getAssetPriceSymbolAndBalance isBorrowedAsset borrowerDetail tokenState oracleState maybeAssetAddress =
    let
        maybeCToken =
            maybeAssetAddress
                |> Maybe.map getAssetAddressString
                |> Maybe.andThen (Eth.Token.getCTokenByAddress tokenState.cTokens)
    in
    case maybeCToken of
        Just cToken ->
            let
                apiBorrowedToken =
                    if isBorrowedAsset then
                        borrowerDetail.assetsBorrowed
                            |> List.filter (\borrowerCToken -> borrowerCToken.address == cToken.contractAddress)
                            |> List.head

                    else
                        borrowerDetail.availableCollateral
                            |> List.filter (\borrowerCToken -> borrowerCToken.address == cToken.contractAddress)
                            |> List.head
            in
            ( Eth.Oracle.getOraclePrice oracleState cToken.underlying
            , Just cToken.underlying.symbol
            , apiBorrowedToken
            )

        Nothing ->
            ( Nothing, Nothing, Nothing )


formatTokenBalance : Maybe Decimal -> Maybe String -> String
formatTokenBalance maybeTokenBalance maybeTokenSymbol =
    case ( maybeTokenBalance, maybeTokenSymbol ) of
        ( Just tokenBalance, Just tokenSymbol ) ->
            NumberFormatter.formatTokenBalanceWithSymbol tokenBalance tokenSymbol

        _ ->
            "..."


getCollateralPriceWithDiscountIncentiveString : Maybe Decimal -> DisplayCurrency -> Maybe Decimal -> Maybe Decimal -> String
getCollateralPriceWithDiscountIncentiveString maybeEtherUsdPrice displayCurrency maybeCollateralPrice maybeLiquidationIncentive =
    case ( maybeCollateralPrice, maybeLiquidationIncentive ) of
        ( Just collateralPrice, Just liquidationIncentive ) ->
            let
                liqudationIncentiveAsPercentOfPrice =
                    Decimal.sub (Decimal.fromInt 2) liquidationIncentive

                priceAfterDiscount =
                    Decimal.mul collateralPrice liqudationIncentiveAsPercentOfPrice

                priceString =
                    DisplayCurrency.formatDisplayCurrencyInNumberSpec
                        displayCurrency
                        maybeEtherUsdPrice
                        (DisplayCurrency.UsdValue priceAfterDiscount)
            in
            priceString ++ " (" ++ NumberFormatter.formatPercentage liqudationIncentiveAsPercentOfPrice ++ ")"

        _ ->
            "..."


selectedBorrowerView : Maybe Decimal -> Maybe Decimal -> Maybe Decimal -> TokenState -> OracleState -> DisplayCurrency -> Maybe BorrowerDetail -> Html Msg
selectedBorrowerView maybeEtherUsdPrice maybeCloseFactor maybeLiquidationIncentive tokenState oracleState displayCurrency maybeBorrowerDetail =
    case maybeBorrowerDetail of
        Just borrowerDetail ->
            let
                ( maybeBorowPriceUsd, maybeBorrowSymbol, maybeBorrowedAccountCToken ) =
                    getAssetPriceSymbolAndBalance True borrowerDetail tokenState oracleState borrowerDetail.liquidateDetails.maybeBorrowToCloseAddress

                ( maybeCollateralPriceUsd, maybeCollateralSymbol, maybeCollateralAccountCToken ) =
                    getAssetPriceSymbolAndBalance False borrowerDetail tokenState oracleState borrowerDetail.liquidateDetails.desiredCollateralAddress

                maybeBorrowBalance =
                    maybeBorrowedAccountCToken
                        |> Maybe.map .underlyingBorrowBalance

                maybeCollateralBalance =
                    maybeCollateralAccountCToken
                        |> Maybe.map .underlyingSupplyBalance

                panelDetails =
                    LiquidationPanelDetails
                        borrowerDetail.borrowerAddress
                        maybeLiquidationIncentive
                        maybeCloseFactor
                        borrowerDetail.maybeSumCollateralEth
                        borrowerDetail.maybeSumBorrowsEth
                        borrowerDetail.liquidateDetails.maybeBorrowToCloseAddress
                        maybeBorrowBalance
                        maybeBorowPriceUsd
                        borrowerDetail.liquidateDetails.desiredCollateralAddress
                        maybeCollateralBalance
                        maybeCollateralPriceUsd
                        displayCurrency

                borrowedTokensAvailable =
                    borrowerDetail.assetsBorrowed
                        |> List.sortBy .underlyingSymbol
                        |> List.map
                            (\borrowerCToken ->
                                ( getContractAddressString borrowerCToken.address, borrowerCToken.underlyingSymbol, Just borrowerCToken.underlyingBorrowBalance )
                            )

                preferredCurrencyFormatter maybeAssetPriceUsd =
                    maybeAssetPriceUsd
                        |> Maybe.map
                            (\assetPriceUsd ->
                                DisplayCurrency.formatDisplayCurrencyInNumberSpec
                                    displayCurrency
                                    maybeEtherUsdPrice
                                    (DisplayCurrency.UsdValue assetPriceUsd)
                            )
                        |> Maybe.withDefault "..."

                borrowPriceString =
                    preferredCurrencyFormatter maybeBorowPriceUsd

                collateralPriceString =
                    preferredCurrencyFormatter maybeCollateralPriceUsd

                collateralTokensAvailable =
                    borrowerDetail.availableCollateral
                        |> List.sortBy .underlyingSymbol
                        |> List.map
                            (\borrowerCToken ->
                                ( getContractAddressString borrowerCToken.address, borrowerCToken.underlyingSymbol, Just borrowerCToken.underlyingSupplyBalance )
                            )
            in
            div [ class "container" ]
                [ div [ class "row" ]
                    [ div [ class "col-sm-6" ]
                        [ div [ class "legacy-panel" ]
                            [ div [ class "header" ]
                                [ span [] [ text "Repay Borrow" ]
                                ]
                            , div [ class "content" ]
                                [ borrowedTokensAvailable
                                    |> selectorView True panelDetails.maybeBorrowAddress
                                , div [ class "calculation" ]
                                    [ span [] [ text "Price" ]
                                    , span [] [ text borrowPriceString ]
                                    ]
                                , div [ class "calculation" ]
                                    [ span [] [ text "User Borrowed" ]
                                    , span [] [ text (formatTokenBalance maybeBorrowBalance maybeBorrowSymbol) ]
                                    ]
                                , div [ class "calculation" ]
                                    [ span [] [ text "Max Quantity" ]
                                    , span [] [ text (formatTokenBalance (getMaxClosable panelDetails) maybeBorrowSymbol) ]
                                    ]
                                , getBorrowInputBox borrowerDetail
                                ]
                            ]
                        ]
                    , div [ class "col-sm-6" ]
                        [ div [ class "legacy-panel" ]
                            [ div [ class "header" ]
                                [ span [] [ text "Receive Collateral" ]
                                ]
                            , div [ class "content" ]
                                [ collateralTokensAvailable
                                    |> selectorView False panelDetails.maybeCollateralAddress
                                , div [ class "calculation" ]
                                    [ span [] [ text "Price" ]
                                    , span [] [ text collateralPriceString ]
                                    ]
                                , div [ class "calculation" ]
                                    [ span [] [ text "Bonus Price" ]
                                    , span [] [ text (getCollateralPriceWithDiscountIncentiveString maybeEtherUsdPrice displayCurrency maybeCollateralPriceUsd maybeLiquidationIncentive) ]
                                    ]
                                , div [ class "calculation" ]
                                    [ span [] [ text "User Supplied" ]
                                    , span [] [ text (formatTokenBalance maybeCollateralBalance maybeCollateralSymbol) ]
                                    ]
                                , div [ class "calculation" ]
                                    [ span [] [ text "You Will Receive" ]
                                    , span [] [ text (formatTokenBalance (getAmountSeize borrowerDetail.liquidateDetails.borrowCloseAmountInput panelDetails) maybeCollateralSymbol) ]
                                    ]
                                , button
                                    [ class "button main"
                                    , onClick <|
                                        ForSelf
                                            (AskLiquidate tokenState.cTokens borrowerDetail.liquidateDetails)
                                    ]
                                    [ text "Liquidate" ]
                                ]
                            ]
                        ]
                    ]
                ]

        Nothing ->
            div [ class "container-small empty" ]
                [ h4 [ class "title" ] [ text "Liquidate At-Risk Positions" ]
                , p [] [ text "By closing at-risk borrowing positions, you will receive discounted collateral. Select an account to see more." ]
                ]


{-| Max Closable is the min of each of the 3:
-- 1. borrowCurrent
-- 2. supplyCurrent / (1+liquidationDiscount) \* oracleassetCollateral / oracleassetBorrow
-- 3. shortfall/ ( oracleassetBorrow \* (collateralRatio-liquidiationDiscount-1))
-}
getMaxClosable : LiquidationPanelDetails -> Maybe Decimal
getMaxClosable panelDetails =
    Functions.andThen6
        panelDetails.maybeLiquidationIncentive
        panelDetails.maybeCloseFactor
        panelDetails.maybeBorrowCurrent
        panelDetails.maybeBorrowAssetPriceUsd
        panelDetails.maybeCollateralCurrent
        panelDetails.maybeCollateralAssetPriceUsd
        (\liquidationIncentive closeFactor borrowCurrent borrowAssetPriceUsd collateralCurrent collateralAssetPriceUsd ->
            let
                maybeCollateralOverLiquidationIncentive =
                    Decimal.fastdiv collateralCurrent liquidationIncentive

                maybeCollateralOverBorrowPriceRatio =
                    Decimal.fastdiv collateralAssetPriceUsd borrowAssetPriceUsd

                maybeDiscountedBorrowDenominatedCollateral =
                    case ( maybeCollateralOverLiquidationIncentive, maybeCollateralOverBorrowPriceRatio ) of
                        ( Just collateralOverOnePlusLiquidationDiscount, Just collateralOverBorrowPriceRatio ) ->
                            Just (Decimal.mul collateralOverOnePlusLiquidationDiscount collateralOverBorrowPriceRatio)

                        _ ->
                            Nothing

                maxBorrowClosable =
                    Decimal.mul borrowCurrent closeFactor
            in
            case maybeDiscountedBorrowDenominatedCollateral of
                Just discountedBorrowDenominatedCollateral ->
                    Just (Functions.decimalMin maxBorrowClosable discountedBorrowDenominatedCollateral)

                _ ->
                    Just maxBorrowClosable
        )



-- amountSize = amountClose * (oracleassetBorrow / oracleassetCollateral) * (1+liquidationDiscount)


getAmountSeize : LiquidateBorrowInput -> LiquidationPanelDetails -> Maybe Decimal
getAmountSeize borrowCloseAmountInput panelDetails =
    Functions.andThen3
        panelDetails.maybeBorrowAssetPriceUsd
        panelDetails.maybeCollateralAssetPriceUsd
        panelDetails.maybeLiquidationIncentive
        (\borrowAssetPriceEth collateralAssetPriceUsd liquidationIncentive ->
            let
                maybeAmountClose =
                    case borrowCloseAmountInput of
                        Normal ( _, maybeBorrowCloseAmount ) ->
                            maybeBorrowCloseAmount

                        Empty ->
                            Nothing

                maybeBorrowOverCollateralPriceRatio =
                    Decimal.fastdiv borrowAssetPriceEth collateralAssetPriceUsd
            in
            case ( maybeAmountClose, maybeBorrowOverCollateralPriceRatio ) of
                ( Just amountClose, Just borrowOverCollateralPriceRatio ) ->
                    Decimal.mul amountClose borrowOverCollateralPriceRatio
                        |> Decimal.mul liquidationIncentive
                        |> Just

                _ ->
                    Nothing
        )


getBorrowInputBox : BorrowerDetail -> Html Msg
getBorrowInputBox borrowerDetail =
    case borrowerDetail.liquidateDetails.borrowCloseAmountInput of
        Normal ( inputText, _ ) ->
            input [ type_ "text", value inputText, onInput (ForSelf << LiquidateAmountInputChanged) ] []

        Empty ->
            input [ type_ "text", value "", placeholder "Amount to Close", onInput (ForSelf << LiquidateAmountInputChanged) ] []


selectorView : Bool -> Maybe AssetAddress -> List ( String, String, Maybe Decimal ) -> Html Msg
selectorView isBorrowSelector maybeSelectedAssetAddress tokensAvailableTupleList =
    let
        onInputMsg =
            if isBorrowSelector then
                ForSelf << SetBorrowedAsset

            else
                ForSelf << SetDesiredCollateral

        placeholderOptionText =
            if isBorrowSelector then
                "Select Borrow To Close"

            else
                "Select Desired Collateral"

        placeholderOptionStyling =
            [ value "", disabled ]
                ++ (case maybeSelectedAssetAddress of
                        Just _ ->
                            []

                        Nothing ->
                            [ selected ]
                   )
    in
    select [ onInput onInputMsg ]
        ([ option placeholderOptionStyling [ text placeholderOptionText ] ]
            ++ (tokensAvailableTupleList
                    |> List.map
                        (\( assetAddress, symbol, _ ) ->
                            let
                                optionStyling =
                                    [ value assetAddress ]
                                        ++ (case maybeSelectedAssetAddress of
                                                Just selectedAssetAddress ->
                                                    if assetAddress == getAssetAddressString selectedAssetAddress then
                                                        [ selected ]

                                                    else
                                                        []

                                                Nothing ->
                                                    []
                                           )
                            in
                            option optionStyling [ text symbol ]
                        )
               )
        )


recentActivityView : Translations.Lang -> Time.Zone -> List Transaction -> Maybe Network -> Account -> CTokenSet -> Html Msg
recentActivityView userLanguage timezone transactions maybeNetwork account cTokens =
    let
        filteredTransactions =
            case ( maybeNetwork, account ) of
                ( Just network, Acct customerAccount _ ) ->
                    transactions
                        |> List.filter
                            (\transaction ->
                                let
                                    maybeTransactionCToken =
                                        Eth.Token.getCTokenByAddress cTokens (Ethereum.getContractAddressString transaction.contract)
                                in
                                transaction.customer
                                    == customerAccount
                                    && transaction.function
                                    == "liquidateBorrow"
                                    && transaction.network
                                    == network
                                    && maybeTransactionCToken
                                    /= Nothing
                            )

                _ ->
                    []
    in
    div [ class "panel" ]
        ([ div [ class "panel__header" ] [ span [] [ text "Recent Activity" ] ] ]
            ++ (if List.isEmpty filteredTransactions then
                    [ div [ class "content" ] [ text "No activity yet." ] ]

                else
                    [ div [ class "transactions" ] (List.map (liquidateTransactionEl userLanguage maybeNetwork timezone cTokens) filteredTransactions) ]
               )
        )


liquidateTransactionEl : Translations.Lang -> Maybe Network -> Time.Zone -> CTokenSet -> Transaction -> Html msg
liquidateTransactionEl userLanguage maybeNetwork timezone cTokens transaction =
    let
        timestamp =
            Eth.Transaction.transactionDateFormatter timezone transaction.timestamp

        description =
            Functions.default (Eth.TrxDescriptions.describeTransaction userLanguage cTokens transaction.contract transaction.function (transaction.args ++ [ "" ])) ""

        ( className, statusEl ) =
            case ( transaction.status, transaction.error ) of
                ( Eth.Transaction.Success, Nothing ) ->
                    ( "success"
                    , span [ class "status success" ] []
                    )

                ( Eth.Transaction.Pending, _ ) ->
                    ( "pending"
                    , span [ class "status accent-bg" ]
                        [ div [ class "lds-ring" ]
                            [ div [] []
                            , div [] []
                            , div [] []
                            , div [] []
                            ]
                        ]
                    )

                ( _, _ ) ->
                    ( "failure"
                    , span [ class "status fail" ] []
                    )

        errorEl =
            case transaction.error of
                Just error ->
                    div [ class "transaction-error" ]
                        [ text (Eth.TrxDescriptions.describeGracefulFailure error)
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


buildLiquidateDetailsFromPortResponse : Ethereum.CustomerAddress -> AssetAddress -> Decimal -> AssetAddress -> LiquidateBorrowDetails
buildLiquidateDetailsFromPortResponse customerAddress borrowToCloseAddress borrowedAmount desiredCollateralAddress =
    LiquidateBorrowDetails
        customerAddress
        (Just borrowToCloseAddress)
        (Normal ( Decimal.toString borrowedAmount, Just borrowedAmount ))
        (Just desiredCollateralAddress)



-- Actual Liquidate Collateral


port askLiquidatePort : { cTokenAddress : String, customerAddress : String, borrowerAddress : String, borrowedAssetAmountWeiStr : String, borrowedAssetDecimals : Int, desiredAssetAddress : String, desiredAssetDecimals : Int, isCEther : Bool } -> Cmd msg


askLiquidate : Ethereum.ContractAddress -> Ethereum.CustomerAddress -> Ethereum.CustomerAddress -> String -> Int -> Ethereum.AssetAddress -> Int -> Bool -> Cmd msg
askLiquidate (Ethereum.Contract cTokenAddress) (Ethereum.Customer liquidatorAddress) borrowerAddresses borrowAmountString borrowedAssetDecimals (Asset desiredAsset) desiredAssetDecimals isCEther =
    askLiquidatePort
        { cTokenAddress = cTokenAddress
        , customerAddress = liquidatorAddress
        , borrowerAddress = getCustomerAddressString borrowerAddresses
        , borrowedAssetAmountWeiStr = borrowAmountString
        , borrowedAssetDecimals = borrowedAssetDecimals
        , desiredAssetAddress = desiredAsset
        , desiredAssetDecimals = desiredAssetDecimals
        , isCEther = isCEther
        }


port giveLiquidatePort : (Json.Decode.Value -> msg) -> Sub msg


giveLiquidate : (Result Json.Decode.Error LiquidateBorrowDetails -> msg) -> Sub msg
giveLiquidate wrapper =
    let
        decoder =
            Json.Decode.map4 buildLiquidateDetailsFromPortResponse
                (Json.Decode.field "borrowerAddress" decodeCustomerAddress)
                (Json.Decode.field "borrowedAssetAddress" decodeAssetAddress)
                (Json.Decode.field "borrowedAmount" decimal)
                (Json.Decode.field "desiredCollateralAddress" decodeAssetAddress)
    in
    giveLiquidatePort
        (Json.Decode.decodeValue decoder >> wrapper)
