module DappInterface.MainModel exposing
    ( BorrowingContainerState
    , BorrowingRisk(..)
    , ChooseWalletModalState(..)
    , CollateralModalActionState(..)
    , CollateralToggleModalState
    , InputActionPaneState(..)
    , Model
    , MouseEvent
    , PrimaryActionModalInput(..)
    , PrimaryActionModalState
    , PrimaryActionType(..)
    , ScreenPosition
    , getBorrowingRisk
    , getConfig
    , getCurrentConfig
    , getProvider
    , mouseEventDecoder
    )

import Admin
import CompoundComponents.DisplayCurrency exposing (DisplayCurrency(..))
import CompoundComponents.Eth.ConnectedEthWallet as ConnectedEthWallet
import CompoundComponents.Eth.Ethereum exposing (Account(..), AssetAddress(..), ContractAddress(..), CustomerAddress(..))
import CompoundComponents.Eth.Network exposing (Network(..), networkName)
import CompoundComponents.Ether.BNTransaction exposing (BNTransactionState)
import DOM exposing (Rectangle)
import DappInterface.ClaimCompModal as ClaimCompModal
import DappInterface.CommonViews as CommonViews
import DappInterface.Page exposing (Page(..))
import DappInterface.Propose as Propose
import DappInterface.Vote as Vote
import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Eth.Compound exposing (CompoundMsg(..), CompoundState)
import Eth.Config exposing (Config)
import Eth.Governance
import Eth.Oracle exposing (OracleMsg(..), OracleState)
import Eth.Token exposing (CToken, TokenMsg(..), TokenState)
import Eth.Transaction exposing (TransactionState)
import Json.Decode
import Json.Encode
import Liquidate
import Preferences exposing (Preferences, PreferencesMsg(..))
import Repl
import Strings.Translations as Translations
import Time
import Utils.BrowserInfo


type alias ScreenPosition =
    { x : Int
    , y : Int
    }


type alias MouseEvent =
    { clientPos : ScreenPosition
    , targetPos : ScreenPosition
    }


type alias BorrowingContainerState =
    { maybeBorrowingLimitPopoverPosition : Maybe ScreenPosition
    , showNetAPYHoverView : Bool
    , maybePreviousAnimatedBalances : Maybe ( Decimal, Decimal )
    , maybeCurrentAnimatedBalances : Maybe ( Decimal, Decimal )
    , maybeLastBalancesChangeTimestamp : Maybe Time.Posix
    , shouldRemoveSlotMachineActiveClass : Bool
    }


type BorrowingRisk
    = NoRisk
    | LowRisk
    | MediumRisk
    | HighRisk


type ChooseWalletModalState
    = NotVisible
    | ChooseProvider
    | LoadingLegerAccounts
    | LedgerConnectionError
    | ChooseLedgerAccount
    | Connecting


type CollateralModalActionState
    = InitialState
    | AwaitingEnterExitConfirmTransaction
    | AwaitingEnterExitTransactionMined


type alias CollateralToggleModalState =
    { chosenAsset : CToken
    , entering : Bool
    , actionState : CollateralModalActionState
    }


type PrimaryActionType
    = MintAction
    | RedeemAction
    | BorrowAction
    | RepayBorrowAction


type InputActionPaneState
    = ChoosingInputs
    | EnableFailed String
    | AwaitingConfirmTransaction
    | AwaitingTransactionMined String


type PrimaryActionModalInput
    = Normal ( String, Maybe Decimal )
    | Empty
    | Max


type alias PrimaryActionModalState =
    { chosenAsset : CToken
    , primaryActionType : PrimaryActionType
    , inputActionPaneState : InputActionPaneState
    , inputValue : PrimaryActionModalInput
    , errors : List String
    }


type alias Model =
    { page : Page
    , adminModel : Admin.Model
    , liquidateModel : Liquidate.Model
    , apiBaseUrlMap : Dict String String
    , appVersion : Maybe Float
    , configs : Dict String Config
    , dataProviders : Dict String String
    , configAbis : Json.Encode.Value
    , account : Account
    , commonViewsModel : CommonViews.Model
    , connectedEthWalletModel : ConnectedEthWallet.Model
    , claimCompModalState : ClaimCompModal.Model
    , borrowingContainerState : BorrowingContainerState
    , collateralToggleModalState : Maybe CollateralToggleModalState
    , primaryActionModalState : Maybe PrimaryActionModalState
    , network : Maybe Network
    , tokenState : TokenState
    , transactionState : TransactionState
    , bnTransactionState : BNTransactionState
    , compoundState : CompoundState
    , oracleState : OracleState
    , blockNumber : Maybe Int
    , preferences : Preferences
    , governanceState : Eth.Governance.GovernanceState
    , errors : List String
    , currentTime : Maybe Time.Posix
    , currentTimeZone : Time.Zone
    , browserType : Utils.BrowserInfo.BrowserType
    , proposeModel : Propose.Model
    , voteModel : Vote.Model
    , maybeGasPrice : Maybe Decimal
    , userLanguage : Translations.Lang
    , repl : Repl.Model
    }


getBorrowingRisk : Decimal -> BorrowingRisk
getBorrowingRisk borrowPercentOfLimit =
    if Decimal.gt borrowPercentOfLimit (Decimal.fromInt 80) then
        HighRisk

    else if Decimal.gt borrowPercentOfLimit (Decimal.fromInt 60) then
        MediumRisk

    else if Decimal.gt borrowPercentOfLimit Decimal.zero then
        LowRisk

    else
        NoRisk


getCurrentConfig : Model -> Maybe Config
getCurrentConfig model =
    Maybe.andThen (getConfig model.configs) model.network


getConfig : Dict String Config -> Network -> Maybe Config
getConfig configs network =
    Dict.get (String.toLower (networkName network)) configs


getProvider : Dict String String -> Network -> Maybe String
getProvider dataProviders network =
    Dict.get (String.toLower (networkName network)) dataProviders


mouseEvent : Int -> Int -> Rectangle -> MouseEvent
mouseEvent clientX clientY target =
    { clientPos = ScreenPosition clientX clientY
    , targetPos = ScreenPosition (truncate target.left) (truncate target.top)
    }


mouseEventDecoder : Json.Decode.Decoder MouseEvent
mouseEventDecoder =
    Json.Decode.map3
        mouseEvent
        (Json.Decode.field "clientX" Json.Decode.int)
        (Json.Decode.field "clientY" Json.Decode.int)
        (Json.Decode.field "target" DOM.boundingClientRect)
