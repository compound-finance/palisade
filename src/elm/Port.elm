port module Port exposing
    ( askNetwork
    , askNewBlock
    , askSetBlockNativeNetworkPort
    , encodeParameters
    , giveAccountBalance
    , giveEncodedExtrinsic
    , giveEncodedParameters
    , giveError
    , giveNetwork
    , giveNewBlock
    , imageError
    , setGasPrice
    , setTitle
    )

import CompoundComponents.Eth.Decoders
import CompoundComponents.Eth.Ethereum exposing (CustomerAddress(..), TrxHash)
import CompoundComponents.Eth.Network exposing (Network, networkFromId)
import Decimal exposing (Decimal)
import Json.Decode exposing (Value)



-- Handle JavaScript errors


port giveError : (String -> msg) -> Sub msg



-- Set Page Titles


port setTitle : String -> Cmd msg



-- Set Provider Type


port giveProviderType : (Int -> msg) -> Sub msg



-- Update BlockNative target network


port askSetBlockNativeNetworkPort : { networkId : Int } -> Cmd msg


port giveAccountBalancePort : (Value -> msg) -> Sub msg


giveAccountBalance : (Result Json.Decode.Error Decimal -> msg) -> Sub msg
giveAccountBalance wrapper =
    let
        decoder =
            Json.Decode.field "balance" CompoundComponents.Eth.Decoders.decimal
    in
    giveAccountBalancePort
        (Json.Decode.decodeValue decoder >> wrapper)



-- Asking for the network


port askNetworkPort : {} -> Cmd msg


askNetwork : Cmd msg
askNetwork =
    askNetworkPort {}


port giveNetworkPort : (Value -> msg) -> Sub msg


giveNetwork : (Result Json.Decode.Error (Maybe Network) -> msg) -> Sub msg
giveNetwork wrapper =
    let
        decoder =
            Json.Decode.field "network" (Json.Decode.maybe (Json.Decode.map networkFromId Json.Decode.int))
    in
    giveNetworkPort
        (Json.Decode.decodeValue decoder >> wrapper)



-- Asking for the network


port askNewBlockPort : {} -> Cmd msg


askNewBlock : Cmd msg
askNewBlock =
    askNewBlockPort {}


port giveNewBlockPort : (Value -> msg) -> Sub msg


giveNewBlock : (Result Json.Decode.Error Int -> msg) -> Sub msg
giveNewBlock wrapper =
    let
        decoder =
            Json.Decode.field "block" Json.Decode.int
    in
    giveNewBlockPort
        (Json.Decode.decodeValue decoder >> wrapper)



-- Set current gas price


port setGasPricePort : { amountWeiStr : String } -> Cmd msg


setGasPrice : Decimal -> Cmd msg
setGasPrice gasPriceWei =
    setGasPricePort
        { amountWeiStr = Decimal.toString gasPriceWei
        }



-- Encode Parameters


port adminDashboardEncodeParametersPort : { argTypes : List String, args : List Value } -> Cmd msg


encodeParameters : List String -> List Value -> Cmd msg
encodeParameters fnArgTypes fnArgs =
    adminDashboardEncodeParametersPort
        { argTypes = fnArgTypes
        , args = fnArgs
        }


port giveEncodedParametersPort : (Json.Decode.Value -> msg) -> Sub msg


giveEncodedParameters : (Result Json.Decode.Error String -> msg) -> Sub msg
giveEncodedParameters wrapper =
    giveEncodedParametersPort
        (Json.Decode.decodeValue Json.Decode.string >> wrapper)



-- Give Pallets and Extrinsics


port giveEncodedExtrinsicPort : (Json.Decode.Value -> msg) -> Sub msg


giveEncodedExtrinsic : (Result Json.Decode.Error (Maybe String) -> msg) -> Sub msg
giveEncodedExtrinsic wrapper =
    giveEncodedExtrinsicPort
        (Json.Decode.decodeValue (Json.Decode.maybe Json.Decode.string) >> wrapper)


port handleImageError : { elementId : String, imgSrc : String } -> Cmd msg


imageError : String -> String -> Cmd msg
imageError elementId imgSrc =
    let
        data =
            { elementId = elementId
            , imgSrc = imgSrc
            }
    in
    handleImageError data
