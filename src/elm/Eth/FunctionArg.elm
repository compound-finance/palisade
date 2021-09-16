module Eth.FunctionArg exposing (FunctionArg, buildFunctionArg, setArg)

import Array exposing (Array)
import CompoundComponents.Eth.Ethereum exposing (Account(..), AssetAddress(..), ContractAddress(..), CustomerAddress(..), getContractAddressString, isValidAddress)
import Decimal
import Json.Decode exposing (Value)
import Json.Encode
import Regex exposing (contains)
import Utils.ABIHelper exposing (ABIInputOutputType)


type alias FunctionArg =
    { rawType : String
    , ethType : EthType
    , text : String
    , valid : Bool
    , encoded : Value
    }


type EthType
    = Array EthType
    | Uint Int
    | Address
    | Bytes
    | Unknown


buildFunctionArg : ABIInputOutputType -> FunctionArg
buildFunctionArg abiValue =
    let
        text =
            ""

        ethType =
            getEthType abiValue.type_

        valid =
            validateArg ethType text
    in
    { rawType = abiValue.type_
    , ethType = ethType
    , text = text
    , valid = valid
    , encoded =
        if valid then
            encodeArg ethType text

        else
            Json.Encode.null
    }


isValidBytes : Regex.Regex
isValidBytes =
    Maybe.withDefault Regex.never (Regex.fromString "^(0x)")



-- TODO: Fill in other types


getEthType : String -> EthType
getEthType valueType =
    if String.endsWith "[]" valueType then
        Array (getEthType (String.dropRight 2 valueType))

    else
        case valueType of
            "uint256" ->
                Uint 256

            "address" ->
                Address

            "bytes" ->
                Bytes

            _ ->
                Unknown


splitArray : String -> List String
splitArray str =
    case str of
        "" ->
            []

        _ ->
            String.split "," str


validateArg : EthType -> String -> Bool
validateArg ethType value =
    case ethType of
        Array t ->
            splitArray value
                |> List.map (validateArg t)
                |> List.foldl (&&) True

        Uint sz ->
            case Decimal.fromString value of
                Just decimal ->
                    Decimal.eq decimal (Decimal.truncate 0 decimal)

                Nothing ->
                    False

        Address ->
            isValidAddress value

        Bytes ->
            contains isValidBytes value

        Unknown ->
            -- Currently, no validation
            True


encodeArg : EthType -> String -> Value
encodeArg ethType value =
    case ethType of
        Array t ->
            splitArray value
                |> Json.Encode.list Json.Encode.string

        Uint sz ->
            Maybe.withDefault Decimal.zero (Decimal.fromString value)
                |> Decimal.truncate 0
                |> Decimal.toString
                |> Json.Encode.string

        Address ->
            Json.Encode.string value

        Bytes ->
            Json.Encode.string value

        Unknown ->
            Json.Encode.string value


setArg : String -> FunctionArg -> FunctionArg
setArg value functionArg =
    let
        argValid =
            validateArg functionArg.ethType value

        argEncoded =
            encodeArg functionArg.ethType value
    in
    { functionArg | valid = argValid, encoded = argEncoded, text = value }
