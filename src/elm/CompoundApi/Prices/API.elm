module CompoundApi.Prices.API exposing (Response, Signature, decoder, fetch)

import BigInt exposing (BigInt)
import CompoundComponents.Ether.Decoder as Decoder exposing (Decoder)
import CompoundComponents.Ether.Helpers exposing (collapseResult, combineResults)
import CompoundComponents.Ether.Hex as Hex exposing (Hex)
import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Http
import Json.Decode



-- TODO: Timestamp
-- TODO: Allow other endpoints


endpoint : String
endpoint =
    "https://prices.compound.finance"


{-| TODO: Add timestamp
-}
type alias Signature =
    { message : Hex
    , signature : Hex
    , symbol : String
    , price : Decimal
    }


type alias Response =
    Dict String (List Signature)


type alias SimpleResponse =
    { timestamp : Maybe Int
    , messages : List Hex
    , signatures : List Hex
    , prices : Dict String Decimal
    }


type alias Message =
    { fieldType : String
    , timestamp : BigInt
    , symbol : String
    , price : BigInt
    }


fetch : Http.Request Response
fetch =
    Http.get
        endpoint
        decoder


decoder : Json.Decode.Decoder Response
decoder =
    Json.Decode.dict
        (Json.Decode.map4 SimpleResponse
            (Json.Decode.field "timestamp" (Json.Decode.string |> Json.Decode.map String.toInt))
            (Json.Decode.field "messages" (Json.Decode.list hex))
            (Json.Decode.field "signatures" (Json.Decode.list hex))
            (Json.Decode.field "prices" (Json.Decode.dict decimal))
        )
        |> Json.Decode.map Dict.toList
        |> Json.Decode.map
            (List.map
                (\( reporter, { timestamp, messages, signatures, prices } ) ->
                    if List.length messages /= List.length signatures then
                        Err "message / signature length mismatch"

                    else
                        List.map2 Tuple.pair messages signatures
                            |> List.map
                                (\( message, signature ) ->
                                    message
                                        |> Decoder.decodeListHex
                                            (Decoder.map4 Message
                                                Decoder.string
                                                (Decoder.uintSized 64)
                                                Decoder.string
                                                (Decoder.uintSized 64)
                                            )
                                        |> Result.map
                                            (\{ symbol, price } ->
                                                Signature
                                                    message
                                                    signature
                                                    symbol
                                                    (Decimal.fromBigIntWithExponent -6 price)
                                            )
                                )
                            |> combineResults
                            |> Result.map (\r -> ( reporter, r ))
                )
            )
        |> Json.Decode.map combineResults
        |> Json.Decode.map (Result.map Dict.fromList)
        |> Json.Decode.andThen (collapseResult Json.Decode.succeed Json.Decode.fail)


hex : Json.Decode.Decoder Hex
hex =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case Hex.parseHex str of
                    Ok hex_ ->
                        Json.Decode.succeed hex_

                    Err err ->
                        Json.Decode.fail err
            )


decimal : Json.Decode.Decoder Decimal
decimal =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case Decimal.fromString str of
                    Just dec ->
                        Json.Decode.succeed dec

                    Nothing ->
                        Json.Decode.fail ("unable to decode decimal: " ++ str)
            )
