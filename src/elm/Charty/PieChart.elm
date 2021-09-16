module Charty.PieChart exposing
    ( Dataset
    , Group
    , Config
    , defaults
    , view
    )

{-| This module is in charge of drawing pie charts.


# Data representation

@docs Dataset
@docs Group


# Settings

@docs Config
@docs defaults


# Drawing

@docs view

-}

import Charty.Color as Color exposing (Color)
import Svg exposing (..)
import Svg.Attributes exposing (..)


{-| A dataset is just a list of groups.
-}
type alias Dataset =
    List Group


{-| A group that will be drawn in the chart. Note that depending on the
`maxGroupCount` setting, a group may end up being grouped with others in an
"Others" slice of the pie.
-}
type alias Group =
    { value : Float }


{-| Configuration for how the chart will be drawn. Note that
[`PieChart.defaults`](Charty-PieChart#defaults) can be used as a base
configuration.

If the color assignment is changed and a `maxGroupCount` is specified, keep in
mind that the dataset passed to the function may have some of your groups merged
into a single "Others" category.

-}
type alias Config =
    { background : Color
    , maxGroupCount : Maybe Int
    , colorAssignment : Dataset -> List ( Color, Group )
    }


type alias Slice =
    { percentage : Float
    , color : Color
    }


{-| Default configuration. At most 8 slices will be drawn and a default color
palette is used.
-}
defaults : Config
defaults =
    { background = "transparent"
    , maxGroupCount = Just 8
    , colorAssignment = Color.assignDefaults
    }


{-| This function generates svg markup for the chart, provided a the necessary
configuration and dataset. Example usage:

    sampleDataset : PieChart.Dataset
    sampleDataset =
        [ ( "Group A", 40.0 )
        , ( "Group B", 25.0 )
        , ( "Group C", 35.0 )
        ]

    view : Model -> Html Msg
    view model =
        Html.div
            []
            [ Html.p [] [ Html.text "Wow!" ]
            , PieChart.view PieChart.defaults dataset
            ]

-}
view : Config -> Dataset -> Svg msg
view config dataset =
    let
        slices =
            preprocess config dataset
    in
    drawSlices config slices


preprocess : Config -> Dataset -> List Slice
preprocess config dataset =
    dataset
        |> normalize
        |> truncate config.maxGroupCount
        |> config.colorAssignment
        |> List.map
            (\( color, { value } ) ->
                { color = color
                , percentage = value
                }
            )


normalize : Dataset -> Dataset
normalize dataset =
    let
        sum =
            sumValues dataset
    in
    List.map (\{ value } -> { value = 100 * value / sum }) dataset


truncate : Maybe Int -> Dataset -> Dataset
truncate maxGroupCount dataset =
    case maxGroupCount of
        Nothing ->
            dataset

        Just n ->
            case List.drop (n - 1) dataset of
                [] ->
                    dataset

                _ :: [] ->
                    dataset

                rest ->
                    List.take (n - 1) dataset
                        ++ [ { value = sumValues rest } ]


sumValues : Dataset -> Float
sumValues =
    List.foldr (\{ value } s -> value + s) 0


drawSlice : Config -> Float -> Slice -> Svg msg
drawSlice config start slice =
    let
        ( x1, y1 ) =
            circumferencePoint start

        ( x2, y2 ) =
            -- self closing arcs (100%) are not drawn by the browser.
            -- this is a hack, but seems to work visually and keeps drawing code consistent.
            circumferencePoint (start + Basics.min 99.9999 slice.percentage)

        largeArc =
            if slice.percentage <= 50 then
                "0"

            else
                "1"

        pathDefinition =
            (List.concat >> String.join " ")
                [ -- start at center
                  [ "M 70 70" ]

                -- straight line to point 1
                , [ "L ", String.fromFloat x1, String.fromFloat y1 ]

                -- arc definition
                , [ "A 70 70 0", largeArc, "1" ]

                -- arc to point 2.
                , [ String.fromFloat x2, String.fromFloat y2 ]

                -- return to center
                , [ "Z" ]
                ]
    in
    Svg.path
        [ d pathDefinition
        , stroke "transparent"
        , fill slice.color
        ]
        []


{-| Returns the coordinates of the point in the circumference that corresponds to the percentage.
-}
circumferencePoint : Float -> ( Float, Float )
circumferencePoint percentage =
    let
        ang =
            percentage * (2 * pi) / 100
    in
    ( 70 * (1 + sin ang), 70 * (1 - cos ang) )


drawSlices : Config -> List Slice -> Svg msg
drawSlices config slices =
    slices
        |> accumulateStart 0
        |> List.map (uncurry (drawSlice config))
        |> Svg.svg [ viewBox "0 0 140 140", width "100%" ]


accumulateStart : Float -> List Slice -> List ( Float, Slice )
accumulateStart start slices =
    case slices of
        [] ->
            []

        s :: ss ->
            ( start, s ) :: accumulateStart (start + s.percentage) ss


{-| Change how arguments are passed to a function.
This combines two arguments into a single pair.
-}
uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b
