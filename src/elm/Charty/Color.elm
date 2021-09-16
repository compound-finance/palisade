module Charty.Color exposing
    ( Color
    , defaultPalette
    , assignDefaults
    )

{-| Utilities for assigning colors to series/data groups.
You probably won't need to use it unless yo want to modify or use the default color paletter for other purposes.

@docs Color
@docs defaultPalette
@docs assignDefaults

-}

import Array exposing (Array)


{-| The color used to draw a line. For the moment, any string used to specify
SVG colors is valid, so things such as "red" and "#FF0000" should work.
-}
type alias Color =
    String


{-| Default color palette.
-}
defaultPalette : Array Color
defaultPalette =
    Array.fromList
        [ "#9669ED" -- borrow
        , "#00D395" -- supply
        , "#657786" -- color_grey_1
        ]


{-| Assigns a color of the default palette to each element of the least.
If the list has too many items, color will be repeated.
-}
assignDefaults : List a -> List ( Color, a )
assignDefaults dataset =
    let
        colorCount =
            Array.length defaultPalette

        color index =
            case Array.get (modBy colorCount index) defaultPalette of
                Nothing ->
                    "#00D395"

                Just x ->
                    x
    in
    List.indexedMap (\i series -> ( color i, series )) dataset
