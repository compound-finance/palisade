module Utils.ArrayHelper exposing (..)

import Array exposing (Array)

{-| -}
updateArray : Int -> (a -> a) -> Array a -> Array a
updateArray index fn arr =
  case Array.get index arr of
    Just el ->
      Array.set index (fn el) arr

    Nothing ->
      arr
