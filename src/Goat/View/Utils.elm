module Goat.View.Utils exposing (toPx)


toPx : Int -> String
toPx number =
    String.fromInt number ++ "px"
