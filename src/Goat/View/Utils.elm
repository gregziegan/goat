module Goat.View.Utils exposing (posToString, toPx)


posToString : { x : number, y : number } -> String
posToString pos =
    toString pos.x ++ "," ++ toString pos.y


toPx : number -> String
toPx number =
    toString number ++ "px"
