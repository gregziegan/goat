module Goat.View.Utils exposing (..)

import Goat.Model exposing (EndPosition, ResizeDirection(..), StartPosition)


posToString : { x : number, y : number } -> String
posToString pos =
    toString pos.x ++ "," ++ toString pos.y


directionToCursor : ResizeDirection -> String
directionToCursor direction =
    case direction of
        NWSE ->
            "northWestCursor"

        NESW ->
            "northEastCursor"

        Move ->
            "moveCursor"


toPx : number -> String
toPx number =
    toString number ++ "px"
