module Goat.View.Utils exposing (..)

import Goat.AnnotationAttributes exposing (StrokeStyle, arrowAngle)
import Goat.Model exposing (EndPosition, ResizeDirection(..), StartPosition)
import Goat.Utils exposing (shiftPosition)
import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Json
import Mouse exposing (Position)


onMouseDown : Json.Decoder msg -> Attribute msg
onMouseDown decodeToMsg =
    on "mousedown" decodeToMsg


onMouseUp : Json.Decoder msg -> Attribute msg
onMouseUp decodeToMsg =
    on "mouseup" decodeToMsg


posToString : { x : number, y : number } -> String
posToString pos =
    toString pos.x ++ "," ++ toString pos.y


linePath : Float -> StartPosition -> EndPosition -> String
linePath strokeWidth start end =
    let
        theta =
            (2 * pi)
                - (arrowAngle start end)

        perpen =
            (pi / 2) - theta

        dx =
            toFloat (end.x - start.x)

        dy =
            toFloat (end.y - start.y)

        startFloat =
            { x = toFloat start.x, y = toFloat start.y }

        ccPt1 =
            shiftPosition ((strokeWidth / 2) * cos perpen) ((strokeWidth / 2) * sin perpen) startFloat

        ccPt2 =
            shiftPosition dx dy ccPt1

        ccPt3 =
            shiftPosition (-strokeWidth * cos perpen) (-strokeWidth * sin perpen) ccPt2

        ccPt4 =
            shiftPosition -dx -dy ccPt3
    in
        "M"
            ++ posToString start
            ++ " L"
            ++ posToString ccPt1
            ++ " L"
            ++ posToString ccPt2
            ++ " L"
            ++ posToString ccPt3
            ++ " L"
            ++ posToString ccPt4
            ++ "Z"


freeDrawPath : StartPosition -> List Position -> String
freeDrawPath start positions =
    freeDrawPathHelper positions ("M " ++ posToString start)


freeDrawPathHelper : List Position -> String -> String
freeDrawPathHelper positions pathString =
    case positions of
        [] ->
            pathString

        pos :: rest ->
            freeDrawPathHelper rest (pathString ++ " L " ++ posToString pos)


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


arrowHeadPath : EndPosition -> String
arrowHeadPath pos =
    "M"
        ++ toString (toFloat pos.x - 20.6667)
        ++ ","
        ++ toString (toFloat pos.y - 2.8)
        ++ "l-4.62033,-10.72559l 25.66667, 13.66667l -25.66667, 13.66667l4.62033, -10.33667z"
