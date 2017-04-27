module Goat.View.Utils exposing (..)

import Goat.Model exposing (Annotation(..), AnnotationState(..), ShapeType(..), Shape, ResizeDirection(..), StartPosition, EndPosition, StrokeStyle(..))
import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Json
import Rocket exposing ((=>))
import Goat.Utils exposing (arrowAngle)


onMouseDown : Json.Decoder msg -> Attribute msg
onMouseDown decodeToMsg =
    on "mousedown" decodeToMsg


onMouseUp : Json.Decoder msg -> Attribute msg
onMouseUp decodeToMsg =
    on "mouseup" decodeToMsg


defaultPrevented : Html.Events.Options
defaultPrevented =
    Html.Events.Options False True


stopPropagation : Html.Events.Options
stopPropagation =
    Html.Events.Options True False


toLineStyle : StrokeStyle -> ( String, String )
toLineStyle strokeStyle =
    case strokeStyle of
        SolidThin ->
            "2" => ""

        SolidMedium ->
            "4" => ""

        SolidThick ->
            "6" => ""

        SolidVeryThick ->
            "8" => ""

        DashedThin ->
            "2" => "10, 5"

        DashedMedium ->
            "4" => "10, 5"

        DashedThick ->
            "6" => "10, 5"

        DashedVeryThick ->
            "8" => "10, 5"


linePath : StartPosition -> EndPosition -> String
linePath start end =
    "M" ++ toString start.x ++ "," ++ toString start.y ++ " l" ++ toString (end.x - start.x) ++ "," ++ toString (end.y - start.y)


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


fontSizeToLineHeight : Int -> Float
fontSizeToLineHeight fontSize =
    toFloat fontSize * 1.2


spotlightToMaskCutout : ( Int, Annotation ) -> Maybe ( Int, ShapeType, Shape )
spotlightToMaskCutout ( index, annotation ) =
    case annotation of
        Spotlight shapeType shape ->
            Just ( index, shapeType, shape )

        _ ->
            Nothing


annotationStateToCursor : AnnotationState -> String
annotationStateToCursor annotationState =
    case annotationState of
        ReadyToDraw ->
            "crosshair"

        DrawingAnnotation _ _ ->
            "crosshair"

        MovingAnnotation _ _ _ _ ->
            "move"

        ResizingAnnotation _ _ ->
            "nesw-resize"

        EditingATextBox _ _ ->
            "default"

        _ ->
            "crosshair"


arrowHeadPath : EndPosition -> String
arrowHeadPath pos =
    "M"
        ++ toString (toFloat pos.x - 20.6667)
        ++ ","
        ++ toString (toFloat pos.y - 2.8)
        ++ "l-4.62033,-10.72559l 25.66667, 13.66667l -25.66667, 13.66667l4.62033, -10.33667z"


arrowPath : Shape -> String
arrowPath shape =
    let
        theta =
            (2 * pi)
                - (arrowAngle shape.start shape.end)

        perpen =
            (pi / 2) - theta

        comp =
            pi - theta

        start =
            { x = toFloat shape.start.x, y = toFloat shape.start.y }

        end =
            { x =
                if shape.end.x > shape.start.x && shape.end.y > shape.start.y then
                    (-20 * cos -theta) + toFloat shape.end.x
                else if shape.end.x < shape.start.x && shape.end.y < shape.start.y then
                    (20 * cos -comp) + toFloat shape.end.x
                else if shape.end.x < shape.start.x && shape.end.y > shape.start.y then
                    (20 * cos -comp) + toFloat shape.end.x
                else
                    (-20 * cos -theta) + toFloat shape.end.x
            , y =
                if shape.end.x > shape.start.x && shape.end.y > shape.start.y then
                    (-20 * sin -theta) + toFloat shape.end.y
                else if shape.end.x < shape.start.x && shape.end.y < shape.start.y then
                    (-20 * sin -comp) + toFloat shape.end.y
                else if shape.end.x < shape.start.x && shape.end.y > shape.start.y then
                    (-20 * sin -comp) + toFloat shape.end.y
                else
                    (20 * sin theta) + toFloat shape.end.y
            }

        dx =
            end.x - start.x

        dy =
            end.y - start.y

        halfWayPt =
            dx * 0.54286

        arcPt =
            dx * 0.85714
    in
        "M "
            ++ toString start.x
            ++ ","
            ++ toString start.y
            ++ "l"
            ++ toString (4 * cos perpen)
            ++ ","
            ++ toString (4 * sin perpen)
            ++ "c"
            ++ toString halfWayPt
            ++ ","
            ++ toString (dy * 0.54286)
            ++ " "
            ++ toString arcPt
            ++ ","
            ++ toString (dy * 0.85714)
            ++ " "
            ++ toString (dx + (2.5 * cos perpen))
            ++ ","
            ++ toString (dy + (2.5 * sin perpen))
            ++ "l "
            ++ toString (-13 * cos (-theta + (pi / 2)))
            ++ ","
            ++ toString (-13 * sin (-theta + (pi / 2)))
            ++ "c"
            ++ toString (arcPt - dx + (2.5 * cos perpen))
            ++ ","
            ++ toString (((dy * 0.85714) - dy) + (2.5 * sin perpen))
            ++ " "
            ++ toString (halfWayPt - dx + (2.5 * cos perpen))
            ++ ","
            ++ toString (((dy * 0.54286) - dy) + (2.5 * sin perpen))
            ++ " "
            ++ toString (-dx + (2.5 * cos perpen))
            ++ ","
            ++ toString (-dy + (2.5 * sin perpen))
            ++ "l"
            ++ toString (4 * cos perpen)
            ++ ","
            ++ toString (4 * sin perpen)
