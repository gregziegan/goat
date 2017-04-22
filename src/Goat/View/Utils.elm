module Goat.View.Utils exposing (..)

import Goat.Model exposing (Annotation(..), AnnotationState(..), ShapeType(..), Shape, ResizeDirection(..), StartPosition, EndPosition, StrokeStyle(..))
import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Json
import Rocket exposing ((=>))


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
