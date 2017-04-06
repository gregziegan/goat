module Goat.Helpers exposing (..)

import Goat.Model exposing (..)
import Mouse exposing (Position)
import Rocket exposing ((=>))


minDrawingDistance : number
minDrawingDistance =
    4


isDrawingLargeEnough : StartPosition -> EndPosition -> Bool
isDrawingLargeEnough start end =
    abs (start.x - end.x) < minDrawingDistance || abs (start.y - end.y) < minDrawingDistance


toPx : number -> String
toPx number =
    toString number ++ "px"


calcShapePos : StartPosition -> EndPosition -> ShapeMode -> EndPosition
calcShapePos start end shapeMode =
    case shapeMode of
        DrawingShape ->
            end

        DrawingEqualizedShape ->
            equalXandY start end


calcLinePos : StartPosition -> EndPosition -> LineMode -> EndPosition
calcLinePos start end lineMode =
    case lineMode of
        DrawingLine ->
            end

        DrawingDiscreteLine ->
            stepMouse start end


equalXandY : StartPosition -> EndPosition -> EndPosition
equalXandY a b =
    if b.y - a.y < 0 then
        Position b.x (a.y - (Basics.max (b.x - a.x) (a.x - b.x)))
    else
        Position b.x (a.y + (Basics.max (b.x - a.x) (a.x - b.x)))


positionMapX : (Int -> Int) -> Position -> Position
positionMapX fn pos =
    { pos | x = fn pos.x }


positionMapY : (Int -> Int) -> Position -> Position
positionMapY fn pos =
    { pos | y = fn pos.y }


stepMouse : StartPosition -> EndPosition -> EndPosition
stepMouse start curPos =
    arrowAngle start curPos
        / (pi / 4)
        |> round
        |> toFloat
        |> (*) (pi / 4)
        |> toDeltas (calcDistance start curPos)
        |> positionMapX ((+) start.x)
        |> positionMapY ((+) start.y)


toLineStyle : StrokeStyle -> ( String, String )
toLineStyle strokeStyle =
    case strokeStyle of
        SolidThin ->
            "4" => ""

        SolidMedium ->
            "6" => ""

        SolidThick ->
            "8" => ""

        SolidVeryThick ->
            "10" => ""

        DashedThin ->
            "4" => "10, 5"

        DashedMedium ->
            "6" => "10, 5"

        DashedThick ->
            "8" => "10, 5"

        DashedVeryThick ->
            "10" => "10, 5"


toDrawingPosition : Mouse.Position -> Mouse.Position
toDrawingPosition mouse =
    { mouse | x = mouse.x - controlUIWidth, y = mouse.y - 10 }


annotationStateToCursor : AnnotationState -> String
annotationStateToCursor annotationState =
    case annotationState of
        ReadyToDraw ->
            "crosshair"

        DrawingAnnotation _ ->
            "crosshair"

        MovingAnnotation _ _ _ ->
            "move"

        ResizingAnnotation _ _ _ vertex ->
            "nesw-resize"

        EditingATextBox _ ->
            "default"

        _ ->
            "crosshair"


arrowAngle : StartPosition -> EndPosition -> Float
arrowAngle a b =
    let
        theta =
            atan2 (toFloat (b.y - a.y)) (toFloat (b.x - a.x))

        radians =
            if theta < 0.0 then
                (2 * pi) + theta
            else
                theta
    in
        radians


toDeltas : Float -> Float -> Position
toDeltas h theta =
    Position (round (cos theta * h)) (round (sin theta * h))


calcDistance : Position -> Position -> Float
calcDistance a b =
    sqrt <| toFloat <| (b.x - a.x) ^ 2 + (b.y - a.y) ^ 2
