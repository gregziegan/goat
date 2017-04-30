module Goat.Utils exposing (isDrawingTooSmall, isSpotlightDrawing, calcShapePos, calcLinePos, equalXandY, positionMap, positionMapX, positionMapY, mapAtIndex, removeItemIf, removeItem, isEmptyTextBox, drawingsAreEqual, toDrawingPosition, toPosition, getFirstSpotlightIndex, shiftPosition, getPositions, stepMouse, arrowAngle, getAnnotationAttributes, currentAnnotationAttributes, annotationStateAttributes)

import Array.Hamt as Array exposing (Array)
import Goat.ControlOptions as ControlOptions
import Goat.Model exposing (Annotation(..), AnnotationAttributes, AnnotationState(..), Drawing(..), EndPosition, LineType(..), Model, ResizeDirection(..), Shape, ShapeType(..), StartPosition, StrokeStyle(..))
import List.Extra
import Mouse exposing (Position)
import Rocket exposing ((=>))
import SingleTouch as ST


minDrawingDistance : number
minDrawingDistance =
    4


minSpotlightDrawingDistance : number
minSpotlightDrawingDistance =
    8


isDrawingTooSmall : Bool -> StartPosition -> EndPosition -> Bool
isDrawingTooSmall isSpotlight start end =
    if isSpotlight then
        abs (start.x - end.x) < minSpotlightDrawingDistance && abs (start.y - end.y) < minSpotlightDrawingDistance
    else
        abs (start.x - end.x) < minDrawingDistance && abs (start.y - end.y) < minDrawingDistance


isSpotlightDrawing : Drawing -> Bool
isSpotlightDrawing drawing =
    case drawing of
        DrawSpotlight _ ->
            True

        _ ->
            False


calcShapePos : Bool -> StartPosition -> EndPosition -> EndPosition
calcShapePos shiftPressed start end =
    if shiftPressed then
        equalXandY start end
    else
        end


calcLinePos : Bool -> StartPosition -> EndPosition -> EndPosition
calcLinePos shiftPressed start end =
    if shiftPressed then
        stepMouse start end
    else
        end


equalXandY : StartPosition -> EndPosition -> EndPosition
equalXandY a b =
    if b.y - a.y < 0 then
        Position b.x (a.y - Basics.max (b.x - a.x) (a.x - b.x))
    else
        Position b.x (a.y + Basics.max (b.x - a.x) (a.x - b.x))


positionMapX : (Int -> Int) -> Position -> Position
positionMapX fn pos =
    { pos | x = fn pos.x }


positionMapY : (Int -> Int) -> Position -> Position
positionMapY fn pos =
    { pos | y = fn pos.y }


positionMap : (Int -> Int) -> Position -> Position
positionMap fn { x, y } =
    Position (fn x) (fn y)


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


toDrawingPosition : Mouse.Position -> Mouse.Position
toDrawingPosition mouse =
    { mouse | x = mouse.x - ControlOptions.controlUIWidth, y = mouse.y - 10 }


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


toPosition : ST.SingleTouch -> Position
toPosition st =
    Position (round st.touch.clientX) (round st.touch.clientY)


mapAtIndex : Int -> (a -> a) -> Array a -> Array a
mapAtIndex index fn xs =
    case Array.get index xs of
        Just x ->
            Array.set index (fn x) xs

        Nothing ->
            xs


drawingsAreEqual : Drawing -> Drawing -> Bool
drawingsAreEqual drawing drawing2 =
    case drawing of
        DrawLine lineType ->
            case drawing2 of
                DrawLine lineType2 ->
                    lineType == lineType2

                _ ->
                    False

        DrawFreeHand ->
            drawing2 == DrawFreeHand

        DrawShape shapeType ->
            case drawing2 of
                DrawShape shapeType2 ->
                    shapeType == shapeType2

                _ ->
                    False

        DrawTextBox ->
            case drawing2 of
                DrawTextBox ->
                    True

                _ ->
                    False

        DrawSpotlight shapeType ->
            case drawing2 of
                DrawSpotlight shapeType2 ->
                    shapeType == shapeType2

                _ ->
                    False

        DrawPixelate ->
            drawing2 == DrawPixelate


isSpotlightShape : Annotation -> Bool
isSpotlightShape annotation =
    case annotation of
        Spotlight _ _ ->
            True

        _ ->
            False


removeItem : Int -> Array a -> Array a
removeItem index arr =
    Array.append (Array.slice 0 index arr) (Array.slice (index + 1) (Array.length arr) arr)


removeItemIf : (a -> Bool) -> Int -> Array a -> Array a
removeItemIf fn index xs =
    case Array.get index xs of
        Just x ->
            if fn x then
                removeItem index xs
            else
                xs

        Nothing ->
            xs


isEmptyTextBox : Annotation -> Bool
isEmptyTextBox annotation =
    case annotation of
        TextBox textArea ->
            textArea.text == ""

        _ ->
            False


getFirstSpotlightIndex : Array Annotation -> Int
getFirstSpotlightIndex annotations =
    annotations
        |> Array.toList
        |> List.Extra.findIndex isSpotlightShape
        |> Maybe.withDefault 0


shiftPosition : Int -> Int -> Mouse.Position -> Mouse.Position
shiftPosition dx dy pos =
    { pos | x = pos.x + dx, y = pos.y + dy }


getPositions : Annotation -> ( StartPosition, EndPosition )
getPositions annotation =
    case annotation of
        Lines lineType line ->
            line.start => line.end

        FreeDraw shape _ ->
            shape.start => shape.end

        Shapes shapeType _ shape ->
            shape.start => shape.end

        TextBox textArea ->
            textArea.start => textArea.end

        Spotlight shapeType shape ->
            shape.start => shape.end

        Pixelate start end ->
            start => end


getAnnotationAttributes : Annotation -> AnnotationAttributes -> AnnotationAttributes
getAnnotationAttributes annotation existingAttrs =
    case annotation of
        Lines _ shape ->
            AnnotationAttributes shape.strokeColor existingAttrs.fill shape.strokeStyle existingAttrs.fontSize

        FreeDraw shape _ ->
            AnnotationAttributes shape.strokeColor existingAttrs.fill shape.strokeStyle existingAttrs.fontSize

        Shapes _ fill shape ->
            AnnotationAttributes shape.strokeColor fill shape.strokeStyle existingAttrs.fontSize

        TextBox textArea ->
            AnnotationAttributes textArea.fill existingAttrs.fill existingAttrs.strokeStyle textArea.fontSize

        Spotlight _ shape ->
            AnnotationAttributes shape.strokeColor existingAttrs.fill shape.strokeStyle existingAttrs.fontSize

        Pixelate _ _ ->
            existingAttrs


currentAnnotationAttributes : Model -> AnnotationAttributes
currentAnnotationAttributes { strokeColor, fill, strokeStyle, fontSize } =
    AnnotationAttributes strokeColor fill strokeStyle fontSize


annotationStateAttributes : Model -> AnnotationAttributes
annotationStateAttributes model =
    case model.annotationState of
        SelectedAnnotation _ annotationAttrs ->
            annotationAttrs

        MovingAnnotation _ _ _ annotationAttrs ->
            annotationAttrs

        ResizingAnnotation _ annotationAttrs ->
            annotationAttrs

        EditingATextBox _ annotationAttrs ->
            annotationAttrs

        _ ->
            currentAnnotationAttributes model
