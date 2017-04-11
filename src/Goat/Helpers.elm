module Goat.Helpers exposing (..)

import Array.Hamt as Array exposing (Array)
import Goat.Model exposing (Annotation(..), AnnotationState(..), Drawing(..), EndPosition, Image, LineMode(..), LineType(..), ShapeMode(..), Shape, ShapeType(..), StartPosition, StrokeStyle(..), controlUIWidth)
import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Json
import Keyboard.Extra as Keyboard exposing (Key(Shift), isPressed)
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
        DrawSpotlight _ _ ->
            True

        _ ->
            False


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

        DrawingAnnotation _ _ ->
            "crosshair"

        MovingAnnotation _ _ _ ->
            "move"

        ResizingAnnotation _ ->
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


theGoats : List Image
theGoats =
    [ Image "images/goat.jpg" 235 276 639 751
    , Image "images/goat2.jpg" 235 276 639 751
    ]


toPosition : ST.SingleTouch -> Position
toPosition st =
    Position (round st.touch.clientX) (round st.touch.clientY)


onMouseDown : Json.Decoder msg -> Attribute msg
onMouseDown decodeToMsg =
    on "mousedown" decodeToMsg


onMouseUp : Json.Decoder msg -> Attribute msg
onMouseUp decodeToMsg =
    on "mouseup" decodeToMsg


onMouseUpOrLeave : Json.Decoder msg -> List (Attribute msg)
onMouseUpOrLeave decodeToMsg =
    [ on "mouseleave" decodeToMsg, onMouseUp decodeToMsg ]


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
        DrawLine lineType _ ->
            case drawing2 of
                DrawLine lineType2 _ ->
                    lineType == lineType2

                _ ->
                    False

        DrawShape shapeType _ ->
            case drawing2 of
                DrawShape shapeType2 _ ->
                    shapeType == shapeType2

                _ ->
                    False

        DrawTextBox ->
            case drawing2 of
                DrawTextBox ->
                    True

                _ ->
                    False

        DrawSpotlight shapeType shapeMode ->
            case drawing2 of
                DrawSpotlight shapeType2 _ ->
                    shapeType == shapeType2

                _ ->
                    False


isSpotlightShape : Annotation -> Bool
isSpotlightShape annotation =
    case annotation of
        Spotlight _ _ ->
            True

        _ ->
            False


fontSizeToLineHeight : Int -> Float
fontSizeToLineHeight fontSize =
    toFloat fontSize * 1.2


svgTextHeight : Int -> String -> Int
svgTextHeight fontSize text =
    text
        |> String.split "\n"
        |> List.length
        |> (*) fontSize


linePath : StartPosition -> EndPosition -> String
linePath start end =
    "M" ++ toString start.x ++ "," ++ toString start.y ++ " l" ++ toString (end.x - start.x) ++ "," ++ toString (end.y - start.y)


spotlightToMaskCutout : ( Int, Annotation ) -> Maybe ( Int, ShapeType, Shape )
spotlightToMaskCutout ( index, annotation ) =
    case annotation of
        Spotlight shapeType shape ->
            Just ( index, shapeType, shape )

        _ ->
            Nothing


selectLine : LineType -> Keyboard.State -> Drawing
selectLine lineType keyboardState =
    DrawLine lineType <|
        if isPressed Shift keyboardState then
            DrawingDiscreteLine
        else
            DrawingLine


selectShape : ShapeType -> Keyboard.State -> Drawing
selectShape shapeType keyboardState =
    DrawShape shapeType <|
        if isPressed Shift keyboardState then
            DrawingEqualizedShape
        else
            DrawingShape


selectSpotlight : ShapeType -> Keyboard.State -> Drawing
selectSpotlight shapeType keyboardState =
    DrawSpotlight shapeType <|
        if isPressed Shift keyboardState then
            DrawingEqualizedShape
        else
            DrawingShape


defaultPrevented : Html.Events.Options
defaultPrevented =
    Html.Events.Options False True


stopPropagation : Html.Events.Options
stopPropagation =
    Html.Events.Options True False


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
