module Goat.Annotation exposing (Annotation(..), Drawing(..), SelectState(..), LineType(..), Shape, ShapeType(..), TextArea, defaultStroke, defaultDrawing, defaultShape, defaultSpotlight, strokeStyles, isFreeHand, isSpotlightShape, isEmptyTextBox, updateTextArea, attributes, arrowAngle, updateFill, updateStrokeColor, updateStrokeStyle, updateFontSize, positions, toLineStyle, toStrokeWidth, shiftPosition, move, shiftForPaste, setFill, setStrokeColor, setStrokeStyle, setFontSize, shapes, spotlights, StartPosition, EndPosition, newTextBox, fromDrawing, resize, autoExpandConfig, calcLinePos, calcShapePos, fontSizeToLineHeight, stepMouse)

import AutoExpand
import Goat.Annotation.Shared exposing (AnnotationAttributes, DrawingInfo, ResizingInfo, StrokeStyle(..), Vertex(..))
import Color exposing (Color)
import Mouse exposing (Position)
import Rocket exposing ((=>))


type alias StartPosition =
    Position


type alias EndPosition =
    Position


type LineType
    = Arrow
    | StraightLine


type ShapeType
    = Rect
    | RoundedRect
    | Ellipse


type alias Shape =
    { start : Position
    , end : Position
    , strokeColor : Color
    , strokeStyle : StrokeStyle
    }


type alias TextArea =
    { start : Position
    , end : Position
    , fill : Color
    , fontSize : Int
    , text : String
    , angle : Float
    , autoexpand : AutoExpand.State
    }


type Annotation
    = Lines LineType Shape
    | FreeDraw Shape (List Position)
    | Shapes ShapeType (Maybe Color) Shape
    | TextBox TextArea
    | Spotlight ShapeType Shape
    | Pixelate Position Position


{-| Annotations are viewed differently based on the kind of selection.

1.  Selected corresponds to annotations that are not in a state for resizing/moving.
    This is currently only relevant to Textboxes when they are being edited.
2.  SelectedWithVertices shows vertices on any annotation that allows for resizing/moving
3.  NotSelected shows the unadorned annotation

-}
type SelectState
    = Selected
    | SelectedWithVertices
    | NotSelected


type Drawing
    = DrawLine LineType
    | DrawFreeHand
    | DrawShape ShapeType
    | DrawTextBox
    | DrawSpotlight ShapeType
    | DrawPixelate


shapes : List Drawing
shapes =
    [ DrawShape RoundedRect
    , DrawShape Rect
    , DrawShape Ellipse
    , DrawLine StraightLine
    ]


spotlights : List Drawing
spotlights =
    [ DrawSpotlight RoundedRect
    , DrawSpotlight Rect
    , DrawSpotlight Ellipse
    ]


defaultStroke : StrokeStyle
defaultStroke =
    SolidMedium


strokeStyles : List StrokeStyle
strokeStyles =
    [ SolidThin
    , SolidMedium
    , SolidThick
    , SolidVeryThick
    , DashedThin
    , DashedMedium
    , DashedThick
    , DashedVeryThick
    ]


defaultDrawing : Drawing
defaultDrawing =
    DrawLine Arrow


defaultShape : Drawing
defaultShape =
    DrawShape RoundedRect


defaultSpotlight : Drawing
defaultSpotlight =
    DrawSpotlight RoundedRect


isFreeHand : Annotation -> Bool
isFreeHand annotation =
    case annotation of
        FreeDraw _ _ ->
            True

        _ ->
            False


isSpotlightShape : Annotation -> Bool
isSpotlightShape annotation =
    case annotation of
        Spotlight _ _ ->
            True

        _ ->
            False


isEmptyTextBox : Annotation -> Bool
isEmptyTextBox annotation =
    case annotation of
        TextBox textArea ->
            textArea.text == ""

        _ ->
            False


positions : Annotation -> ( Position, Position )
positions annotation =
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


attributes : Annotation -> AnnotationAttributes -> AnnotationAttributes
attributes annotation existingAttrs =
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


updateTextArea : AutoExpand.State -> String -> Annotation -> Annotation
updateTextArea state textValue annotation =
    case annotation of
        TextBox textBox ->
            TextBox { textBox | autoexpand = state, text = textValue }

        _ ->
            annotation


toLineStyle : StrokeStyle -> ( String, String )
toLineStyle strokeStyle =
    let
        strokeWidth =
            toStrokeWidth strokeStyle
    in
        case strokeStyle of
            SolidThin ->
                toString strokeWidth => ""

            SolidMedium ->
                toString strokeWidth => ""

            SolidThick ->
                toString strokeWidth => ""

            SolidVeryThick ->
                toString strokeWidth => ""

            DashedThin ->
                toString strokeWidth => "10, 5"

            DashedMedium ->
                toString strokeWidth => "10, 5"

            DashedThick ->
                toString strokeWidth => "10, 5"

            DashedVeryThick ->
                toString strokeWidth => "10, 5"


toStrokeWidth : StrokeStyle -> number
toStrokeWidth strokeStyle =
    case strokeStyle of
        SolidThin ->
            2

        SolidMedium ->
            4

        SolidThick ->
            6

        SolidVeryThick ->
            8

        DashedThin ->
            2

        DashedMedium ->
            4

        DashedThick ->
            6

        DashedVeryThick ->
            8


arrowAngle : Position -> Position -> Float
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


setFill : Maybe Color -> AnnotationAttributes -> AnnotationAttributes
setFill fill attrs =
    { attrs | fill = fill }


setStrokeColor : Color -> AnnotationAttributes -> AnnotationAttributes
setStrokeColor strokeColor attrs =
    { attrs | strokeColor = strokeColor }


setStrokeStyle : StrokeStyle -> AnnotationAttributes -> AnnotationAttributes
setStrokeStyle strokeStyle attrs =
    { attrs | strokeStyle = strokeStyle }


setFontSize : Int -> AnnotationAttributes -> AnnotationAttributes
setFontSize fontSize attrs =
    { attrs | fontSize = fontSize }


updateStrokeColor : Color -> Annotation -> Annotation
updateStrokeColor strokeColor annotation =
    case annotation of
        Lines lineType line ->
            Lines lineType { line | strokeColor = strokeColor }

        FreeDraw shape positions ->
            FreeDraw { shape | strokeColor = strokeColor } positions

        Shapes shapeType fill shape ->
            Shapes shapeType fill { shape | strokeColor = strokeColor }

        TextBox textBox ->
            TextBox { textBox | fill = strokeColor }

        Spotlight shapeType shape ->
            Spotlight shapeType { shape | strokeColor = strokeColor }

        Pixelate _ _ ->
            annotation


updateFill : Maybe Color -> Annotation -> Annotation
updateFill fill annotation =
    case annotation of
        Lines _ _ ->
            annotation

        FreeDraw _ _ ->
            annotation

        Shapes shapeType _ shape ->
            Shapes shapeType fill shape

        TextBox textBox ->
            annotation

        Spotlight shapeType shape ->
            annotation

        Pixelate _ _ ->
            annotation


updateStrokeStyle : StrokeStyle -> Annotation -> Annotation
updateStrokeStyle strokeStyle annotation =
    case annotation of
        Lines lineType line ->
            Lines lineType { line | strokeStyle = strokeStyle }

        FreeDraw shape positions ->
            FreeDraw { shape | strokeStyle = strokeStyle } positions

        Shapes shapeType fill shape ->
            Shapes shapeType fill { shape | strokeStyle = strokeStyle }

        TextBox textBox ->
            annotation

        Spotlight shapeType shape ->
            Spotlight shapeType { shape | strokeStyle = strokeStyle }

        Pixelate _ _ ->
            annotation


updateFontSize : Int -> Annotation -> Annotation
updateFontSize fontSize annotation =
    case annotation of
        TextBox textBox ->
            TextBox { textBox | fontSize = fontSize }

        _ ->
            annotation


shift :
    ( Int, Int )
    -> { a | start : Position, end : Position }
    -> { a | end : Position, start : Position }
shift ( dx, dy ) drawing =
    { drawing
        | start = shiftPosition dx dy drawing.start
        , end = shiftPosition dx dy drawing.end
    }


move : ( Int, Int ) -> Annotation -> Annotation
move translate annotation =
    case annotation of
        Lines lineType line ->
            Lines lineType (shift translate line)

        FreeDraw shape positions ->
            FreeDraw (shift translate shape) (List.map (shiftPosition (Tuple.first translate) (Tuple.second translate)) positions)

        Shapes shapeType fill shape ->
            Shapes shapeType fill (shift translate shape)

        TextBox textArea ->
            TextBox (shift translate textArea)

        Spotlight shapeType shape ->
            Spotlight shapeType (shift translate shape)

        Pixelate start end ->
            Pixelate (shiftPosition (Tuple.first translate) (Tuple.second translate) start) (shiftPosition (Tuple.first translate) (Tuple.second translate) end)


resize : Bool -> ResizingInfo -> Annotation -> Annotation
resize constrain resizingInfo annotation =
    case annotation of
        Lines lineType shape ->
            Lines lineType (resizeVertices (calcLinePos constrain) resizingInfo shape)

        FreeDraw _ _ ->
            annotation

        Shapes shapeType fill shape ->
            Shapes shapeType fill (resizeVertices (calcShapePos constrain) resizingInfo shape)

        TextBox textArea ->
            TextBox (resizeVertices (calcShapePos False) resizingInfo textArea)

        Spotlight shapeType shape ->
            Spotlight shapeType (resizeVertices (calcShapePos constrain) resizingInfo shape)

        Pixelate start end ->
            Pixelate (resizeVertices (calcShapePos constrain) resizingInfo { start = start, end = end }).start (resizeVertices (calcShapePos constrain) resizingInfo { start = start, end = end }).end


resizeVertices : (StartPosition -> EndPosition -> Position) -> ResizingInfo -> { a | start : Position, end : Position } -> { a | start : Position, end : Position }
resizeVertices constrain { curPos, vertex, originalCoords } annotation =
    let
        ( start, end ) =
            originalCoords
    in
        case vertex of
            Start ->
                { annotation | start = constrain annotation.end curPos }

            End ->
                { annotation | end = constrain annotation.start curPos }

            StartPlusX ->
                { annotation | start = constrain annotation.end curPos, end = Position start.x end.y }

            StartPlusY ->
                { annotation | start = constrain annotation.end curPos, end = Position end.x start.y }


shiftForPaste : Annotation -> Annotation
shiftForPaste annotation =
    case annotation of
        Lines lineType line ->
            Lines lineType { line | start = shiftPosition 10 10 line.start, end = shiftPosition 10 10 line.end }

        FreeDraw shape positions ->
            FreeDraw { shape | start = shiftPosition 10 10 shape.start, end = shiftPosition 10 10 shape.end } (List.map (shiftPosition 10 10) positions)

        Shapes shapeType fill shape ->
            Shapes shapeType fill { shape | start = shiftPosition 10 10 shape.start, end = shiftPosition 10 10 shape.end }

        TextBox textArea ->
            TextBox { textArea | start = shiftPosition 10 10 textArea.start, end = shiftPosition 10 10 textArea.end }

        Spotlight shapeType shape ->
            Spotlight shapeType { shape | start = shiftPosition 10 10 shape.start, end = shiftPosition 10 10 shape.end }

        Pixelate start end ->
            Pixelate (shiftPosition 10 10 start) (shiftPosition 10 10 end)


shiftPosition : number -> number -> { x : number, y : number } -> { x : number, y : number }
shiftPosition dx dy pos =
    { pos | x = pos.x + dx, y = pos.y + dy }


calcShapePos : Bool -> Position -> Position -> Position
calcShapePos shiftPressed start end =
    if shiftPressed then
        equalXandY start end
    else
        end


calcLinePos : Bool -> Position -> Position -> Position
calcLinePos shiftPressed start end =
    if shiftPressed then
        stepMouse start end
    else
        end


equalXandY : Position -> Position -> Position
equalXandY a b =
    if b.y - a.y < 0 then
        Position b.x (a.y - Basics.max (b.x - a.x) (a.x - b.x))
    else
        Position b.x (a.y + Basics.max (b.x - a.x) (a.x - b.x))


stepMouse : Position -> Position -> Position
stepMouse start curPos =
    arrowAngle start curPos
        / (pi / 4)
        |> round
        |> toFloat
        |> (*) (pi / 4)
        |> toDeltas (calcDistance start curPos)
        |> shiftPosition start.x start.y


toDeltas : Float -> Float -> Position
toDeltas h theta =
    Position (round (cos theta * h)) (round (sin theta * h))


calcDistance : Position -> Position -> Float
calcDistance a b =
    sqrt <| toFloat <| (b.x - a.x) ^ 2 + (b.y - a.y) ^ 2


newTextBox : (Int -> { state : AutoExpand.State, textValue : String } -> msg) -> Int -> AnnotationAttributes -> DrawingInfo -> Annotation
newTextBox onInput id { fill, strokeColor, strokeStyle, fontSize } { start, curPos } =
    TextBox (TextArea start curPos strokeColor fontSize "Text" 0 (AutoExpand.initState (autoExpandConfig onInput id fontSize)))


fromDrawing : Bool -> Drawing -> AnnotationAttributes -> DrawingInfo -> Maybe Annotation
fromDrawing constrain drawing { fill, strokeColor, strokeStyle, fontSize } { start, curPos, positions } =
    case drawing of
        DrawLine lineType ->
            Just (Lines lineType (Shape start (calcLinePos constrain start curPos) strokeColor strokeStyle))

        DrawFreeHand ->
            Just (FreeDraw (Shape start curPos strokeColor strokeStyle) positions)

        DrawShape shapeType ->
            Just (Shapes shapeType fill (Shape start (calcShapePos constrain start curPos) strokeColor strokeStyle))

        DrawTextBox ->
            Nothing

        DrawSpotlight shapeType ->
            Just (Spotlight shapeType (Shape start (calcShapePos constrain start curPos) strokeColor strokeStyle))

        DrawPixelate ->
            Just (Pixelate start curPos)


autoExpandConfig : (Int -> { state : AutoExpand.State, textValue : String } -> msg) -> Int -> Int -> AutoExpand.Config msg
autoExpandConfig onInput index fontSize =
    AutoExpand.config
        { onInput = onInput index
        , padding = 2
        , minRows = 1
        , maxRows = 4
        , lineHeight = fontSizeToLineHeight fontSize
        }
        |> AutoExpand.withId ("text-box-edit--" ++ toString index)
        |> AutoExpand.withClass "text-box-textarea"


fontSizeToLineHeight : Int -> Float
fontSizeToLineHeight fontSize =
    toFloat fontSize * 1.2
