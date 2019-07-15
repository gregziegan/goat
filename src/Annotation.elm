module Annotation exposing
    ( Annotation(..)
    , TextArea
    , autoExpandConfig
    , fontSizeToLineHeight
    , fromDrawing
    , isEmptyTextBox
    , isFreeHand
    , isSpotlightShape
    , move
    , newTextBox
    , resize
    , shiftForPaste
    , startAndEnd
    , styles
    , textareaPadding
    , updateAttributes
    , updateFill
    , updateTextArea
    )

import AutoExpand
import Color exposing (Color)
import Controls
import Drawing exposing (Drawing(..), LineType, Shape, ShapeType, calcLinePos, calcShapePos)
import Drawing.Options exposing (DrawingStyles, StrokeStyle(..))
import EditState exposing (ResizingInfo)
import Html.Attributes
import Position exposing (Position)


type alias Config =
    { constrain : Bool
    , drawing : Drawing
    , styles : DrawingStyles
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
            String.trim textArea.text == ""

        _ ->
            False


startAndEnd : Annotation -> ( Position, Position )
startAndEnd annotation =
    case annotation of
        Lines _ line ->
            ( line.start, line.end )

        FreeDraw shape _ ->
            ( shape.start, shape.end )

        Shapes _ _ shape ->
            ( shape.start, shape.end )

        TextBox textArea ->
            ( textArea.start, textArea.end )

        Spotlight _ shape ->
            ( shape.start, shape.end )

        Pixelate start end ->
            ( start, end )


defaultAttributes : DrawingStyles
defaultAttributes =
    Controls.defaultDrawingStyles


styles : Annotation -> DrawingStyles
styles annotation =
    let
        fromShape shape =
            { defaultAttributes | strokeColor = shape.strokeColor, strokeStyle = shape.strokeStyle }

        withFill fill attrs =
            { attrs | fill = fill }
    in
    case annotation of
        Lines _ shape ->
            fromShape shape

        FreeDraw shape _ ->
            fromShape shape

        Shapes _ fill shape ->
            shape
                |> fromShape
                |> withFill fill

        TextBox textArea ->
            { defaultAttributes | strokeColor = textArea.fill, fill = Just textArea.fill, fontSize = textArea.fontSize }

        Spotlight _ shape ->
            fromShape shape

        Pixelate _ _ ->
            defaultAttributes


updateTextArea : AutoExpand.State -> String -> Annotation -> Annotation
updateTextArea state textValue annotation =
    case annotation of
        TextBox textBox ->
            TextBox { textBox | autoexpand = state, text = textValue }

        _ ->
            annotation


updateAttributes : DrawingStyles -> Annotation -> Annotation
updateAttributes { strokeColor, fill, strokeStyle, fontSize } annotation =
    case annotation of
        Lines lineType line ->
            Lines lineType { line | strokeColor = strokeColor, strokeStyle = strokeStyle }

        FreeDraw shape points ->
            FreeDraw { shape | strokeColor = strokeColor } points

        Shapes shapeType _ shape ->
            Shapes shapeType fill { shape | strokeColor = strokeColor, strokeStyle = strokeStyle }

        TextBox textBox ->
            TextBox { textBox | fill = strokeColor, fontSize = fontSize }

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

        TextBox _ ->
            annotation

        Spotlight _ _ ->
            annotation

        Pixelate _ _ ->
            annotation


shift :
    ( Int, Int )
    -> { a | start : Position, end : Position }
    -> { a | end : Position, start : Position }
shift ( dx, dy ) drawing =
    { drawing
        | start = Position.shift dx dy drawing.start
        , end = Position.shift dx dy drawing.end
    }


move : ( Int, Int ) -> Annotation -> Annotation
move translate annotation =
    case annotation of
        Lines lineType line ->
            Lines lineType (shift translate line)

        FreeDraw shape points ->
            FreeDraw (shift translate shape) (List.map (Position.shift (Tuple.first translate) (Tuple.second translate)) points)

        Shapes shapeType fill shape ->
            Shapes shapeType fill (shift translate shape)

        TextBox textArea ->
            TextBox (shift translate textArea)

        Spotlight shapeType shape ->
            Spotlight shapeType (shift translate shape)

        Pixelate start end ->
            Pixelate (Position.shift (Tuple.first translate) (Tuple.second translate) start) (Position.shift (Tuple.first translate) (Tuple.second translate) end)


resize : Bool -> ResizingInfo -> Annotation -> Annotation
resize constrain resizingInfo annotation =
    case annotation of
        Lines lineType shape ->
            Lines lineType (EditState.resizeVertices (calcLinePos constrain) resizingInfo shape)

        FreeDraw _ _ ->
            annotation

        Shapes shapeType fill shape ->
            Shapes shapeType fill (EditState.resizeVertices (calcShapePos constrain) resizingInfo shape)

        TextBox textArea ->
            TextBox (EditState.resizeVertices (calcShapePos False) resizingInfo textArea)

        Spotlight shapeType shape ->
            Spotlight shapeType (EditState.resizeVertices (calcShapePos constrain) resizingInfo shape)

        Pixelate start end ->
            Pixelate (EditState.resizeVertices (calcShapePos constrain) resizingInfo { start = start, end = end }).start (EditState.resizeVertices (calcShapePos constrain) resizingInfo { start = start, end = end }).end


shiftForPaste : Annotation -> Annotation
shiftForPaste annotation =
    case annotation of
        Lines lineType line ->
            Lines lineType { line | start = Position.shift 10 10 line.start, end = Position.shift 10 10 line.end }

        FreeDraw shape points ->
            FreeDraw { shape | start = Position.shift 10 10 shape.start, end = Position.shift 10 10 shape.end } (List.map (Position.shift 10 10) points)

        Shapes shapeType fill shape ->
            Shapes shapeType fill { shape | start = Position.shift 10 10 shape.start, end = Position.shift 10 10 shape.end }

        TextBox textArea ->
            TextBox { textArea | start = Position.shift 10 10 textArea.start, end = Position.shift 10 10 textArea.end }

        Spotlight shapeType shape ->
            Spotlight shapeType { shape | start = Position.shift 10 10 shape.start, end = Position.shift 10 10 shape.end }

        Pixelate start end ->
            Pixelate (Position.shift 10 10 start) (Position.shift 10 10 end)


newTextBox : (Int -> { state : AutoExpand.State, textValue : String } -> msg) -> Int -> DrawingStyles -> EditState.DrawingInfo -> Annotation
newTextBox onInput id { strokeColor, fontSize } { start, curPos } =
    TextBox (TextArea start curPos strokeColor fontSize "Text" 0 (AutoExpand.initState (autoExpandConfig onInput id fontSize)))


fromDrawing : Config -> EditState.DrawingInfo -> Maybe Annotation
fromDrawing config { start, curPos, positions } =
    let
        { fill, strokeColor, strokeStyle } =
            config.styles
    in
    case config.drawing of
        DrawLine lineType ->
            Just (Lines lineType (Shape start (calcLinePos config.constrain start curPos) strokeColor strokeStyle))

        DrawFreeHand ->
            Just (FreeDraw (Shape start curPos strokeColor strokeStyle) positions)

        DrawShape shapeType ->
            Just (Shapes shapeType fill (Shape start (calcShapePos config.constrain start curPos) strokeColor strokeStyle))

        DrawTextBox ->
            Nothing

        DrawSpotlight shapeType ->
            Just (Spotlight shapeType (Shape start (calcShapePos config.constrain start curPos) strokeColor strokeStyle))

        DrawPixelate ->
            Just (Pixelate start curPos)


autoExpandConfig : (Int -> { state : AutoExpand.State, textValue : String } -> msg) -> Int -> Int -> AutoExpand.Config msg
autoExpandConfig onInput index fontSize =
    AutoExpand.config
        { onInput = onInput index
        , padding = textareaPadding
        , minRows = 1
        , maxRows = 4
        , lineHeight = fontSizeToLineHeight fontSize
        }
        |> AutoExpand.withAttribute (Html.Attributes.id ("text-box-edit--" ++ String.fromInt index))
        |> AutoExpand.withAttribute (Html.Attributes.class "text-box-textarea")


fontSizeToLineHeight : Int -> Float
fontSizeToLineHeight fontSize =
    toFloat fontSize * 1.2


textareaPadding : number
textareaPadding =
    2
