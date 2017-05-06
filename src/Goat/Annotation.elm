module Goat.Annotation exposing (Annotation(..), SelectState(..), StrokeStyle(..), LineType(..), Shape, ShapeType(..), TextArea, AnnotationAttributes, defaultStroke, strokeStyles, isFreeHand, isSpotlightShape, isEmptyTextBox, updateTextArea, attributes, arrowAngle, updateFill, updateStrokeColor, updateStrokeStyle, updateFontSize, positions, toLineStyle, toStrokeWidth, arrowPath, spotlightToMaskCutout)

import AutoExpand
import Color exposing (Color)
import Mouse exposing (Position)
import Rocket exposing ((=>))


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


type StrokeStyle
    = SolidThin
    | SolidMedium
    | SolidThick
    | SolidVeryThick
    | DashedThin
    | DashedMedium
    | DashedThick
    | DashedVeryThick


type alias AnnotationAttributes =
    { strokeColor : Color
    , fill : Maybe Color
    , strokeStyle : StrokeStyle
    , fontSize : Int
    }


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


spotlightToMaskCutout : ( Int, Annotation ) -> Maybe ( Int, ShapeType, Shape )
spotlightToMaskCutout ( index, annotation ) =
    case annotation of
        Spotlight shapeType shape ->
            Just ( index, shapeType, shape )

        _ ->
            Nothing


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


updateFill : Maybe Color -> AnnotationAttributes -> AnnotationAttributes
updateFill fill attrs =
    { attrs | fill = fill }


updateStrokeColor : Color -> AnnotationAttributes -> AnnotationAttributes
updateStrokeColor strokeColor attrs =
    { attrs | strokeColor = strokeColor }


updateStrokeStyle : StrokeStyle -> AnnotationAttributes -> AnnotationAttributes
updateStrokeStyle strokeStyle attrs =
    { attrs | strokeStyle = strokeStyle }


updateFontSize : Int -> AnnotationAttributes -> AnnotationAttributes
updateFontSize fontSize attrs =
    { attrs | fontSize = fontSize }
