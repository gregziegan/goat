module Goat.View.DrawingArea.Annotation exposing (..)

import AutoExpand
import Color exposing (Color)
import Color.Convert
import Goat.Annotation exposing (Annotation(..), AnnotationAttributes, LineType(..), SelectState(..), Shape, ShapeType(..), StrokeStyle(SolidThin), TextArea, Vertex, arrowAngle, arrowPath, toLineStyle, toStrokeWidth, isFreeHand)
import Goat.EditState as EditState exposing (EditState, MovingInfo, ResizingInfo, whenMoving)
import Goat.Model exposing (..)
import Goat.Update exposing (Msg(..), autoExpandConfig)
import Goat.Utils exposing (calcLinePos, calcShapePos, fontSizeToLineHeight, shiftPosition, toDrawingPosition, toPosition)
import Goat.View.DrawingArea.Vertices as Vertices
import Goat.View.EventUtils exposing (defaultPrevented, stopPropagation, onMouseUp)
import Goat.View.Utils exposing (posToString, toPx, directionToCursor)
import Html exposing (Attribute, Html, button, div, h2, h3, img, li, p, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, src, style)
import Html.Events exposing (onClick, onWithOptions)
import Json.Decode as Json
import Keyboard.Extra exposing (Key(Shift), KeyChange)
import List.Extra
import Mouse exposing (Position)
import Rocket exposing ((=>))
import SingleTouch as ST
import Svg exposing (Svg, circle, defs, foreignObject, marker, rect, svg)
import Svg.Attributes as Attr
import Touch as T


rectAttrs : StartPosition -> EndPosition -> List (Svg.Attribute Msg)
rectAttrs start end =
    [ Attr.width <| toString <| abs <| end.x - start.x
    , Attr.height <| toString <| abs <| end.y - start.y
    , Attr.x <| toString <| Basics.min start.x end.x
    , Attr.y <| toString <| Basics.min start.y end.y
    , Attr.filter "url(#dropShadow)"
    ]


ellipseAttributes : Shape -> List (Svg.Attribute Msg)
ellipseAttributes { start, end } =
    let
        dx =
            toFloat (end.x - start.x)

        dy =
            toFloat (end.y - start.y)
    in
        [ Attr.rx (toString (abs dx / 2))
        , Attr.ry (toString (abs dy / 2))
        , Attr.cx (toString (toFloat start.x + dx / 2))
        , Attr.cy (toString (toFloat start.y + dy / 2))
        , Attr.filter "url(#dropShadow)"
        ]


fillAttrs : Maybe Color -> List (Svg.Attribute Msg)
fillAttrs fill =
    case fill of
        Just color ->
            [ Attr.fill <| Color.Convert.colorToHex color
            , Attr.pointerEvents "auto"
            ]

        Nothing ->
            [ Attr.fillOpacity "0"
            , Attr.pointerEvents "visibleStroke"
            ]


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


freeDrawPathHelper : List Position -> String -> String
freeDrawPathHelper positions pathString =
    case positions of
        [] ->
            pathString

        pos :: rest ->
            freeDrawPathHelper rest (pathString ++ " L " ++ posToString pos)


freeDrawPath : StartPosition -> List Position -> String
freeDrawPath start positions =
    freeDrawPathHelper positions ("M " ++ posToString start)


freeDrawAttributes : Shape -> List Position -> List (Svg.Attribute Msg)
freeDrawAttributes shape positions =
    [ Attr.d (freeDrawPath shape.start (List.reverse (shape.end :: positions)))
    , Attr.fill "none"
    , Attr.strokeLinejoin "round"
    ]
        ++ strokeAttrs shape.strokeStyle shape.strokeColor


shapeAttributes : ShapeType -> Shape -> Maybe Color -> List (Svg.Attribute Msg)
shapeAttributes shapeType shape fill =
    fillAttrs fill
        ++ strokeAttrs shape.strokeStyle shape.strokeColor
        ++ case shapeType of
            Rect ->
                rectAttrs shape.start shape.end ++ [ Attr.strokeLinejoin "round" ]

            RoundedRect ->
                rectAttrs shape.start shape.end ++ [ Attr.rx "15", Attr.ry "15" ]

            Ellipse ->
                ellipseAttributes shape


strokeAttrs : StrokeStyle -> Color -> List (Svg.Attribute Msg)
strokeAttrs strokeStyle strokeColor =
    let
        ( strokeWidth, dashArray ) =
            toLineStyle strokeStyle
    in
        [ Attr.stroke <| Color.Convert.colorToHex strokeColor
        , Attr.strokeWidth strokeWidth
        , Attr.strokeDasharray dashArray
        ]


simpleLineAttrs : Shape -> List (Svg.Attribute Msg)
simpleLineAttrs { start, end, strokeColor, strokeStyle } =
    [ Attr.stroke "none"
    , Attr.fill <| Color.Convert.colorToHex strokeColor
    , Attr.d <| linePath (toStrokeWidth strokeStyle) start end
    , Attr.filter "url(#dropShadow)"
    ]


arrowAttributes : Shape -> List (Svg.Attribute Msg)
arrowAttributes shape =
    [ Attr.stroke "none"
    , Attr.fill (Color.Convert.colorToHex shape.strokeColor)
    , Attr.d (arrowPath shape)
    , Attr.filter "url(#dropShadow)"
    ]


arrowHeadPath : EndPosition -> String
arrowHeadPath pos =
    "M"
        ++ toString (toFloat pos.x - 20.6667)
        ++ ","
        ++ toString (toFloat pos.y - 2.8)
        ++ "l-4.62033,-10.72559l 25.66667, 13.66667l -25.66667, 13.66667l4.62033, -10.33667z"


moveArrowHead : Int -> StartPosition -> EndPosition -> MovingInfo -> List (Svg.Attribute Msg)
moveArrowHead index start end { translate, id } =
    let
        theta =
            (2 * pi)
                - (arrowAngle start end)

        ( dx, dy ) =
            translate
    in
        if index == id then
            [ Attr.transform ("translate(" ++ toString dx ++ "," ++ toString dy ++ ") rotate(" ++ toString (-theta * (180 / pi)) ++ " " ++ toString end.x ++ " " ++ toString end.y ++ ")") ]
        else
            []


arrowHeadAttrs : StartPosition -> EndPosition -> Color -> List (Svg.Attribute Msg)
arrowHeadAttrs start end strokeColor =
    let
        theta =
            (2 * pi)
                - (arrowAngle start end)
    in
        [ Attr.d (arrowHeadPath end)
        , Attr.fill <| Color.Convert.colorToHex strokeColor
        , Attr.stroke "none"
        , Attr.transform ("rotate(" ++ toString (-theta * (180 / pi)) ++ " " ++ toString end.x ++ " " ++ toString end.y ++ ")")
        ]


viewArrowHeadDrawing : Bool -> StartPosition -> Position -> Color -> Svg Msg
viewArrowHeadDrawing showDropShadow start end strokeColor =
    Svg.path
        (arrowHeadAttrs start end strokeColor
            ++ if showDropShadow then
                [ Attr.filter "url(#dropShadow)" ]
               else
                []
        )
        []


viewArrowHead : Int -> EditState -> List (Svg.Attribute Msg) -> Bool -> StartPosition -> EndPosition -> Color -> Svg Msg
viewArrowHead index editState attrs showDropShadow start end strokeColor =
    let
        theta =
            (2 * pi)
                - (arrowAngle start end)
    in
        Svg.path
            (arrowHeadAttrs start end strokeColor
                ++ attrs
                ++ EditState.whenMoving (moveArrowHead index start end) editState []
                ++ if showDropShadow then
                    [ Attr.filter "url(#dropShadow)" ]
                   else
                    []
            )
            []


lineAttributes : LineType -> Shape -> List (Svg.Attribute Msg)
lineAttributes lineType shape =
    case lineType of
        Arrow ->
            strokeAttrs shape.strokeStyle shape.strokeColor ++ arrowAttributes shape

        StraightLine ->
            strokeAttrs shape.strokeStyle shape.strokeColor ++ simpleLineAttrs shape


viewDrawing : Model -> AnnotationAttributes -> EditState -> Bool -> Svg Msg
viewDrawing model annotationAttrs editState isInMask =
    EditState.whenDrawing
        (\{ start, curPos, positions } ->
            viewDrawingHelper model annotationAttrs start curPos positions isInMask
        )
        model.editState
        (Svg.text "")


viewDrawingHelper : Model -> AnnotationAttributes -> StartPosition -> Position -> List Position -> Bool -> Svg Msg
viewDrawingHelper { drawing, pressedKeys, editState } { strokeColor, fill, strokeStyle, fontSize } start curPos freeDrawPositions isInMask =
    let
        constrain =
            List.member Shift pressedKeys

        lineAttrs lineType =
            lineAttributes lineType <| Shape start (calcLinePos constrain start curPos) strokeColor strokeStyle

        shapeAttrs shapeType =
            shapeAttributes shapeType (Shape start (calcShapePos constrain start curPos) strokeColor strokeStyle) fill

        spotlightAttrs shapeType =
            if isInMask then
                shapeAttributes shapeType (Shape start (calcShapePos constrain start curPos) strokeColor strokeStyle) (Just Color.black)
            else
                shapeAttributes shapeType (Shape start (calcShapePos constrain start curPos) strokeColor strokeStyle) Nothing
    in
        case drawing of
            DrawLine lineType ->
                case lineType of
                    Arrow ->
                        Svg.g []
                            [ viewArrowHeadDrawing True start (calcLinePos constrain start curPos) strokeColor
                            , Svg.path (lineAttrs lineType) []
                            , viewArrowHeadDrawing False start (calcLinePos constrain start curPos) strokeColor
                            ]

                    StraightLine ->
                        Svg.path (lineAttrs lineType) []

            DrawFreeHand ->
                Svg.path (freeDrawAttributes (Shape start curPos strokeColor strokeStyle) freeDrawPositions) []

            DrawShape shapeType ->
                case shapeType of
                    Rect ->
                        Svg.rect (shapeAttrs shapeType) []

                    RoundedRect ->
                        Svg.rect (shapeAttrs shapeType) []

                    Ellipse ->
                        Svg.ellipse (shapeAttrs shapeType) []

            DrawTextBox ->
                Svg.rect ((shapeAttributes Rect <| Shape start curPos (Color.rgb 230 230 230) SolidThin) Nothing ++ [ Attr.strokeWidth "1" ]) []

            DrawSpotlight shapeType ->
                case shapeType of
                    Rect ->
                        Svg.rect (spotlightAttrs shapeType) []

                    RoundedRect ->
                        Svg.rect (spotlightAttrs shapeType) []

                    Ellipse ->
                        Svg.ellipse (spotlightAttrs shapeType) []

            DrawPixelate ->
                Svg.text ""


viewLine : Int -> EditState -> List (Svg.Attribute Msg) -> LineType -> Shape -> Svg Msg
viewLine index editState attrs lineType shape =
    case lineType of
        StraightLine ->
            Svg.path (lineAttributes lineType shape ++ attrs) []

        Arrow ->
            Svg.g []
                [ viewArrowHead index editState attrs True shape.start shape.end shape.strokeColor
                , Svg.path (lineAttributes lineType shape ++ attrs) []
                , viewArrowHead index editState attrs False shape.start shape.end shape.strokeColor
                ]


viewFreeDraw : SelectState -> List (Svg.Attribute Msg) -> Shape -> List Position -> Svg Msg
viewFreeDraw selectState attrs shape positions =
    let
        leftMostX =
            List.Extra.minimumBy .x positions
                |> Maybe.map .x
                |> Maybe.withDefault 0

        rightMostX =
            List.Extra.maximumBy .x positions
                |> Maybe.map .x
                |> Maybe.withDefault 0

        topMostY =
            List.Extra.minimumBy .y positions
                |> Maybe.map .y
                |> Maybe.withDefault 0

        bottomMostY =
            List.Extra.maximumBy .y positions
                |> Maybe.map .y
                |> Maybe.withDefault 0
    in
        Svg.g attrs
            ([ Svg.path (freeDrawAttributes shape positions) []
             ]
                ++ if selectState == Selected then
                    [ Svg.rect
                        [ Attr.x (toString (leftMostX - 5))
                        , Attr.y (toString (topMostY - 5))
                        , Attr.width (toString (10 + rightMostX - leftMostX))
                        , Attr.height (toString (10 + bottomMostY - topMostY))
                        , Attr.stroke "#555"
                        , Attr.strokeWidth "0.5"
                        , Attr.strokeDasharray "10, 5"
                        , Attr.fill "none"
                        , Attr.strokeLinejoin "round"
                        , Attr.pointerEvents "none"
                        ]
                        []
                    ]
                   else
                    []
            )


viewShape : List (Svg.Attribute Msg) -> ShapeType -> Maybe Color -> Shape -> Svg Msg
viewShape attrs shapeType fill shape =
    case shapeType of
        Rect ->
            Svg.rect (shapeAttributes shapeType shape fill ++ attrs) []

        RoundedRect ->
            Svg.rect (shapeAttributes shapeType shape fill ++ attrs) []

        Ellipse ->
            Svg.ellipse (shapeAttributes shapeType shape fill ++ attrs) []


viewTextArea : Int -> TextArea -> Svg Msg
viewTextArea index ({ start, end, fill, fontSize, autoexpand } as textArea) =
    foreignObject
        []
        [ div
            [ class "text-box-container"
            , style
                [ "top" => toPx (Basics.min start.y end.y)
                , "left" => toPx (Basics.min start.x end.x)
                , "width" => toPx (abs (end.x - start.x))
                , "font-size" => toPx fontSize
                , "color" => Color.Convert.colorToHex fill
                ]
            , Html.Events.onWithOptions "mousedown" stopPropagation (Json.succeed PreventTextMouseDown)
            ]
            [ AutoExpand.view (autoExpandConfig index fontSize) autoexpand textArea.text
            ]
        ]


viewTextBox : List (Svg.Attribute Msg) -> SelectState -> Int -> TextArea -> Svg Msg
viewTextBox attrs selectState index ({ start, end, fill, fontSize } as textBox) =
    case selectState of
        Selected ->
            viewTextArea index textBox

        NotSelected ->
            textBox.text
                |> String.split "\n"
                |> List.map (Svg.tspan [ Attr.dy <| toString <| fontSizeToLineHeight fontSize, Attr.x <| toString <| Basics.min start.x end.x, Attr.fill <| Color.Convert.colorToHex fill, Attr.fontSize <| toString fontSize ] << List.singleton << Svg.text)
                |> Svg.text_ ([ Attr.y <| toString <| Basics.min start.y end.y, Attr.fontFamily "sans-serif" ] ++ attrs)

        SelectedWithVertices ->
            textBox.text
                |> String.split "\n"
                |> List.map (Svg.tspan [ Attr.dy <| toString <| fontSizeToLineHeight fontSize, Attr.x <| toString <| Basics.min start.x end.x, Attr.fill <| Color.Convert.colorToHex fill ] << List.singleton << Svg.text)
                |> Svg.text_
                    ([ Attr.y <| toString <| Basics.min start.y end.y
                     , Html.Events.onDoubleClick <| FocusTextArea index
                     , ST.onSingleTouch T.TouchStart T.preventAndStop (\_ -> FocusTextArea index)
                     , Attr.stroke <|
                        if fill == Color.black then
                            "white"
                        else
                            "black"
                     , Attr.strokeWidth "0.5px"
                     , Attr.fontSize <| toString fontSize
                     , Attr.fontFamily "sans-serif"
                     ]
                        ++ attrs
                    )


viewPixelate : EditState -> Int -> Annotation -> Maybe (List (Svg Msg))
viewPixelate editState index annotation =
    case annotation of
        Pixelate start end ->
            Just [ Svg.rect (editStateAttributes index editState ++ rectAttrs start end ++ [ Attr.fill "black", Attr.style "all" ]) [] ]

        _ ->
            Nothing


viewAnnotation : EditState -> Int -> Annotation -> ( Svg Msg, Maybe (Svg Msg) )
viewAnnotation editState index annotation =
    let
        selectState =
            EditState.selectState index (not (isFreeHand annotation)) editState

        editStateAttrs =
            editStateAttributes index editState

        toVertexEvents =
            editStateVertexEvents index editState

        vertices verticesType { start, end } =
            Vertices.viewVertices verticesType start end toVertexEvents selectState
    in
        case annotation of
            Lines lineType shape ->
                viewLine index editState editStateAttrs lineType shape
                    => vertices Linear shape

            FreeDraw shape positions ->
                viewFreeDraw selectState editStateAttrs shape positions
                    => Nothing

            Shapes shapeType fill shape ->
                viewShape editStateAttrs shapeType fill shape
                    => vertices Rectangular shape

            TextBox textBox ->
                viewTextBox editStateAttrs selectState index textBox
                    => Nothing

            Spotlight shapeType shape ->
                viewShape editStateAttrs shapeType Nothing shape
                    => vertices Rectangular shape

            Pixelate start end ->
                Svg.rect (rectAttrs start end ++ [ Attr.fill "none", Attr.style "pointer-events: all;" ] ++ editStateAttrs) []
                    => vertices Rectangular { start = start, end = end }


editStateAttributes : Int -> EditState -> List (Svg.Attribute Msg)
editStateAttributes index editState =
    []
        |> EditState.whenNotSelecting (annAttrsWhenNotSelecting index) editState
        |> EditState.whenDrawing (annAttrsWhenDrawing index) editState
        |> EditState.whenSelecting (annAttrsWhenSelecting index) editState
        |> EditState.whenMoving (annAttrsWhenMoving index) editState
        |> EditState.whenResizing (annAttrsWhenResizing index) editState
        |> EditState.whenEditingText (annAttrsWhenEditingText) editState


annAttrsWhenNotSelecting : Int -> List (Svg.Attribute Msg)
annAttrsWhenNotSelecting annIndex =
    [ Html.Events.onWithOptions "mousedown" stopPropagation <| Json.map (SelectAndMoveAnnotation annIndex << toDrawingPosition) Mouse.position
    , Attr.class "pointerCursor"
    , onWithOptions "contextmenu" (Html.Events.Options True True) (Json.map (ToggleSelectedAnnotationMenu annIndex) Mouse.position)
    ]


annAttrsWhenDrawing : Int -> a -> List (Svg.Attribute Msg)
annAttrsWhenDrawing annIndex _ =
    [ Attr.class "crosshairCursor" ]


annAttrsWhenSelecting : Int -> a -> List (Svg.Attribute Msg)
annAttrsWhenSelecting annIndex _ =
    [ Attr.class "moveCursor"
    , Html.Events.onWithOptions "mousedown" stopPropagation <| Json.map (StartMovingAnnotation annIndex << toDrawingPosition) Mouse.position
    , ST.onSingleTouch T.TouchStart T.preventAndStop (StartMovingAnnotation annIndex << toDrawingPosition << toPosition)
    , onWithOptions "contextmenu" defaultPrevented (Json.map (ToggleSelectedAnnotationMenu annIndex) Mouse.position)
    ]


annAttrsWhenMoving : Int -> MovingInfo -> List (Svg.Attribute Msg)
annAttrsWhenMoving annIndex { id, translate } =
    let
        ( dx, dy ) =
            translate
    in
        [ onMouseUp <| Json.map (FinishMovingAnnotation << toDrawingPosition) Mouse.position
        , ST.onSingleTouch T.TouchEnd T.preventAndStop (FinishMovingAnnotation << toDrawingPosition << toPosition)
        , Attr.class "moveCursor"
        ]
            ++ if id == annIndex then
                [ Attr.transform <| "translate(" ++ toString dx ++ "," ++ toString dy ++ ")" ]
               else
                []


annAttrsWhenResizing : Int -> a -> List (Svg.Attribute Msg)
annAttrsWhenResizing annIndex _ =
    [ Attr.class "resizeCursor" ]


annAttrsWhenEditingText : a -> List (Svg.Attribute Msg)
annAttrsWhenEditingText _ =
    [ Attr.class "crosshairCursor" ]


editStateVertexEvents : Int -> EditState -> Vertex -> ResizeDirection -> List (Svg.Attribute Msg)
editStateVertexEvents index editState vertex direction =
    [ Html.Events.onWithOptions "mousedown" stopPropagation <| Json.map (StartResizingAnnotation index vertex << toDrawingPosition) Mouse.position
    , ST.onSingleTouch T.TouchStart T.preventAndStop (StartResizingAnnotation index vertex << toDrawingPosition << toPosition)
    , Attr.class (directionToCursor direction)
    ]
        ++ whenMoving vertexAttrsWhenMoving editState []


vertexAttrsWhenMoving : MovingInfo -> List (Svg.Attribute Msg)
vertexAttrsWhenMoving { translate } =
    let
        ( dx, dy ) =
            translate
    in
        [ Attr.transform <| "translate(" ++ toString dx ++ "," ++ toString dy ++ ")" ]
