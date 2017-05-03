module Goat.View.DrawingArea.Annotation exposing (..)

import AutoExpand
import Color exposing (Color)
import Color.Convert
import Goat.Model exposing (..)
import Goat.Update exposing (Msg(..), autoExpandConfig)
import Goat.Utils exposing (arrowAngle, calcLinePos, shiftPosition, calcShapePos, toDrawingPosition, toPosition)
import Goat.View.DrawingArea.Vertices as Vertices
import Goat.View.Utils exposing (..)
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


viewArrowHead : List (Svg.Attribute Msg) -> ( Int, Int ) -> StartPosition -> EndPosition -> Color -> Svg Msg
viewArrowHead attrs ( dx, dy ) start end strokeColor =
    let
        theta =
            (2 * pi)
                - (arrowAngle start end)
    in
        Svg.path
            (attrs
                ++ [ Attr.d (arrowHeadPath end)
                   , Attr.fill <| Color.Convert.colorToHex strokeColor
                   , Attr.stroke "none"
                   , Attr.transform ("translate(" ++ toString dx ++ "," ++ toString dy ++ ") rotate(" ++ toString (-theta * (180 / pi)) ++ " " ++ toString end.x ++ " " ++ toString end.y ++ ")")
                   ]
            )
            []


lineAttributes : LineType -> Shape -> List (Svg.Attribute Msg)
lineAttributes lineType shape =
    case lineType of
        Arrow ->
            strokeAttrs shape.strokeStyle shape.strokeColor ++ arrowAttributes shape

        StraightLine ->
            strokeAttrs shape.strokeStyle shape.strokeColor ++ simpleLineAttrs shape


viewDrawing : Model -> AnnotationAttributes -> StartPosition -> Position -> Bool -> Svg Msg
viewDrawing { drawing, pressedKeys, freeDrawPositions } { strokeColor, fill, strokeStyle, fontSize } start curPos isInMask =
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
                            [ viewArrowHead [ Attr.filter "url(#dropShadow)" ] ( 0, 0 ) start (calcLinePos constrain start curPos) strokeColor
                            , Svg.path (lineAttrs lineType) []
                            , viewArrowHead [] ( 0, 0 ) start (calcLinePos constrain start curPos) strokeColor
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


getSelectState : Int -> Annotation -> AnnotationState -> SelectState
getSelectState annIndex annotation annotationState =
    case annotationState of
        SelectedAnnotation index _ ->
            if index == annIndex then
                case annotation of
                    FreeDraw _ _ ->
                        Selected

                    _ ->
                        SelectedWithVertices
            else
                NotSelected

        MovingAnnotation index _ _ _ ->
            if index == annIndex then
                case annotation of
                    FreeDraw _ _ ->
                        Selected

                    _ ->
                        SelectedWithVertices
            else
                NotSelected

        ResizingAnnotation { index } _ ->
            if index == annIndex then
                case annotation of
                    FreeDraw _ _ ->
                        Selected

                    _ ->
                        SelectedWithVertices
            else
                NotSelected

        EditingATextBox index _ ->
            if index == annIndex then
                Selected
            else
                NotSelected

        _ ->
            NotSelected


annotationStateEvents : Int -> AnnotationState -> List (Svg.Attribute Msg)
annotationStateEvents annIndex annotationState =
    case annotationState of
        ReadyToDraw ->
            [ Html.Events.onWithOptions "mousedown" stopPropagation <| Json.map (SelectAndMoveAnnotation annIndex << toDrawingPosition) Mouse.position
            , Attr.class "pointerCursor"
            , onWithOptions "contextmenu" (Html.Events.Options True True) (Json.map (ToggleSelectedAnnotationMenu annIndex) Mouse.position)
            ]

        DrawingAnnotation _ _ ->
            [ Attr.class "crosshairCursor" ]

        SelectedAnnotation _ _ ->
            [ Attr.class "moveCursor"
            , Html.Events.onWithOptions "mousedown" stopPropagation <| Json.map (StartMovingAnnotation annIndex << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchStart T.preventAndStop (StartMovingAnnotation annIndex << toDrawingPosition << toPosition)
            , onWithOptions "contextmenu" defaultPrevented (Json.map (ToggleSelectedAnnotationMenu annIndex) Mouse.position)
            ]

        MovingAnnotation index _ ( dx, dy ) _ ->
            [ onMouseUp <| Json.map (FinishMovingAnnotation << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchEnd T.preventAndStop (FinishMovingAnnotation << toDrawingPosition << toPosition)
            , Attr.class "moveCursor"
            ]
                ++ if index == annIndex then
                    [ Attr.transform <| "translate(" ++ toString dx ++ "," ++ toString dy ++ ")" ]
                   else
                    []

        ResizingAnnotation _ _ ->
            [ Attr.class "resizeCursor"
            ]

        EditingATextBox index _ ->
            [ Attr.class "crosshairCursor" ]


annotationStateVertexEvents : Int -> AnnotationState -> Vertex -> ResizeDirection -> List (Svg.Attribute Msg)
annotationStateVertexEvents index annotationState vertex direction =
    [ Html.Events.onWithOptions "mousedown" stopPropagation <| Json.map (StartResizingAnnotation index vertex << toDrawingPosition) Mouse.position
    , ST.onSingleTouch T.TouchStart T.preventAndStop (StartResizingAnnotation index vertex << toDrawingPosition << toPosition)
    , Attr.class (directionToCursor direction)
    ]
        ++ case annotationState of
            MovingAnnotation _ _ ( dx, dy ) _ ->
                [ Attr.transform <| "translate(" ++ toString dx ++ "," ++ toString dy ++ ")" ]

            ResizingAnnotation _ _ ->
                [ onMouseUp <| Json.map (FinishResizingAnnotation << toDrawingPosition) Mouse.position
                , ST.onSingleTouch T.TouchEnd T.preventAndStop (FinishResizingAnnotation << toDrawingPosition << toPosition)
                ]

            _ ->
                []


viewLine : ( Int, Int ) -> List (Svg.Attribute Msg) -> LineType -> Shape -> Svg Msg
viewLine offset attrs lineType shape =
    case lineType of
        StraightLine ->
            Svg.path (lineAttributes lineType shape ++ attrs) []

        Arrow ->
            Svg.g []
                [ viewArrowHead (Attr.filter "url(#dropShadow)" :: attrs) offset shape.start shape.end shape.strokeColor
                , Svg.path (lineAttributes lineType shape ++ attrs) []
                , viewArrowHead attrs offset shape.start shape.end shape.strokeColor
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
            [ AutoExpand.view (autoExpandConfig index) (fontSizeToLineHeight fontSize) autoexpand textArea.text
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


viewPixelate : AnnotationState -> Int -> Annotation -> Maybe (List (Svg Msg))
viewPixelate annotationState index annotation =
    case annotation of
        Pixelate start end ->
            Just [ Svg.rect (rectAttrs start end ++ [ Attr.fill "black", Attr.style "all" ] ++ (annotationStateEvents index annotationState)) [] ]

        _ ->
            Nothing


viewAnnotation : AnnotationState -> Int -> Annotation -> ( Svg Msg, Maybe (Svg Msg) )
viewAnnotation annotationState index annotation =
    let
        selectState =
            getSelectState index annotation annotationState

        annotationStateAttrs =
            annotationStateEvents index annotationState

        offset =
            case annotationState of
                MovingAnnotation annIndex _ offset _ ->
                    if annIndex == index then
                        offset
                    else
                        ( 0, 0 )

                _ ->
                    ( 0, 0 )

        toVertexEvents =
            annotationStateVertexEvents index annotationState

        vertices verticesType { start, end } =
            Vertices.viewVertices verticesType start end toVertexEvents selectState
    in
        case annotation of
            Lines lineType shape ->
                viewLine offset annotationStateAttrs lineType shape
                    => vertices Linear shape

            FreeDraw shape positions ->
                viewFreeDraw selectState annotationStateAttrs shape positions
                    => Nothing

            Shapes shapeType fill shape ->
                viewShape annotationStateAttrs shapeType fill shape
                    => vertices Rectangular shape

            TextBox textBox ->
                viewTextBox annotationStateAttrs selectState index textBox
                    => Nothing

            Spotlight shapeType shape ->
                viewShape annotationStateAttrs shapeType Nothing shape
                    => vertices Rectangular shape

            Pixelate start end ->
                Svg.rect (rectAttrs start end ++ [ Attr.fill "none", Attr.style "pointer-events: all;" ] ++ annotationStateAttrs) []
                    => vertices Rectangular { start = start, end = end }
