module Goat.View.DrawingArea.Annotation exposing (..)

import AutoExpand
import Color exposing (Color)
import Color.Convert
import Goat.Model exposing (..)
import Goat.Update exposing (Msg(..), autoExpandConfig)
import Goat.Utils exposing (arrowAngle, calcLinePos, calcShapePos, toDrawingPosition, toPosition)
import Goat.View.DrawingArea.Vertices as Vertices
import Goat.View.Utils exposing (..)
import Html exposing (Attribute, Html, button, div, h2, h3, img, li, p, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, src, style)
import Html.Events exposing (onClick, onWithOptions)
import Json.Decode as Json
import Keyboard.Extra exposing (Key(Shift), KeyChange, isPressed)
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
    [ Attr.rx <| toString <| abs <| (end.x - start.x) // 2
    , Attr.ry <| toString <| abs <| (end.y - start.y) // 2
    , Attr.cx <| toString <| start.x + ((end.x - start.x) // 2)
    , Attr.cy <| toString <| start.y + ((end.y - start.y) // 2)
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


shapeAttributes : ShapeType -> Shape -> Maybe Color -> List (Svg.Attribute Msg)
shapeAttributes shapeType shape fill =
    fillAttrs fill
        ++ strokeAttrs shape.strokeStyle shape.strokeColor
        ++ case shapeType of
            Rect ->
                rectAttrs shape.start shape.end

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
simpleLineAttrs { start, end } =
    [ Attr.fill "none"
    , Attr.d <| linePath start end

    --  , Attr.filter "url(#dropShadow)"
    ]


arrowAttributes : Shape -> List (Svg.Attribute Msg)
arrowAttributes { start, end, strokeColor } =
    let
        dx =
            toFloat (end.x - start.x)

        dy =
            toFloat (end.y - start.y)

        d =
            sqrt (dx ^ 2 + dy ^ 2)

        halfWayPt =
            dy * 0.54286

        arcPt =
            dy * 0.85714

        theta =
            (2 * pi)
                - (arrowAngle start end)
                |> Debug.log "theta"

        perpen =
            (pi / 2) - theta

        -- |> Debug.log ""
        comp =
            pi - theta

        -- * -1
    in
        -- [ Attr.markerStart ("url(#arrow-head--" ++ Color.Convert.colorToHex strokeColor ++ ")")
        -- , Attr.markerEnd ("url(#arrow-tail--)" ++ Color.Convert.colorToHex strokeColor ++ ")")
        [ Attr.fill (Color.Convert.colorToHex strokeColor)

        -- , Attr.d
        --     ("M "
        --         ++ posToString start
        --         ++ "l"
        --         ++ toString (13 * cos perpen)
        --         ++ ","
        --         ++ toString (13 * sin perpen)
        --         ++ "l "
        --         ++ toString dx
        --         ++ ","
        --         ++ toString dy
        --         ++ "l "
        --         ++ toString (26 * cos comp)
        --         ++ ","
        --         ++ toString (26 * sin comp)
        --         ++ "l"
        --         ++ toString -dx
        --         ++ ","
        --         ++ toString -dy
        --         ++ "l"
        --         ++ toString (13 * cos perpen)
        --         ++ ","
        --         ++ toString (13 * sin perpen)
        --     )
        -- , Attr.d
        --     ("M "
        --         ++ posToString start
        --         ++ "l"
        --         ++ toString (13 * cos perpen)
        --         ++ ","
        --         ++ toString (13 * sin perpen)
        --         ++ "l "
        --         ++ toString dx
        --         ++ ","
        --         ++ toString dy
        --         ++ "l "
        --         ++ toString (26 * cos comp)
        --         ++ ","
        --         ++ toString (26 * sin comp)
        --         ++ "l"
        --         ++ toString -dx
        --         ++ ","
        --         ++ toString -dy
        --         ++ "l"
        --         ++ toString (13 * cos perpen)
        --         ++ ","
        --         ++ toString (13 * sin perpen)
        -- ++ "c -5,"
        -- ++ (toString halfWayPt)
        -- ++ " -5,"
        -- ++ (toString arcPt)
        -- ++ " -5,"
        -- ++ (toString dy)
        -- ++ "l -16,0"
        -- ++ "c 0,"
        -- ++ (toString (arcPt - dy))
        -- ++ " 0,"
        -- ++ (toString (halfWayPt - dy))
        -- ++ " -5,"
        -- ++ (toString -dy)
        -- ++ "z"
        --)
        , Attr.d
            ("M "
                ++ posToString start
                ++ "l"
                ++ toString (13 * cos perpen)
                ++ ","
                ++ toString (13 * sin perpen)
                ++ "l "
                ++ toString dx
                ++ ","
                ++ toString dy
                ++ "l "
                ++ toString (-26 * cos (-theta + (pi / 2)))
                ++ ","
                ++ toString (-26 * sin (-theta + (pi / 2)))
                ++ "l"
                ++ toString -dx
                ++ ","
                ++ toString -dy
                ++ "z"
             -- ++ "l"
             -- ++ toString (13 * cos -comp)
             -- ++ ","
             -- ++ toString (13 * sin -comp)
            )

        -- ++ "l"
        -- ++ toString (13 * cos perpen)
        -- ++ ","
        -- ++ toString (13 * sin perpen)
        -- , Attr.filter "url(#dropShadow)"
        ]


posToString : Position -> String
posToString pos =
    toString pos.x ++ "," ++ toString pos.y


lineAttributes : LineType -> Shape -> List (Svg.Attribute Msg)
lineAttributes lineType shape =
    case lineType of
        Arrow ->
            arrowAttributes shape ++ strokeAttrs shape.strokeStyle shape.strokeColor

        StraightLine ->
            simpleLineAttrs shape ++ strokeAttrs shape.strokeStyle shape.strokeColor


viewDrawing : Model -> AnnotationAttributes -> StartPosition -> Position -> Bool -> Svg Msg
viewDrawing { drawing, keyboardState } { strokeColor, fill, strokeStyle, fontSize } start curPos isInMask =
    let
        discretize =
            isPressed Shift keyboardState

        lineAttrs lineType =
            lineAttributes lineType <| Shape start (calcLinePos discretize start curPos) strokeColor strokeStyle

        shapeAttrs shapeType =
            shapeAttributes shapeType (Shape start (calcShapePos discretize start curPos) strokeColor strokeStyle) fill

        spotlightAttrs shapeType =
            if isInMask then
                shapeAttributes shapeType (Shape start (calcShapePos discretize start curPos) strokeColor strokeStyle) (Just Color.black)
            else
                shapeAttributes shapeType (Shape start (calcShapePos discretize start curPos) strokeColor strokeStyle) Nothing
    in
        case drawing of
            DrawLine lineType ->
                case lineType of
                    Arrow ->
                        Svg.path (lineAttrs lineType) []

                    StraightLine ->
                        Svg.path (lineAttrs lineType) []

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


getSelectState : Int -> AnnotationState -> SelectState
getSelectState annIndex annotationState =
    case annotationState of
        SelectedAnnotation index _ ->
            if index == annIndex then
                SelectedWithVertices
            else
                NotSelected

        MovingAnnotation index _ _ _ ->
            if index == annIndex then
                SelectedWithVertices
            else
                NotSelected

        ResizingAnnotation { index } _ ->
            if index == annIndex then
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


viewLine : List (Svg.Attribute Msg) -> LineType -> Shape -> List (Svg Msg)
viewLine attrs lineType shape =
    case lineType of
        StraightLine ->
            [ Svg.path (lineAttributes lineType shape ++ attrs) [] ]

        Arrow ->
            [ Svg.path (lineAttributes lineType shape ++ attrs) [] ]


viewShape : List (Svg.Attribute Msg) -> ShapeType -> Maybe Color -> Shape -> List (Svg Msg)
viewShape attrs shapeType fill shape =
    case shapeType of
        Rect ->
            [ Svg.rect (shapeAttributes shapeType shape fill ++ attrs) [] ]

        RoundedRect ->
            [ Svg.rect (shapeAttributes shapeType shape fill ++ attrs) [] ]

        Ellipse ->
            [ Svg.ellipse (shapeAttributes shapeType shape fill ++ attrs) [] ]


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


viewTextBox : List (Svg.Attribute Msg) -> SelectState -> Int -> TextArea -> List (Svg Msg)
viewTextBox attrs selectState index ({ start, end, fill, fontSize } as textBox) =
    case selectState of
        Selected ->
            viewTextArea index textBox
                |> List.singleton

        NotSelected ->
            textBox.text
                |> String.split "\n"
                |> List.map (Svg.tspan [ Attr.dy <| toString <| fontSizeToLineHeight fontSize, Attr.x <| toString <| Basics.min start.x end.x, Attr.fill <| Color.Convert.colorToHex fill, Attr.fontSize <| toString fontSize ] << List.singleton << Svg.text)
                |> Svg.text_ ([ Attr.y <| toString <| Basics.min start.y end.y ] ++ attrs)
                |> List.singleton

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
                     ]
                        ++ attrs
                    )
                |> List.singleton


viewPixelate : AnnotationState -> Int -> Annotation -> Maybe (List (Svg Msg))
viewPixelate annotationState index annotation =
    case annotation of
        Pixelate start end ->
            Just [ Svg.rect (rectAttrs start end ++ [ Attr.fill "black", Attr.style "all" ] ++ (annotationStateEvents index annotationState)) [] ]

        _ ->
            Nothing


viewAnnotation : AnnotationState -> Int -> Annotation -> List (Svg Msg)
viewAnnotation annotationState index annotation =
    let
        selectState =
            getSelectState index annotationState

        annotationStateAttrs =
            annotationStateEvents index annotationState

        toVertexEvents =
            annotationStateVertexEvents index annotationState

        vertices verticesType { start, end } =
            Vertices.viewVertices verticesType start end toVertexEvents selectState
    in
        case annotation of
            Lines lineType shape ->
                viewLine annotationStateAttrs lineType shape
                    |> flip List.append (vertices Linear shape)

            Shapes shapeType fill shape ->
                viewShape annotationStateAttrs shapeType fill shape
                    |> flip List.append (vertices Rectangular shape)

            TextBox textBox ->
                viewTextBox annotationStateAttrs selectState index textBox

            Spotlight shapeType shape ->
                viewShape annotationStateAttrs shapeType Nothing shape
                    |> flip List.append (vertices Rectangular shape)

            Pixelate start end ->
                [ Svg.rect (rectAttrs start end ++ [ Attr.fill "none", Attr.style "pointer-events: all;" ] ++ annotationStateAttrs) [] ]
                    |> flip List.append (vertices Rectangular { start = start, end = end })
