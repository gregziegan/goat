module Annotation exposing (Annotation, Position, arrow, line, freeDraw, rect, roundedRect, ellipse, textBox, pixelate)

import Annotation.Attributes exposing (Attributes, Fill)
import Annotation.Selection exposing (Selection)
import AutoExpand
import Color exposing (Color)
import DrawingArea.Vertices as Vertices
import EditState as EditState exposing (AnnotationConfig, EditState)
import EventUtils exposing (onMouseUp, stopPropagationAndDefault)
import Html exposing (Attribute, Html, button, div, h2, h3, img, li, p, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, src, style)
import Html.Events exposing (onClick)
import Html.Events.Extra.Touch as T
import Json.Decode as Json
import List.Extra
import Mouse
import Svg exposing (Svg, circle, defs, foreignObject, marker, rect, svg)
import Svg.Attributes as Attr
import Utils exposing (toPx)


type Vertices
    = Rectangular
    | Linear


type alias LineInfo =
    { start : Mouse.Position, end : Mouse.Position, strokeStyle : StrokeStyle, strokeColor : Color }

type alias FreeDrawInfo =
    { positions : List Mouse.Position, strokeStyle : StrokeStyle, strokeColor : Color }



type alias RectangleInfo =
    { rect : Rect
    , fill : Fill
    , strokeColor : Color
    , strokeStyle : StrokeStyle
    , rounded : Boolean
    }


type alias EllipseInfo =
    { cx : Int
    , cy : Int
    , rx : Int
    , ry : Int
    , fill : Fill
    , strokeColor : Color
    , strokeStyle : StrokeStyle
    }


type alias TextBoxInfo =
    { width : Int
    , color : Color
    , fontSize : Int
    , text : String
    , angle : Float
    , expandState : AutoExpand.State
    }


type Annotation
    = Line LineInfo
    | Arrow LineInfo
    | FreeDraw FreeDrawInfo
    | Rectangle RectangleInfo
    | Ellipse EllipseInfo
    | TextBox TextBoxInfo
    | Pixelate Rect


lineHelp : Position -> Annotation.Attributes -> Annotation
lineHelp { start, end } { strokeStyle, strokeColor } =
    LineInfo start strokeStyle strokeColor

arrow : Position -> Annotation.Attributes -> Annotation
arrow =
    Arrow lineHelp
  

line : Position -> Annotation.Attributes -> Annotation
line =
    Line lineHelp

freeDraw : List { x : Int, y : Int } -> Annotation.Attributes -> Annotation
freeDraw positions { strokeStyle, strokeColor } =
    FreeDraw (FreeDrawInfo positions strokeStyle strokeColor)

rectHelp : Boolean -> Position -> Annotation.Attributes -> Annotation
rectHelp rounded position attributes =
    Rectangle
        { rect = rectFromPosition position
        , fill = attributes.fill
        , strokeStyle = attributes.strokeStyle
        , strokeColor = attributes.strokeColor
        , rounded = rounded
        }


rect : Position -> Annotation.Attributes -> Annotation
rect position attributes =
    rectHelp False position attributes
  

roundedRect : Position -> Annotation.Attributes -> Annotation
roundedRect position attributes =
    rectHelp True position attributes



ellipse : Position -> Annotation.Attributes -> Annotation
ellipse position attributes =
    let
        dx =
            end.x - start.x

        dy =
            end.y - start.y
    in
    Ellipse
        { cx = start.x + dx / 2
        , cy = start.y + dy / 2
        , rx = abs dx / 2
        , ry = abs dy / 2
        , fill = attributes.fill
        , strokeStyle = attributes.strokeStyle
        , strokeColor = attributes.strokeColor
        }

textBox : String -> (String -> msg) -> Position -> Annotation.Attributes -> Annotation
textBox id onInput position attributes =
    TextBox
        { width = 
        , color = attributes.strokeColor
        , fontSize = attributes.fontSize
        , text = ""
        , angle = 0.0
        , expandState = AutoExpand.initState (autoExpandConfig onInput id attributes.fontSize))
        }

pixelate : Position -> Annotation
pixelate = position
    Pixelate (rectFromPosition position)




rectAttrs : Rect -> List (Svg.Attribute Msg)
rectAttrs start end =
    [ Attr.width <| String.fromInt <| 
    , Attr.height <| String.fromInt <| abs <| end.y - start.y
    , Attr.x <| String.fromInt <| Basics.min start.x end.x
    , Attr.y <| String.fromInt <| Basics.min start.y end.y
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
    [ Attr.rx (String.fromFloat (abs dx / 2))
    , Attr.ry (String.fromFloat (abs dy / 2))
    , Attr.cx (String.fromFloat (toFloat start.x + dx / 2))
    , Attr.cy (String.fromFloat (toFloat start.y + dy / 2))
    , Attr.filter "url(#dropShadow)"
    ]


fillAttrs : Maybe Color -> List (Svg.Attribute Msg)
fillAttrs fill =
    case fill of
        Just color ->
            [ Attr.fill color
            , Attr.pointerEvents "auto"
            ]

        Nothing ->
            [ Attr.fillOpacity "0"
            , Attr.pointerEvents "visibleStroke"
            ]


positionToString : Mouse.Position -> String
positionToString { x, y } =
    String.fromInt x ++ "," ++ String.fromInt y


posToString : { x : Float, y : Float } -> String
posToString { x, y } =
    String.fromFloat x ++ "," ++ String.fromFloat y


linePath : Float -> StartPosition -> EndPosition -> String
linePath strokeWidth start end =
    let
        theta =
            (2 * pi)
                - arrowAngle start end

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
        ++ positionToString start
        ++ " L"
        ++ posToString ccPt1
        ++ " L"
        ++ posToString ccPt2
        ++ " L"
        ++ posToString ccPt3
        ++ " L"
        ++ posToString ccPt4
        ++ "Z"


freeDrawPathHelper : List Mouse.Position -> String -> String
freeDrawPathHelper positions pathString =
    case positions of
        [] ->
            pathString

        lastPos :: [] ->
            pathString ++ " L " ++ positionToString lastPos

        pos :: nextPos :: rest ->
            freeDrawPathHelper rest (pathString ++ " S " ++ positionToString pos ++ " " ++ positionToString nextPos)


freeDrawPath : StartPosition -> List Mouse.Position -> String
freeDrawPath start positions =
    freeDrawPathHelper positions ("M " ++ positionToString start)


freeDrawAttributes : Shape -> List Mouse.Position -> List (Svg.Attribute Msg)
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
        ++ (case shapeType of
                Rect ->
                    rectAttrs shape.start shape.end ++ [ Attr.strokeLinejoin "round" ]

                RoundedRect ->
                    rectAttrs shape.start shape.end ++ [ Attr.rx "15", Attr.ry "15" ]

                Ellipse ->
                    ellipseAttributes shape
           )


strokeAttrs : StrokeStyle -> Color -> List (Svg.Attribute Msg)
strokeAttrs strokeStyle strokeColor =
    let
        ( strokeWidth, dashArray ) =
            toLineStyle strokeStyle
    in
    [ Attr.stroke strokeColor
    , Attr.strokeWidth strokeWidth
    , Attr.strokeDasharray dashArray
    ]


simpleLineAttrs : Shape -> List (Svg.Attribute Msg)
simpleLineAttrs { start, end, strokeColor, strokeStyle } =
    [ Attr.stroke "none"
    , Attr.fill strokeColor
    , Attr.d <| linePath (toStrokeWidth strokeStyle) start end
    , Attr.filter "url(#dropShadow)"
    ]


arrowAttributes : Shape -> List (Svg.Attribute Msg)
arrowAttributes shape =
    [ Attr.stroke "none"
    , Attr.fill shape.strokeColor
    , Attr.d (arrowPath shape)
    , Attr.filter "url(#dropShadow)"
    ]


arrowHeadPath : EndPosition -> String
arrowHeadPath pos =
    "M"
        ++ String.fromFloat (toFloat pos.x - 20.6667)
        ++ ","
        ++ String.fromFloat (toFloat pos.y - 2.8)
        ++ "l-4.62033,-10.72559l 25.66667, 13.66667l -25.66667, 13.66667l4.62033, -10.33667z"


moveArrowHead : Int -> StartPosition -> EndPosition -> MovingInfo -> List (Svg.Attribute Msg)
moveArrowHead index start end { translate, id } =
    let
        theta =
            (2 * pi)
                - arrowAngle start end

        ( dx, dy ) =
            translate
    in
    if index == id then
        [ Attr.transform ("translate(" ++ String.fromInt dx ++ "," ++ String.fromInt dy ++ ") rotate(" ++ String.fromFloat (-theta * (180 / pi)) ++ " " ++ String.fromInt end.x ++ " " ++ String.fromInt end.y ++ ")") ]

    else
        []


arrowHeadAttrs : StartPosition -> EndPosition -> Color -> List (Svg.Attribute Msg)
arrowHeadAttrs start end strokeColor =
    let
        theta =
            (2 * pi)
                - arrowAngle start end
    in
    [ Attr.d (arrowHeadPath end)
    , Attr.fill strokeColor
    , Attr.stroke "none"
    , Attr.transform ("rotate(" ++ String.fromFloat (-theta * (180 / pi)) ++ " " ++ String.fromInt end.x ++ " " ++ String.fromInt end.y ++ ")")
    ]


viewArrowHeadDrawing : Bool -> StartPosition -> Mouse.Position -> Color -> Svg Msg
viewArrowHeadDrawing showDropShadow start end strokeColor =
    Svg.path
        (arrowHeadAttrs start end strokeColor
            ++ (if showDropShadow then
                    [ Attr.filter "url(#dropShadow)" ]

                else
                    []
               )
        )
        []


viewArrowHead : Int -> EditState -> List (Svg.Attribute Msg) -> Bool -> StartPosition -> EndPosition -> Color -> Svg Msg
viewArrowHead index editState attrs showDropShadow start end strokeColor =
    let
        theta =
            (2 * pi)
                - arrowAngle start end
    in
    Svg.path
        (arrowHeadAttrs start end strokeColor
            ++ attrs
            ++ Maybe.withDefault [] (Maybe.map (moveArrowHead index start end) (EditState.ifMoving editState))
            ++ (if showDropShadow then
                    [ Attr.filter "url(#dropShadow)" ]

                else
                    []
               )
        )
        []


lineAttributes : LineType -> Shape -> List (Svg.Attribute Msg)
lineAttributes lineType shape =
    case lineType of
        Arrow ->
            strokeAttrs shape.strokeStyle shape.strokeColor ++ arrowAttributes shape

        StraightLine ->
            strokeAttrs shape.strokeStyle shape.strokeColor ++ simpleLineAttrs shape


type alias DrawingModifiers =
    { drawing : Drawing
    , constrain : Bool
    , editState : EditState
    }


viewDrawing : EditState -> DrawingModifiers -> AnnotationAttributes -> Bool -> Svg Msg
viewDrawing editState drawingModifiers annotationAttrs isInMask =
    editState
        |> EditState.viewDrawing (viewDrawingHelper drawingModifiers annotationAttrs isInMask)
        |> Maybe.withDefault (Svg.text "")


viewDrawingHelper : DrawingModifiers -> AnnotationAttributes -> Bool -> DrawingInfo -> Svg Msg
viewDrawingHelper { drawing, constrain, editState } { strokeColor, fill, strokeStyle, fontSize } isInMask { start, curPos, positions } =
    let
        lineAttrs lineType =
            lineAttributes lineType (Shape start (calcLinePos constrain start curPos) strokeColor strokeStyle)

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
            Svg.path (freeDrawAttributes (Shape start curPos strokeColor strokeStyle) positions) []

        DrawShape shapeType ->
            case shapeType of
                Rect ->
                    Svg.rect (shapeAttrs shapeType) []

                RoundedRect ->
                    Svg.rect (shapeAttrs shapeType) []

                Ellipse ->
                    Svg.ellipse (shapeAttrs shapeType) []

        DrawTextBox ->
            Svg.rect ((shapeAttributes Rect <| Shape start curPos Color.grey Annotation.defaultStroke) Nothing ++ [ Attr.strokeWidth "1" ]) []

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


viewFreeDraw : Selection -> List (Svg.Attribute Msg) -> Shape -> List Mouse.Position -> Svg Msg
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
            ++ (if selectState == Selected then
                    [ Svg.rect
                        [ Attr.x (String.fromInt (leftMostX - 5))
                        , Attr.y (String.fromInt (topMostY - 5))
                        , Attr.width (String.fromInt (10 + rightMostX - leftMostX))
                        , Attr.height (String.fromInt (10 + bottomMostY - topMostY))
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
        )


arrowPath : Shape -> String
arrowPath shape =
    let
        theta =
            (2 * pi)
                - arrowAngle shape.start shape.end

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
        ++ String.fromFloat start.x
        ++ ","
        ++ String.fromFloat start.y
        ++ "l"
        ++ String.fromFloat (4 * cos perpen)
        ++ ","
        ++ String.fromFloat (4 * sin perpen)
        ++ "c"
        ++ String.fromFloat halfWayPt
        ++ ","
        ++ String.fromFloat (dy * 0.54286)
        ++ " "
        ++ String.fromFloat arcPt
        ++ ","
        ++ String.fromFloat (dy * 0.85714)
        ++ " "
        ++ String.fromFloat (dx + (2.5 * cos perpen))
        ++ ","
        ++ String.fromFloat (dy + (2.5 * sin perpen))
        ++ "l "
        ++ String.fromFloat (-13 * cos (-theta + (pi / 2)))
        ++ ","
        ++ String.fromFloat (-13 * sin (-theta + (pi / 2)))
        ++ "c"
        ++ String.fromFloat (arcPt - dx + (2.5 * cos perpen))
        ++ ","
        ++ String.fromFloat (((dy * 0.85714) - dy) + (2.5 * sin perpen))
        ++ " "
        ++ String.fromFloat (halfWayPt - dx + (2.5 * cos perpen))
        ++ ","
        ++ String.fromFloat (((dy * 0.54286) - dy) + (2.5 * sin perpen))
        ++ " "
        ++ String.fromFloat (-dx + (2.5 * cos perpen))
        ++ ","
        ++ String.fromFloat (-dy + (2.5 * sin perpen))
        ++ "l"
        ++ String.fromFloat (4 * cos perpen)
        ++ ","
        ++ String.fromFloat (4 * sin perpen)


viewShape : List (Svg.Attribute Msg) -> ShapeType -> Maybe Color -> Shape -> Svg Msg
viewShape attrs shapeType fill shape =
    case shapeType of
        Rect ->
            Svg.rect (shapeAttributes shapeType shape fill ++ attrs) []

        RoundedRect ->
            Svg.rect (shapeAttributes shapeType shape fill ++ attrs) []

        Ellipse ->
            Svg.ellipse (shapeAttributes shapeType shape fill ++ attrs) []


viewTextArea : Int -> TextArea -> Html Msg
viewTextArea index ({ start, end, fill, fontSize, autoexpand } as textArea) =
    foreignObject []
        [ div
            [ class "text-box-container"
            , (\( a, b ) -> style a b) ( "top", toPx (-10 + Basics.min start.y end.y) )
            , (\( a, b ) -> style a b) ( "left", toPx (-20 + Basics.min start.x end.x) )
            , (\( a, b ) -> style a b) ( "width", toPx (abs (end.x - start.x)) )
            , (\( a, b ) -> style a b) ( "font-size", toPx fontSize )
            , (\( a, b ) -> style a b) ( "color", fill )
            , stopPropagationAndDefault "mousedown" (Json.succeed PreventTextMouseDown)
            ]
            [ AutoExpand.view (autoExpandConfig TextBoxInput index fontSize) autoexpand textArea.text
            ]
        ]


svgTextOffsetX : Int
svgTextOffsetX =
    textareaPadding - 20


svgTextOffsetY : Int
svgTextOffsetY =
    -20 + textareaPadding + 6


viewTextBox : List (Svg.Attribute Msg) -> Selection -> Int -> TextArea -> Svg Msg
viewTextBox attrs selectState index ({ start, end, fill, fontSize } as textBox) =
    case selectState of
        Selected ->
            viewTextArea index textBox

        NotSelected ->
            textBox.text
                |> String.split "\n"
                |> List.map (Svg.tspan [ Attr.dy <| String.fromFloat <| fontSizeToLineHeight fontSize, Attr.x <| String.fromInt <| (svgTextOffsetX + Basics.min start.x end.x), Attr.fill fill, Attr.fontSize <| String.fromInt fontSize ] << List.singleton << Svg.text)
                |> Svg.text_ ([ Attr.y <| String.fromInt <| (svgTextOffsetY + Basics.min start.y end.y), Attr.fontFamily "sans-serif" ] ++ attrs)

        SelectedWithVertices ->
            textBox.text
                |> String.split "\n"
                |> List.map (Svg.tspan [ Attr.dy <| String.fromFloat <| fontSizeToLineHeight fontSize, Attr.x <| String.fromInt <| (svgTextOffsetX + Basics.min start.x end.x), Attr.fill fill ] << List.singleton << Svg.text)
                |> Svg.text_
                    ([ Attr.y <| String.fromInt <| (svgTextOffsetY + Basics.min start.y end.y)
                     , Html.Events.onDoubleClick <| FocusTextArea index
                     , Attr.stroke <|
                        if fill == Color.black then
                            "white"

                        else
                            "black"
                     , Attr.strokeWidth "0.5px"
                     , Attr.fontSize <| String.fromInt fontSize
                     , Attr.fontFamily "sans-serif"
                     ]
                        ++ attrs
                    )


annotationConfig : Int -> AnnotationConfig Msg
annotationConfig index =
    { selectAndMove = SelectAndMoveAnnotation index
    , contextMenu = ToggleSelectedAnnotationMenu index
    , startMoving = StartMovingAnnotation index
    , finishMoving = FinishMovingAnnotation
    }


viewPixelate : EditState -> Int -> Annotation -> Maybe (List (Svg Msg))
viewPixelate editState index annotation =
    case annotation of
        Pixelate start end ->
            Just [ Svg.rect (EditState.annotationEvents (annotationConfig index) index editState ++ rectAttrs start end ++ [ Attr.fill Color.black ]) [] ]

        _ ->
            Nothing


viewAnnotation : EditState -> Int -> Annotation -> ( Svg Msg, Maybe (Svg Msg) )
viewAnnotation editState index annotation =
    let
        selectState =
            EditState.selectState index (not (isFreeHand annotation)) editState

        editStateAttrs =
            EditState.annotationEvents (annotationConfig index) index editState

        toVertexEvents =
            EditState.vertexEvents (StartResizingAnnotation index) editState

        vertices verticesType { start, end } =
            Vertices.viewVertices verticesType start end toVertexEvents selectState
    in
    case annotation of
        Lines lineType shape ->
            ( viewLine index editState editStateAttrs lineType shape
            , vertices Linear shape
            )

        FreeDraw shape positions ->
            ( viewFreeDraw selectState editStateAttrs shape positions
            , Nothing
            )

        Shapes shapeType fill shape ->
            ( viewShape editStateAttrs shapeType fill shape
            , vertices Rectangular shape
            )

        TextBox textBox ->
            ( viewTextBox editStateAttrs selectState index textBox
            , Nothing
            )

        Spotlight shapeType shape ->
            ( viewShape editStateAttrs shapeType Nothing shape
            , vertices Rectangular shape
            )

        Pixelate start end ->
            ( Svg.rect (rectAttrs start end ++ [ Attr.fill "none", Attr.style "pointer-events: all;" ] ++ editStateAttrs) []
            , vertices Rectangular { start = start, end = end }
            )


viewSpotlightInMask : EditState -> ( Int, Annotation ) -> Maybe (Svg Msg)
viewSpotlightInMask editState annotationById =
    spotlightToMaskCutout annotationById
        |> Maybe.map (viewSpotlightInMaskHelper editState)


viewSpotlightInMaskHelper : EditState -> ( Int, ShapeType, Shape ) -> Svg Msg
viewSpotlightInMaskHelper editState ( index, shapeType, shape ) =
    viewShape (EditState.annotationEvents (annotationConfig index) index editState) shapeType (Just Color.black) shape


spotlightToMaskCutout : ( Int, Annotation ) -> Maybe ( Int, ShapeType, Shape )
spotlightToMaskCutout ( index, annotation ) =
    case annotation of
        Spotlight shapeType shape ->
            Just ( index, shapeType, shape )

        _ ->
            Nothing


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
            String.trim textArea.text == ""

        _ ->
            False


startAndEnd : Annotation -> ( Mouse.Position, Mouse.Position )
startAndEnd annotation =
    case annotation of
        Lines lineType line ->
            ( line.start, line.end )

        FreeDraw shape _ ->
            ( shape.start, shape.end )

        Shapes shapeType _ shape ->
            ( shape.start, shape.end )

        TextBox textArea ->
            ( textArea.start, textArea.end )

        Spotlight shapeType shape ->
            ( shape.start, shape.end )

        Pixelate start end ->
            ( start, end )


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
            ( String.fromInt strokeWidth, "" )

        SolidMedium ->
            ( String.fromInt strokeWidth, "" )

        SolidThick ->
            ( String.fromInt strokeWidth, "" )

        SolidVeryThick ->
            ( String.fromInt strokeWidth, "" )

        DashedThin ->
            ( String.fromInt strokeWidth, "10, 5" )

        DashedMedium ->
            ( String.fromInt strokeWidth, "10, 5" )

        DashedThick ->
            ( String.fromInt strokeWidth, "10, 5" )

        DashedVeryThick ->
            ( String.fromInt strokeWidth, "10, 5" )


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


arrowAngle : Mouse.Position -> Mouse.Position -> Float
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

        FreeDraw shape points ->
            FreeDraw { shape | strokeColor = strokeColor } points

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

        FreeDraw shape points ->
            FreeDraw { shape | strokeStyle = strokeStyle } points

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
    -> { a | start : Mouse.Position, end : Mouse.Position }
    -> { a | end : Mouse.Position, start : Mouse.Position }
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

        FreeDraw shape points ->
            FreeDraw (shift translate shape) (List.map (shiftPosition (Tuple.first translate) (Tuple.second translate)) points)

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


resizeVertices :
    (StartPosition -> EndPosition -> Mouse.Position)
    -> ResizingInfo
    -> { a | start : Mouse.Position, end : Mouse.Position }
    -> { a | start : Mouse.Position, end : Mouse.Position }
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
            { annotation | start = constrain annotation.end curPos, end = Mouse.Position start.x end.y }

        StartPlusY ->
            { annotation | start = constrain annotation.end curPos, end = Mouse.Position end.x start.y }


shiftForPaste : Annotation -> Annotation
shiftForPaste annotation =
    case annotation of
        Lines lineType line ->
            Lines lineType { line | start = shiftPosition 10 10 line.start, end = shiftPosition 10 10 line.end }

        FreeDraw shape points ->
            FreeDraw { shape | start = shiftPosition 10 10 shape.start, end = shiftPosition 10 10 shape.end } (List.map (shiftPosition 10 10) points)

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


calcShapePos : Bool -> Mouse.Position -> Mouse.Position -> Mouse.Position
calcShapePos shiftPressed start end =
    if shiftPressed then
        equalXandY start end

    else
        end


calcLinePos : Bool -> Mouse.Position -> Mouse.Position -> Mouse.Position
calcLinePos shiftPressed start end =
    if shiftPressed then
        stepMouse start end

    else
        end


equalXandY : Mouse.Position -> Mouse.Position -> Mouse.Position
equalXandY a b =
    if b.y - a.y < 0 then
        Mouse.Position b.x (a.y - Basics.max (b.x - a.x) (a.x - b.x))

    else
        Mouse.Position b.x (a.y + Basics.max (b.x - a.x) (a.x - b.x))


stepMouse : Mouse.Position -> Mouse.Position -> Mouse.Position
stepMouse start curPos =
    arrowAngle start curPos
        / (pi / 4)
        |> round
        |> toFloat
        |> (*) (pi / 4)
        |> toDeltas (calcDistance start curPos)
        |> shiftPosition start.x start.y


toDeltas : Float -> Float -> Mouse.Position
toDeltas h theta =
    Mouse.Position (round (cos theta * h)) (round (sin theta * h))


calcDistance : Mouse.Position -> Mouse.Position -> Float
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


textareaPadding =
    2
