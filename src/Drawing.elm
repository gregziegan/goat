module Drawing exposing (ArrowAttributes, AttributeDropdown(..), Drawing(..), LineType(..), Shape, ShapeType(..), calcLinePos, calcShapePos, equal, freeDrawAttributes, icon, isSpotlight, lineAttributes, rectAttrs, shapeAttributes, shapes, spotlights, toString, viewArrow, viewFreeDraw)

import Color exposing (Color)
import Drawing.Options exposing (Fill, StrokeColor, StrokeStyle(..))
import Environment exposing (OperatingSystem(..))
import Icons
import Position exposing (EndPosition, Position, StartPosition, calcDistance)
import Svg exposing (Svg)
import Svg.Attributes as Attr exposing (d, fill, filter, stroke, transform)


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
    , strokeColor : StrokeColor
    , strokeStyle : StrokeStyle
    }


type Drawing
    = DrawLine LineType
    | DrawFreeHand
    | DrawShape ShapeType
    | DrawTextBox
    | DrawSpotlight ShapeType
    | DrawPixelate


type AttributeDropdown
    = ShapesDropdown
    | SpotlightsDropdown
    | Fonts
    | Fills
    | StrokeColors
    | Strokes


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


isSpotlight : Drawing -> Bool
isSpotlight drawing =
    case drawing of
        DrawSpotlight _ ->
            True

        _ ->
            False


equal : Drawing -> Drawing -> Bool
equal drawing drawing2 =
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


icon : Drawing -> Svg msg
icon drawing =
    case drawing of
        DrawLine lineType ->
            case lineType of
                StraightLine ->
                    Icons.viewLine

                Arrow ->
                    Icons.viewArrow

        DrawFreeHand ->
            Icons.freeHand

        DrawShape shapeType ->
            case shapeType of
                Rect ->
                    Icons.viewRectangle

                RoundedRect ->
                    Icons.viewRoundedRectangle

                Ellipse ->
                    Icons.viewEllipse

        DrawTextBox ->
            Icons.viewText

        DrawSpotlight shapeType ->
            case shapeType of
                Rect ->
                    Icons.viewSpotlightRect

                RoundedRect ->
                    Icons.viewSpotlightRoundedRect

                Ellipse ->
                    Icons.viewSpotlightEllipse

        DrawPixelate ->
            Icons.viewPixelate


toString : OperatingSystem -> Drawing -> String
toString os drawing =
    case os of
        MacOS ->
            macDrawingToString drawing

        Windows ->
            windowsDrawingToString drawing


windowsDrawingToString : Drawing -> String
windowsDrawingToString drawing =
    case drawing of
        DrawLine lineType ->
            case lineType of
                Arrow ->
                    "A̲rrow"

                StraightLine ->
                    "L̲ine"

        DrawFreeHand ->
            "Free H̲and"

        DrawShape shapeType ->
            case shapeType of
                Rect ->
                    "R̲ectangle"

                RoundedRect ->
                    "Ro̲unded Rectangle"

                Ellipse ->
                    "E̲llipse"

        DrawTextBox ->
            "T̲ext"

        DrawSpotlight shapeType ->
            case shapeType of
                Rect ->
                    "Spotlig̲ht Rectangle"

                RoundedRect ->
                    "Spotlight Rounded Rec̲tangle"

                Ellipse ->
                    "Spotlight Elli̲pse"

        DrawPixelate ->
            "P̲ixelate"


macDrawingToString : Drawing -> String
macDrawingToString drawing =
    case drawing of
        DrawLine lineType ->
            case lineType of
                Arrow ->
                    "Arrow (A)"

                StraightLine ->
                    "Line (L)"

        DrawFreeHand ->
            "Free Hand (H)"

        DrawShape shapeType ->
            case shapeType of
                Rect ->
                    "Rectangle (R)"

                RoundedRect ->
                    "Rounded Rectangle (O)"

                Ellipse ->
                    "Ellipse (E)"

        DrawTextBox ->
            "Text (T)"

        DrawSpotlight shapeType ->
            case shapeType of
                Rect ->
                    "Spotlight Rectangle (G)"

                RoundedRect ->
                    "Spotlight Rounded Rectangle (C)"

                Ellipse ->
                    "Spotlight Ellipse (I)"

        DrawPixelate ->
            "Pixelate (P)"



-- VIEW


fillAttrs : Fill -> List (Svg.Attribute msg)
fillAttrs fill =
    case fill of
        Just color ->
            [ Attr.fill <| Color.toHexString color
            , Attr.pointerEvents "auto"
            ]

        Nothing ->
            [ Attr.fillOpacity "0"
            , Attr.pointerEvents "visibleStroke"
            ]


shapeAttributes : ShapeType -> Shape -> Fill -> List (Svg.Attribute msg)
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


strokeAttrs : StrokeStyle -> Color -> List (Svg.Attribute msg)
strokeAttrs strokeStyle strokeColor =
    let
        ( strokeWidth, dashArray ) =
            toLineStyle strokeStyle
    in
    [ Attr.stroke <| Color.toHexString strokeColor
    , Attr.strokeWidth strokeWidth
    , Attr.strokeDasharray dashArray
    ]


lineAttributes : LineType -> Shape -> List (Svg.Attribute msg)
lineAttributes lineType shape =
    case lineType of
        Arrow ->
            strokeAttrs shape.strokeStyle shape.strokeColor ++ arrowAttributes shape

        StraightLine ->
            strokeAttrs shape.strokeStyle shape.strokeColor ++ simpleLineAttrs shape


rectAttrs : StartPosition -> EndPosition -> List (Svg.Attribute msg)
rectAttrs start end =
    [ Attr.width <| String.fromInt <| abs <| end.x - start.x
    , Attr.height <| String.fromInt <| abs <| end.y - start.y
    , Attr.x <| String.fromInt <| Basics.min start.x end.x
    , Attr.y <| String.fromInt <| Basics.min start.y end.y
    , Attr.filter "url(#dropShadow)"
    ]


freeDrawPath : StartPosition -> List Position -> String
freeDrawPath start positions =
    freeDrawPathHelper positions ("M " ++ Position.toString start)


freeDrawAttributes : Shape -> List Position -> List (Svg.Attribute msg)
freeDrawAttributes shape positions =
    [ Attr.d (freeDrawPath shape.start (List.reverse (shape.end :: positions)))
    , Attr.fill "none"
    , Attr.strokeLinejoin "round"
    ]
        ++ strokeAttrs shape.strokeStyle shape.strokeColor


viewFreeDraw : Shape -> List Position -> List (Svg.Attribute msg) -> Svg msg
viewFreeDraw shape positions attrs =
    Svg.path (freeDrawAttributes shape positions) []


freeDrawPathHelper : List Position -> String -> String
freeDrawPathHelper positions pathString =
    case positions of
        [] ->
            pathString

        lastPos :: [] ->
            pathString ++ " L " ++ Position.toString lastPos

        pos :: nextPos :: rest ->
            freeDrawPathHelper rest (pathString ++ " S " ++ Position.toString pos ++ " " ++ Position.toString nextPos)


ellipseAttributes : Shape -> List (Svg.Attribute msg)
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


simpleLineAttrs : Shape -> List (Svg.Attribute msg)
simpleLineAttrs { start, end, strokeColor, strokeStyle } =
    [ Attr.stroke "none"
    , Attr.fill <| Color.toHexString strokeColor
    , Attr.d <| linePath (toStrokeWidth strokeStyle) start end
    , Attr.filter "url(#dropShadow)"
    ]


arrowAttributes : Shape -> List (Svg.Attribute msg)
arrowAttributes shape =
    [ Attr.stroke "none"
    , Attr.fill <| Color.toHexString shape.strokeColor
    , Attr.d (arrowPath shape)
    , Attr.filter "url(#dropShadow)"
    ]


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


linePath : Float -> StartPosition -> EndPosition -> String
linePath strokeWidth start end =
    let
        theta =
            (2 * pi)
                - Position.angle start end

        perpen =
            (pi / 2) - theta

        dx =
            toFloat (end.x - start.x)

        dy =
            toFloat (end.y - start.y)

        startFloat =
            { x = toFloat start.x, y = toFloat start.y }

        ccPt1 =
            Position.shift ((strokeWidth / 2) * cos perpen) ((strokeWidth / 2) * sin perpen) startFloat

        ccPt2 =
            Position.shift dx dy ccPt1

        ccPt3 =
            Position.shift (-strokeWidth * cos perpen) (-strokeWidth * sin perpen) ccPt2

        ccPt4 =
            Position.shift -dx -dy ccPt3
    in
    "M"
        ++ Position.toString start
        ++ " L"
        ++ posToString ccPt1
        ++ " L"
        ++ posToString ccPt2
        ++ " L"
        ++ posToString ccPt3
        ++ " L"
        ++ posToString ccPt4
        ++ "Z"


posToString : { x : Float, y : Float } -> String
posToString { x, y } =
    String.fromFloat x ++ "," ++ String.fromFloat y


arrowPath : Shape -> String
arrowPath shape =
    let
        theta =
            (2 * pi)
                - Position.angle shape.start shape.end

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


arrowHeadAttrs : ArrowAttributes msg -> List (Svg.Attribute msg)
arrowHeadAttrs { start, end, strokeColor } =
    let
        theta =
            (2 * pi)
                - Position.angle start end
    in
    [ d (arrowHeadPath end)
    , fill <| Color.toHexString strokeColor
    , stroke "none"
    , transform ("rotate(" ++ String.fromFloat (-theta * (180 / pi)) ++ " " ++ String.fromInt end.x ++ " " ++ String.fromInt end.y ++ ")")
    ]


type alias ArrowAttributes msg =
    { headAttributes : List (Svg.Attribute msg)
    , bodyAttributes : List (Svg.Attribute msg)
    , start : Position
    , end : Position
    , strokeColor : Color
    }


viewArrow : ArrowAttributes msg -> Svg msg
viewArrow arrow =
    Svg.g []
        [ viewArrowHeadDrawing True arrow
        , viewArrowBody arrow.bodyAttributes
        , viewArrowHeadDrawing False arrow
        ]


viewArrowBody bodyAttributes =
    Svg.path bodyAttributes []


viewArrowHeadDrawing : Bool -> ArrowAttributes msg -> Svg msg
viewArrowHeadDrawing showDropShadow arrow =
    Svg.path
        (arrowHeadAttrs arrow
            ++ arrow.headAttributes
            ++ (if showDropShadow then
                    [ filter "url(#dropShadow)" ]

                else
                    []
               )
        )
        []



-- VIEW


arrowHeadPath : Position -> String
arrowHeadPath pos =
    "M"
        ++ String.fromFloat (toFloat pos.x - 20.6667)
        ++ ","
        ++ String.fromFloat (toFloat pos.y - 2.8)
        ++ "l-4.62033,-10.72559l 25.66667, 13.66667l -25.66667, 13.66667l4.62033, -10.33667z"


viewArrowHead : Bool -> ArrowAttributes msg -> Svg msg
viewArrowHead showDropShadow arrow =
    Svg.path
        (arrowHeadAttrs arrow
            ++ arrow.headAttributes
            ++ (if showDropShadow then
                    [ filter "url(#dropShadow)" ]

                else
                    []
               )
        )
        []


calcLinePos : Bool -> Position -> Position -> Position
calcLinePos shiftPressed start end =
    if shiftPressed then
        Position.step start end

    else
        end


calcShapePos : Bool -> Position -> Position -> Position
calcShapePos shiftPressed start end =
    if shiftPressed then
        equalXandY start end

    else
        end


equalXandY : Position -> Position -> Position
equalXandY a b =
    if b.y - a.y < 0 then
        Position b.x (a.y - Basics.max (b.x - a.x) (a.x - b.x))

    else
        Position b.x (a.y + Basics.max (b.x - a.x) (a.x - b.x))
