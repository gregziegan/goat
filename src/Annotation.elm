module Annotation exposing
    ( Annotation
    , ArrowAttributes
    , Choice(..)
    , Config
    , Def(..)
    , Id
    , Shape
    , calcLinePos
    , calcShapePos
    , configure
    , defaultStyles
    , hasNoText
    , init
    , isSpotlight
    , lineAttributes
    , move
    , rectAttrs
    , resizeFn
    , startAndEnd
    , trackPosition
    , updateTextArea
    , view
    , viewDef
    , withVertices
    )

import Annotation.Options exposing (AnnotationStyles, Fill, FontSize, StrokeColor, StrokeStyle(..))
import Annotation.Vertices as Vertices exposing (Vertex)
import AutoExpand
import Color exposing (Color)
import Environment exposing (OperatingSystem(..))
import EventUtils exposing (stopPropagationAndDefault)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Html.Events
import Icons
import Json.Decode as Decode
import List.Extra
import Palette
import Position exposing (EndPosition, Position, StartPosition)
import Svg exposing (Svg, foreignObject)
import Svg.Attributes as Attr exposing (d, fill, filter, stroke, transform)
import Utils exposing (toPx)


type alias Id =
    Int


type alias TextBoxConfig msg =
    { id : Id
    , onInput : { state : AutoExpand.State, textValue : String } -> msg
    , onFocus : msg
    , fontSize : FontSize
    }


type alias Annotation =
    { id : Id
    , start : Position
    , end : Position
    , positions : List Position
    , text : String
    , angle : Float
    , autoExpand : AutoExpand.State
    , choice : Choice
    , styles : AnnotationStyles
    }


type alias Shape =
    { start : Position
    , end : Position
    , strokeColor : StrokeColor
    , strokeStyle : StrokeStyle
    }


type Config msg
    = Config
        { snap : Bool
        , styles : AnnotationStyles
        , textBox : TextBoxConfig msg
        , eventsForVertex : Maybe (Vertex -> List (Svg.Attribute msg)) -- an annotation does not always show vertices
        }


type Choice
    = Arrow
    | StraightLine
    | FreeHand
    | RoundedRectangle
    | Rectangle
    | Ellipse
    | SpotlightRoundedRectangle
    | SpotlightRectangle
    | SpotlightEllipse
    | Pixelate
    | TextBox


init :
    { id : Id
    , choice : Choice
    , start : StartPosition
    , end : EndPosition
    , positions : List Position
    , styles : AnnotationStyles
    , onInput : { state : AutoExpand.State, textValue : String } -> msg
    , onFocus : msg
    }
    -> Annotation
init { id, start, end, positions, onInput, onFocus, choice, styles } =
    { id = id
    , start = start
    , end = end
    , positions = positions
    , choice = choice
    , autoExpand =
        AutoExpand.initState
            (autoExpandConfig
                { id = id
                , onInput = onInput
                , onFocus = onFocus
                , fontSize = styles.fontSize
                }
            )
    , angle = 0
    , text = "Text"
    , styles = styles
    }


configure :
    { id : Int
    , onInput : { state : AutoExpand.State, textValue : String } -> msg
    , onFocus : msg
    , snap : Bool
    , styles : AnnotationStyles
    , eventsForVertex : Maybe (Vertex -> List (Svg.Attribute msg))
    }
    -> Config msg
configure { id, onInput, onFocus, snap, styles, eventsForVertex } =
    Config
        { snap = snap
        , styles = styles
        , textBox =
            { id = id
            , onInput = onInput
            , onFocus = onFocus
            , fontSize = styles.fontSize
            }
        , eventsForVertex = eventsForVertex
        }


withVertices : (Vertex -> List (Svg.Attribute msg)) -> Config msg -> Config msg
withVertices eventsForVertex (Config config) =
    Config { config | eventsForVertex = Just eventsForVertex }


trackPosition : Position -> Annotation -> Annotation
trackPosition position drawing =
    { drawing
        | positions =
            case drawing.positions of
                [] ->
                    [ position ]

                lastPos :: _ ->
                    if abs (lastPos.x - position.x) < 10 && abs (lastPos.y - position.y) < 10 then
                        drawing.positions

                    else
                        position :: drawing.positions
        , end = position
    }


defaultStyles : AnnotationStyles
defaultStyles =
    AnnotationStyles Palette.purple Nothing SolidMedium 20


isSpotlight : Choice -> Bool
isSpotlight choice =
    case choice of
        SpotlightRectangle ->
            True

        SpotlightRoundedRectangle ->
            True

        SpotlightEllipse ->
            True

        _ ->
            False


hasNoText : Annotation -> Bool
hasNoText annotation =
    annotation.text == ""



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


rectAttributes : Shape -> Fill -> List (Svg.Attribute msg)
rectAttributes shape fill =
    fillAttrs fill
        ++ strokeAttrs shape.strokeStyle shape.strokeColor
        ++ rectAttrs shape.start shape.end
        ++ [ Attr.strokeLinejoin "round" ]


roundedRectAttributes : Shape -> Fill -> List (Svg.Attribute msg)
roundedRectAttributes shape fill =
    fillAttrs fill
        ++ strokeAttrs shape.strokeStyle shape.strokeColor
        ++ rectAttrs shape.start shape.end
        ++ [ Attr.rx "15", Attr.ry "15" ]


ellipseAttributes : Shape -> Fill -> List (Svg.Attribute msg)
ellipseAttributes shape fill =
    fillAttrs fill
        ++ strokeAttrs shape.strokeStyle shape.strokeColor
        ++ ellipseAttrs shape


textAreaAttributes : Shape -> List (Svg.Attribute msg)
textAreaAttributes shape =
    strokeAttrs shape.strokeStyle shape.strokeColor ++ [ Attr.strokeWidth "1" ]


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


lineAttributes : Shape -> List (Svg.Attribute msg)
lineAttributes shape =
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


freeDrawPathHelper : List Position -> String -> String
freeDrawPathHelper positions pathString =
    case positions of
        [] ->
            pathString

        lastPos :: [] ->
            pathString ++ " L " ++ Position.toString lastPos

        pos :: nextPos :: rest ->
            freeDrawPathHelper rest (pathString ++ " S " ++ Position.toString pos ++ " " ++ Position.toString nextPos)


ellipseAttrs : Shape -> List (Svg.Attribute msg)
ellipseAttrs { start, end } =
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
    strokeAttrs shape.strokeStyle shape.strokeColor
        ++ [ Attr.stroke "none"
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
        [ viewArrowHead True arrow
        , viewArrowBody arrow.bodyAttributes
        , viewArrowHead False arrow
        ]


viewArrowBody bodyAttributes =
    Svg.path bodyAttributes []


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



-- VIEW


arrowHeadPath : Position -> String
arrowHeadPath pos =
    "M"
        ++ String.fromFloat (toFloat pos.x - 20.6667)
        ++ ","
        ++ String.fromFloat (toFloat pos.y - 2.8)
        ++ "l-4.62033,-10.72559l 25.66667, 13.66667l -25.66667, 13.66667l4.62033, -10.33667z"


calcLinePos : Bool -> Position -> Position -> Position
calcLinePos shiftPressed start end =
    if shiftPressed then
        Position.snap start end

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


move : ( Int, Int ) -> Annotation -> Annotation
move ( dx, dy ) annotation =
    { annotation
        | start = Position.shift dx dy annotation.start
        , end = Position.shift dx dy annotation.end
    }


resizeFn : Config msg -> Annotation -> (StartPosition -> EndPosition -> Position)
resizeFn (Config config) annotation =
    case annotation.choice of
        Arrow ->
            calcLinePos config.snap

        StraightLine ->
            calcLinePos config.snap

        FreeHand ->
            calcLinePos False

        Rectangle ->
            calcShapePos config.snap

        RoundedRectangle ->
            calcShapePos config.snap

        Ellipse ->
            calcShapePos config.snap

        TextBox ->
            calcShapePos False

        SpotlightRectangle ->
            calcShapePos config.snap

        SpotlightRoundedRectangle ->
            calcShapePos config.snap

        SpotlightEllipse ->
            calcShapePos config.snap

        Pixelate ->
            calcShapePos config.snap


updateTextArea : AutoExpand.State -> String -> Annotation -> Annotation
updateTextArea autoExpand textValue annotation =
    { annotation | autoExpand = autoExpand, text = textValue }


startAndEnd : Annotation -> ( StartPosition, EndPosition )
startAndEnd { start, end } =
    ( start, end )


autoExpandConfig : TextBoxConfig msg -> AutoExpand.Config msg
autoExpandConfig ({ id, onInput, fontSize } as config) =
    AutoExpand.config
        { onInput = onInput
        , padding = textareaPadding
        , minRows = 1
        , maxRows = 4
        , lineHeight = fontSizeToLineHeight fontSize
        }
        |> AutoExpand.withAttribute (Html.Attributes.id ("text-box-edit--" ++ String.fromInt id))
        |> AutoExpand.withAttribute (Html.Attributes.class "text-box-textarea")


fontSizeToLineHeight : Int -> Float
fontSizeToLineHeight fontSize =
    toFloat fontSize * 1.2


textareaPadding : number
textareaPadding =
    2


arrowConfig : Shape -> List (Svg.Attribute msg) -> List (Svg.Attribute msg) -> ArrowAttributes msg
arrowConfig shape attrs head =
    { headAttributes = head
    , bodyAttributes = arrowAttributes shape ++ attrs
    , start = shape.start
    , end = shape.end
    , strokeColor = shape.strokeColor
    }


arrowBody : StartPosition -> EndPosition -> StrokeColor -> Shape
arrowBody start end strokeColor =
    Shape start end strokeColor SolidMedium


spotlightFill : Fill
spotlightFill =
    Just Palette.black


view : List (Svg.Attribute msg) -> Config msg -> Annotation -> Svg msg
view attrs ((Config { snap, styles, eventsForVertex }) as config) ({ start, end, positions, choice } as annotation) =
    let
        { strokeColor, strokeStyle, fill } =
            styles

        line =
            Shape start (calcLinePos snap start end) strokeColor strokeStyle

        shape =
            Shape start (calcShapePos snap start end) strokeColor strokeStyle

        groupWithVertices toVertices viewAnnotation =
            Svg.g []
                [ case ( choice, eventsForVertex ) of
                    ( TextBox, Just _ ) ->
                        viewText attrs annotation

                    ( _, _ ) ->
                        viewAnnotation
                , case eventsForVertex of
                    Just toEvents ->
                        toVertices toEvents annotation

                    Nothing ->
                        Svg.text ""
                ]
    in
    case choice of
        Arrow ->
            arrowConfig (arrowBody start (calcLinePos snap start end) strokeColor) (arrowAttributes shape ++ attrs) attrs
                |> viewArrow
                |> groupWithVertices linearVertices

        StraightLine ->
            Svg.path (lineAttributes line ++ attrs) []
                |> groupWithVertices linearVertices

        FreeHand ->
            Svg.g attrs
                [ viewFreeDraw line positions
                , case eventsForVertex of
                    Just _ ->
                        freeHandSelection positions

                    Nothing ->
                        Svg.text ""
                ]

        Rectangle ->
            Svg.rect (rectAttributes shape fill ++ attrs) []
                |> groupWithVertices rectangularVertices

        RoundedRectangle ->
            Svg.rect (roundedRectAttributes shape fill ++ attrs) []
                |> groupWithVertices rectangularVertices

        Ellipse ->
            Svg.ellipse (ellipseAttributes shape fill ++ attrs) []
                |> groupWithVertices rectangularVertices

        TextBox ->
            viewTextArea config annotation
                |> groupWithVertices rectangularVertices

        SpotlightRectangle ->
            Svg.rect (rectAttributes shape fill ++ attrs) []
                |> groupWithVertices rectangularVertices

        SpotlightRoundedRectangle ->
            Svg.rect (roundedRectAttributes shape fill ++ attrs) []
                |> groupWithVertices rectangularVertices

        SpotlightEllipse ->
            Svg.ellipse (ellipseAttributes shape fill ++ attrs) []
                |> groupWithVertices rectangularVertices

        Pixelate ->
            Svg.rect (rectAttributes (Shape start end Palette.white SolidThin) Nothing ++ attrs) []
                |> groupWithVertices rectangularVertices


type Def msg
    = PixelateCutout (Svg msg)
    | SpotlightCutout (Svg msg)
    | Empty


viewDef : Config msg -> Annotation -> Def msg
viewDef (Config { snap, styles, eventsForVertex }) ({ start, end, positions, choice } as annotation) =
    let
        shape =
            Shape start (calcShapePos snap start end) styles.strokeColor styles.strokeStyle
    in
    case choice of
        Arrow ->
            Empty

        StraightLine ->
            Empty

        FreeHand ->
            Empty

        Rectangle ->
            Empty

        RoundedRectangle ->
            Empty

        Ellipse ->
            Empty

        TextBox ->
            Empty

        SpotlightRectangle ->
            SpotlightCutout <| Svg.rect (rectAttributes shape spotlightFill) []

        SpotlightRoundedRectangle ->
            SpotlightCutout <| Svg.rect (roundedRectAttributes shape spotlightFill) []

        SpotlightEllipse ->
            SpotlightCutout <| Svg.ellipse (ellipseAttributes shape spotlightFill) []

        Pixelate ->
            PixelateCutout <| Svg.rect (rectAttributes shape spotlightFill) []


viewFreeDraw : Shape -> List Position -> Svg msg
viewFreeDraw shape positions =
    Svg.path (freeDrawAttributes shape positions) []


rectangularVertices : (Vertex -> List (Svg.Attribute msg)) -> Annotation -> Svg msg
rectangularVertices eventsForVertex { start, end } =
    Vertices.view { kind = Vertices.Rectangular, start = start, end = end, eventsForVertex = eventsForVertex }


linearVertices : (Vertex -> List (Svg.Attribute msg)) -> Annotation -> Svg msg
linearVertices eventsForVertex { start, end } =
    Vertices.view { kind = Vertices.Linear, start = start, end = end, eventsForVertex = eventsForVertex }


freeHandSelection : List Position -> Svg msg
freeHandSelection positions =
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
    Svg.rect
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


viewText : List (Svg.Attribute msg) -> Annotation -> Svg msg
viewText attrs annotation =
    annotation.text
        |> String.split "\n"
        |> List.map (toTSpan annotation)
        |> Svg.text_
            ([ Attr.y <| String.fromInt <| (svgTextOffsetY + Basics.min annotation.start.y annotation.end.y)
             , Attr.fontFamily "sans-serif"
             ]
                ++ attrs
            )


viewTextArea : Config msg -> Annotation -> Html msg
viewTextArea (Config { styles, textBox }) { start, end, autoExpand, text } =
    foreignObject
        [ Attr.y (toPx (-10 + Basics.min start.y end.y))
        , Attr.x (toPx (-20 + Basics.min start.x end.x))
        , Attr.width "100%"
        , Attr.height "100%"
        ]
        [ div
            [ class "text-box-container"
            , style "font-size" (toPx styles.fontSize)
            , style "color" (Color.toHexString styles.strokeColor)

            -- , stopPropagationAndDefault "mousedown" (Decode.fail "blah")
            ]
            [ AutoExpand.view (autoExpandConfig textBox) autoExpand text
            ]
        ]


svgTextOffsetX : Int
svgTextOffsetX =
    textareaPadding - 20


svgTextOffsetY : Int
svgTextOffsetY =
    -20 + textareaPadding + 6


toTSpan : Annotation -> String -> Svg msg
toTSpan { start, end, styles } spanText =
    Svg.tspan
        [ Attr.dy <| String.fromFloat <| fontSizeToLineHeight styles.fontSize
        , Attr.x <| String.fromInt <| (svgTextOffsetX + Basics.min start.x end.x)
        , Attr.fill (Color.toHexString (Maybe.withDefault Palette.black styles.fill))
        , Attr.fontSize <| String.fromInt styles.fontSize
        ]
        [ Svg.text spanText ]


viewTextBoxWithVertices : Config msg -> List (Svg.Attribute msg) -> Annotation -> Svg msg
viewTextBoxWithVertices (Config config) attrs ({ start, end, text } as annotation) =
    text
        |> String.split "\n"
        |> List.map (toTSpan annotation)
        |> Svg.text_
            ([ Attr.y <| String.fromInt <| (svgTextOffsetY + Basics.min start.y end.y)
             , Html.Events.onDoubleClick <| config.textBox.onFocus
             , Attr.stroke <|
                if config.styles.fill == Just Palette.black then
                    "white"

                else
                    "black"
             , Attr.strokeWidth "0.5px"
             , Attr.fontSize <| String.fromInt config.styles.fontSize
             , Attr.fontFamily "sans-serif"
             ]
                ++ attrs
            )


-- translateArrowHead : Int -> StartPosition -> EndPosition -> MovingInfo -> List (Svg.Attribute Msg)
-- translateArrowHead index start end { translate } =
--     let
--         theta =
--             (2 * pi)
--                 - Position.angle start end
--         ( dx, dy ) =
--             translate
--     in
--     if index == id then
--         [ Attr.transform
--             ("translate("
--                 ++ String.fromInt dx
--                 ++ ","
--                 ++ String.fromInt dy
--                 ++ ") rotate("
--                 ++ String.fromFloat (-theta * (180 / pi))
--                 ++ " "
--                 ++ String.fromInt end.x
--                 ++ " "
--                 ++ String.fromInt end.y
--                 ++ ")"
--             )
--         ]
--     else
--         []