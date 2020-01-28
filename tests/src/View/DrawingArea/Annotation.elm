module DrawingArea.Annotation exposing (all)

import Expect exposing (Expectation)
import Fixtures exposing (aShape, aTextArea, end, goat, model, start, testColor)
import Annotation exposing (Annotation(..), LineType(..), Shape, ShapeType(..), TextArea, arrowAngle, fontSizeToLineHeight, textareaPadding, toLineStyle)
import EditState as EditState
import Model exposing (Image)

import DrawingArea exposing (viewImage, viewPixelatedImage)
import DrawingArea.Annotation exposing (arrowHeadPath, arrowPath, linePath, viewAnnotation)
import Html exposing (Html)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as HtmlSelector exposing (Selector, all, attribute, class, tag, text)
import TestUtil exposing (svgDrawspace)


lineSelector : Shape -> List Selector
lineSelector shape =
    [ attribute "stroke" "none"
    , attribute "fill" (Color.Convert.colorToHex shape.strokeColor)
    , attribute "d" (linePath 4 shape.start shape.end)
    ]


arrowSelector : Shape -> List Selector
arrowSelector shape =
    [ attribute "stroke" "none"
    , attribute "fill" (Color.Convert.colorToHex shape.strokeColor)
    , attribute "d" (arrowPath shape)
    , attribute "filter" "url(#dropShadow)"
    ]


arrowHeadSelector : ( Int, Int ) -> Shape -> List Selector
arrowHeadSelector ( dx, dy ) { start, end, strokeColor } =
    let
        theta =
            (2 * pi)
                - arrowAngle start end
    in
    [ attribute "d" (arrowHeadPath end)
    , attribute "fill" <| Color.Convert.colorToHex strokeColor
    , attribute "stroke" "none"
    , attribute "transform" ("rotate(" ++ toString (-theta * (180 / pi)) ++ " " ++ toString end.x ++ " " ++ toString end.y ++ ")")
    , attribute "filter" "url(#dropShadow)"
    ]


strokeSelectors : String -> String -> Color -> List Selector
strokeSelectors strokeWidth dashArray strokeColor =
    [ attribute "stroke" <| Color.Convert.colorToHex strokeColor
    , attribute "stroke-width" strokeWidth
    , attribute "stroke-dasharray" dashArray
    ]


ellipseSelector : Shape -> List Selector
ellipseSelector shape =
    [ attribute "rx" <| toString <| abs <| (end.x - start.x) // 2
    , attribute "ry" <| toString <| abs <| (end.y - start.y) // 2
    , attribute "cx" <| toString <| start.x + ((end.x - start.x) // 2)
    , attribute "cy" <| toString <| start.y + ((end.y - start.y) // 2)
    , attribute "filter" "url(#dropShadow)"
    ]
        ++ uncurry strokeSelectors (toLineStyle shape.strokeStyle) shape.strokeColor


roundedRectSelector : Shape -> List Selector
roundedRectSelector shape =
    [ attribute "rx" "15"
    , attribute "ry" "15"
    ]
        ++ rectSelector shape


fillSelectors : Maybe Color -> List Selector
fillSelectors fill =
    case fill of
        Just fillColor ->
            [ attribute "fill" (Color.Convert.colorToHex fillColor) ]

        Nothing ->
            [ attribute "fill-opacity" "0" ]


rectSelector : Shape -> List Selector
rectSelector shape =
    attribute "filter" "url(#dropShadow)"
        :: uncurry strokeSelectors (toLineStyle shape.strokeStyle) shape.strokeColor


svgTextOffsetX : Int
svgTextOffsetX =
    textareaPadding - 20


svgTextSelector : TextArea -> List Selector
svgTextSelector { start, end } =
    [ tag "text"
    , attribute "x" <| toString <| (svgTextOffsetX + Basics.min start.x end.x)
    ]


tspanSelector : TextArea -> List Selector
tspanSelector { start, end, fontSize, fill } =
    [ tag "tspan"
    , attribute "dy" <| toString <| fontSizeToLineHeight fontSize
    , attribute "fill" <| Color.Convert.colorToHex fill
    , attribute "x" <| toString <| (svgTextOffsetX + Basics.min start.x end.x)
    ]


imageSelector : Image -> List Selector
imageSelector { width, height, url } =
    [ attribute "width" (String.fromInt (round width))
    , attribute "height" (String.fromInt (round height))

    -- , attribute "xlink:href" url -- possible bug with elm-html-test, will test when there's a fix
    ]


viewFirstAnnotation : Annotation -> Html Msg
viewFirstAnnotation annotation =
    annotation
        |> viewAnnotation EditState.initialState 0
        |> Tuple.first
        |> List.singleton
        |> svgDrawspace


all : Test
all =
    describe "drawing"
        [ describe "annotations"
            [ test "A straight line has the appropriate view attributes" <|
                \() ->
                    aShape
                        |> Lines StraightLine
                        |> viewFirstAnnotation
                        |> Query.fromHtml
                        |> Query.find [ tag "path" ]
                        |> Query.has (lineSelector aShape)
            , test "An arrow has the appropriate view attributes" <|
                \() ->
                    aShape
                        |> Lines Arrow
                        |> viewFirstAnnotation
                        |> Query.fromHtml
                        |> Query.findAll [ tag "path" ]
                        |> Query.index 1
                        |> Query.has (arrowSelector aShape)
            , test "An arrow head has the appropriate view attributes" <|
                \() ->
                    aShape
                        |> Lines Arrow
                        |> viewFirstAnnotation
                        |> Query.fromHtml
                        |> Query.findAll [ tag "path" ]
                        |> Query.first
                        |> Query.has (arrowHeadSelector ( 0, 0 ) aShape)
            , test "A rectangle has the appropriate view attributes" <|
                \() ->
                    aShape
                        |> Shapes Rect (Just testColor)
                        |> viewFirstAnnotation
                        |> Query.fromHtml
                        |> Query.find [ tag "rect" ]
                        |> Query.has (rectSelector aShape)
            , test "A rounded rectangle has the appropriate view attributes" <|
                \() ->
                    aShape
                        |> Shapes RoundedRect (Just testColor)
                        |> viewFirstAnnotation
                        |> Query.fromHtml
                        |> Query.find [ tag "rect" ]
                        |> Query.has (roundedRectSelector aShape)
            , test "An ellipse has the appropriate view attributes" <|
                \() ->
                    aShape
                        |> Shapes Ellipse (Just testColor)
                        |> viewFirstAnnotation
                        |> Query.fromHtml
                        |> Query.find [ tag "ellipse" ]
                        |> Query.has (ellipseSelector aShape)
            , test "A textbox's unselected svg text has the appropriate view attributes" <|
                \() ->
                    aTextArea
                        |> TextBox
                        |> viewFirstAnnotation
                        |> Query.fromHtml
                        |> Query.find [ tag "text" ]
                        |> Query.has (svgTextSelector aTextArea)
            , test "A textbox's unselected svg tspans have the appropriate view attributes" <|
                \() ->
                    aTextArea
                        |> TextBox
                        |> viewFirstAnnotation
                        |> Query.fromHtml
                        |> Query.findAll [ tag "tspan" ]
                        |> Query.each
                            (Expect.all
                                [ Query.has (tspanSelector aTextArea) ]
                            )
            , test "A spotlight has the appropriate view attributes" <|
                \() ->
                    aShape
                        |> Spotlight Rect
                        |> viewFirstAnnotation
                        |> Query.fromHtml
                        |> Query.find [ tag "rect" ]
                        |> Query.has (rectSelector aShape)
            ]
        , describe "viewImage"
            [ test "original image has appropriate attributes to render" <|
                \() ->
                    goat
                        |> viewImage
                        |> Query.fromHtml
                        |> Query.has (attribute "mask" "url(#pixelateMask)" :: imageSelector goat)
            ]
        , describe "viewPixelatedImage"
            [ test "pixelated image has the appropriate attributes to render" <|
                \() ->
                    goat
                        |> viewPixelatedImage
                        |> Query.fromHtml
                        |> Query.has (attribute "filter" "url(#pixelate)" :: imageSelector goat)
            ]
        ]
