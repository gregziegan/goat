module View.Annotation exposing (all)

import Color exposing (Color)
import Color.Convert
import Expect exposing (Expectation)
import Fixtures exposing (end, model, start, aShape, aTextArea, testColor)
import Goat.Helpers exposing (linePath, toLineStyle, fontSizeToLineHeight)
import Goat.Model
    exposing
        ( Annotation(..)
        , AnnotationState(..)
        , Drawing(..)
        , LineMode(..)
        , LineType(..)
        , Shape
        , ShapeMode(..)
        , ShapeType(..)
        , TextArea
        )
import Goat.View exposing (viewAnnotation)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as HtmlSelector exposing (Selector, all, attribute, class, tag, text)
import View.Util exposing (svgDrawspace)


all : Test
all =
    describe "drawing"
        [ viewAnnotationTests ]


lineSelector : Shape -> List Selector
lineSelector shape =
    [ attribute "fill" "none"
    , attribute "d" (linePath shape.start shape.end)
    ]
        ++ (uncurry strokeSelectors (toLineStyle shape.strokeStyle)) shape.strokeColor


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
        ++ (uncurry strokeSelectors (toLineStyle shape.strokeStyle)) shape.strokeColor


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
        :: (uncurry strokeSelectors (toLineStyle shape.strokeStyle)) shape.strokeColor


svgTextSelector : TextArea -> List Selector
svgTextSelector { start, end } =
    [ tag "text"
    , attribute "x" <| toString <| Basics.min start.y end.y
    ]


tspanSelector : TextArea -> List Selector
tspanSelector { start, end, fontSize, fill } =
    [ tag "tspan"
    , attribute "dy" <| toString <| fontSizeToLineHeight fontSize
    , attribute "x" <| toString <| Basics.min start.x end.x
    , attribute "fill" <| Color.Convert.colorToHex fill
    ]


viewAnnotationTests : Test
viewAnnotationTests =
    describe "annotations"
        [ test "A straight line has the appropriate view attributes" <|
            \() ->
                aShape
                    |> Lines StraightLine
                    |> viewAnnotation ReadyToDraw 0
                    |> svgDrawspace
                    |> Query.fromHtml
                    |> Query.find [ tag "path" ]
                    |> Query.has (lineSelector aShape)
        , test "An arrow has the appropriate view attributes" <|
            \() ->
                aShape
                    |> Lines Arrow
                    |> viewAnnotation ReadyToDraw 0
                    |> svgDrawspace
                    |> Query.fromHtml
                    |> Query.find [ tag "path" ]
                    |> Query.has (lineSelector aShape)
        , test "A rectangle has the appropriate view attributes" <|
            \() ->
                aShape
                    |> Shapes Rect (Just testColor)
                    |> viewAnnotation ReadyToDraw 0
                    |> svgDrawspace
                    |> Query.fromHtml
                    |> Query.find [ tag "rect" ]
                    |> Query.has (rectSelector aShape)
        , test "A rounded rectangle has the appropriate view attributes" <|
            \() ->
                aShape
                    |> Shapes RoundedRect (Just testColor)
                    |> viewAnnotation ReadyToDraw 0
                    |> svgDrawspace
                    |> Query.fromHtml
                    |> Query.find [ tag "rect" ]
                    |> Query.has (roundedRectSelector aShape)
        , test "An ellipse has the appropriate view attributes" <|
            \() ->
                aShape
                    |> Shapes Ellipse (Just testColor)
                    |> viewAnnotation ReadyToDraw 0
                    |> svgDrawspace
                    |> Query.fromHtml
                    |> Query.find [ tag "ellipse" ]
                    |> Query.has (ellipseSelector aShape)
        , test "A textbox's unselected svg text has the appropriate view attributes" <|
            \() ->
                aTextArea
                    |> TextBox
                    |> viewAnnotation ReadyToDraw 0
                    |> svgDrawspace
                    |> Query.fromHtml
                    |> Query.find [ tag "text" ]
                    |> Query.has (svgTextSelector aTextArea)
        , test "A textbox's unselected svg tspans have the appropriate view attributes" <|
            \() ->
                aTextArea
                    |> TextBox
                    |> viewAnnotation ReadyToDraw 0
                    |> svgDrawspace
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
                    |> viewAnnotation ReadyToDraw 0
                    |> svgDrawspace
                    |> Query.fromHtml
                    |> Query.find [ tag "rect" ]
                    |> Query.has (rectSelector aShape)
        ]
