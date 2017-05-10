module View.DrawingArea.Definitions exposing (all)

import Fixtures exposing (attributes, drawingInfo)
import Goat.Annotation as Annotation exposing (Drawing(DrawPixelate, DrawSpotlight), ShapeType(Rect))
import Goat.EditState as EditState
import Goat.Update exposing (Msg)
import Goat.View.DrawingArea.Annotation exposing (viewPixelate, viewSpotlightInMask)
import Goat.View.DrawingArea.Definitions as Definitions
import Svg exposing (Svg)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as HtmlSelector exposing (Selector, all, attribute, class, tag, text)
import View.TestUtil exposing (svgDrawspace)


all : Test
all =
    describe "svg definitions tests"
        [ viewDefinitionsTests ]


spotlightMaskSelector =
    [ attribute "x" "0"
    , attribute "y" "0"
    , attribute "width" "100%"
    , attribute "height" "100%"
    , attribute "opacity" "0.5"
    , attribute "fill" "white"
    ]


pixelateMaskSelector =
    [ attribute "x" "0"
    , attribute "y" "0"
    , attribute "width" "100%"
    , attribute "height" "100%"
    , attribute "fill" "white"
    ]


spotlightCutOutSelector =
    [ attribute "fill" "#000000"
    , attribute "width" "26"
    , attribute "height" "38"
    , attribute "x" "50"
    , attribute "y" "50"
    , attribute "class" "pointerCursor"
    ]


pixelateCutoutSelector =
    [ attribute "fill" "#000000"
    , attribute "width" "26"
    , attribute "height" "38"
    , attribute "x" "50"
    , attribute "y" "50"
    , attribute "class" "pointerCursor"
    ]


spotlights : List (Svg Msg)
spotlights =
    case Annotation.fromDrawing False (DrawSpotlight Rect) attributes drawingInfo of
        Just spotlight ->
            viewSpotlightInMask EditState.initialState ( 0, spotlight )
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        Nothing ->
            []


pixelates : List (Svg Msg)
pixelates =
    case Annotation.fromDrawing False DrawPixelate attributes drawingInfo of
        Just pixelate ->
            viewPixelate EditState.initialState 0 pixelate
                |> Maybe.withDefault []

        Nothing ->
            []


maskDefinitions =
    Definitions.view spotlights pixelates
        |> List.singleton
        |> svgDrawspace
        |> Query.fromHtml


viewDefinitionsTests : Test
viewDefinitionsTests =
    describe "viewDefinitions"
        [ test "the spotlight mask contains a full size white backdrop" <|
            \() ->
                maskDefinitions
                    |> Query.find [ tag "mask", attribute "id" "Mask" ]
                    |> Query.children [ tag "rect" ]
                    |> Query.first
                    |> Query.has spotlightMaskSelector
        , test "the spotlight mask contains the correct black cutouts" <|
            \() ->
                maskDefinitions
                    |> Query.find [ tag "mask", attribute "id" "Mask" ]
                    |> Query.children [ tag "rect" ]
                    |> Query.index 1
                    |> Query.has spotlightCutOutSelector
        , test "the pixelate mask contains a full size white backdrop" <|
            \() ->
                maskDefinitions
                    |> Query.find [ tag "mask", attribute "id" "pixelateMask" ]
                    |> Query.children [ tag "rect" ]
                    |> Query.first
                    |> Query.has pixelateMaskSelector
        , test "the pixelate mask contains the correct black cutouts" <|
            \() ->
                maskDefinitions
                    |> Query.find [ tag "mask", attribute "id" "pixelateMask" ]
                    |> Query.children [ tag "rect" ]
                    |> Query.index 1
                    |> Query.has pixelateCutoutSelector
        ]
