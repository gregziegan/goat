module View.DrawingArea.Definitions exposing (all)

import Fixtures exposing (goat)
import Goat.View.DrawingArea.Definitions exposing (viewDefinitions)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as HtmlSelector exposing (Selector, all, attribute, class, tag, text)
import View.Util exposing (svgDrawspace)


all : Test
all =
    describe "svg definitions tests"
        [ viewDefinitionsTests ]


arrowDefinitions : List Selector
arrowDefinitions =
    [ attribute "orient" "auto"
    , attribute "markerWidth" "6"
    , attribute "markerHeight" "6"
    , attribute "refX" "65"
    , attribute "refY" "39"
    , attribute "class" "pointerCursor"
    , attribute "viewBox" "0 0 82 77"
    , attribute "filter" "url(#dropShadow)"
    ]


viewDefinitionsTests : Test
viewDefinitionsTests =
    describe "viewDefinitions"
        [ test "the necessary arrow definitions are included in the svg drawing" <|
            \() ->
                viewDefinitions goat.width goat.height [] []
                    |> svgDrawspace
                    |> Query.fromHtml
                    |> Query.find [ tag "defs" ]
                    |> Query.children [ tag "marker" ]
                    |> Query.each (Query.has arrowDefinitions)
        ]
