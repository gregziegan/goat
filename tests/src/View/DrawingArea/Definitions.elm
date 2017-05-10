module View.DrawingArea.Definitions exposing (all)

import Goat.View.DrawingArea.Definitions exposing (viewDefinitions)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as HtmlSelector exposing (Selector, all, attribute, class, tag, text)
import View.TestUtil exposing (svgDrawspace)


all : Test
all =
    describe "svg definitions tests"
        [ viewDefinitionsTests ]


viewDefinitionsTests : Test
viewDefinitionsTests =
    describe "viewDefinitions"
        [ test "the necessary arrow definitions are included in the svg drawing" <|
            \() ->
                viewDefinitions [] []
                    |> svgDrawspace
                    |> Query.fromHtml
                    |> Query.find [ tag "defs" ]
                    |> Query.children [ tag "marker" ]
                    |> Query.each (Query.has arrowDefinitions)
        ]
