module View.DrawingDefinitions exposing (all)

import Fixtures exposing (goat)
import Goat.Flags exposing (Image)
import Goat.View exposing (viewImage)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as HtmlSelector exposing (Selector, all, attribute, class, tag, text)


all : Test
all =
    describe "svg definitions tests"
        [ imageTests ]


imageSelector : Image -> List Selector
imageSelector { width, height, url } =
    [ attribute "width" (toString (round width))
    , attribute "height" (toString (round height))

    -- , attribute "xlink:href" url -- possible bug with elm-html-test, will test when there's a fix
    , attribute "mask" "url(#pixelateMask)"
    ]


imageTests : Test
imageTests =
    describe "viewImage"
        [ test "original image has appropriate attributes to render" <|
            \() ->
                goat
                    |> viewImage
                    |> Query.fromHtml
                    |> Query.has (imageSelector goat)
        ]
