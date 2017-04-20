module View.DrawingDefinitions exposing (all)

import Fixtures exposing (goat)
import Goat.Flags exposing (Image)
import Goat.View exposing (viewImage, viewPixelatedImage)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as HtmlSelector exposing (Selector, all, attribute, class, tag, text)


all : Test
all =
    describe "svg definitions tests"
        [ imageTests
        , pixelatedImageTests
        ]


imageSelector : Image -> List Selector
imageSelector { width, height, url } =
    [ attribute "width" (toString (round width))
    , attribute "height" (toString (round height))

    -- , attribute "xlink:href" url -- possible bug with elm-html-test, will test when there's a fix
    ]


imageTests : Test
imageTests =
    describe "viewImage"
        [ test "original image has appropriate attributes to render" <|
            \() ->
                goat
                    |> viewImage
                    |> Query.fromHtml
                    |> Query.has (attribute "mask" "url(#pixelateMask)" :: imageSelector goat)
        ]


pixelatedImageTests : Test
pixelatedImageTests =
    describe "viewPixelatedImage"
        [ test "pixelated image has the appropriate attributes to render" <|
            \() ->
                goat
                    |> viewPixelatedImage
                    |> Query.fromHtml
                    |> Query.has (attribute "filter" "url(#pixelate)" :: imageSelector goat)
        ]
