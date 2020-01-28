module TestUtil exposing (..)

import Fixtures exposing (goat)
import Html
import Html.Attributes as Html
import Svg exposing (svg)
import Svg.Attributes as Attr exposing (fontSize)


svgDrawspace : List (Svg.Svg msg) -> Html.Html msg
svgDrawspace =
    svg
        [ Attr.id "drawing"
        , Attr.class "drawing"
        , Attr.width <| toString <| round goat.width
        , Attr.height <| toString <| round goat.height
        , Html.attribute "xmlns" "http://www.w3.org/2000/svg"
        ]
