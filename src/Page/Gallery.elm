module Page.Gallery exposing (..)

import Image exposing (Image)
import Html exposing (Attribute, Html, button, div, h2, h3, img, li, p, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, src, style)
import Html.Events exposing (onClick)
import List.Selection as Selection exposing (Selection)


view : Selection Image -> Html Msg
view images =
    div [ class "image-selector-page" ]
        [ h3 [] [ text "Please select an image to annotate:" ]
        , viewGallery images
        ]


viewGallery : Selection Image -> Html Msg
viewGallery images =
    images
        |> Selection.toList
        |> List.map Image.editLink onSelect
        |> div [ class "image-selector" ]



        