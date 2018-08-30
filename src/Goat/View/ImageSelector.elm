module Goat.View.ImageSelector exposing (view)

import Goat.Model exposing (Image)
import Goat.Update exposing (Msg(..))
import Goat.View.Icons as Icons
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
        |> List.map viewImageOption
        |> div [ class "image-selector" ]


viewImageOption : Image -> Html Msg
viewImageOption image =
    button
        [ class "image-option"
        , Html.Attributes.width <| round image.width
        , Html.Attributes.height <| round image.height
        , onClick <| SelectImage (Ok image)
        ]
        [ img [ src image.url, Html.Attributes.height <| round image.height, Html.Attributes.width <| round image.width ] []
        , div [ onClick <| SelectImage (Ok image), class "image-edit-pencil" ]
            [ Icons.viewPencil
            ]
        ]
