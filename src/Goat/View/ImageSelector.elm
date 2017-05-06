module Goat.View.ImageSelector exposing (view)

import Goat.Flags exposing (Image)
import Goat.View.Icons as Icons
import Goat.Update exposing (Msg(..), autoExpandConfig)
import Html exposing (Attribute, Html, button, div, h2, h3, img, li, p, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, src, style)
import Html.Events exposing (onClick, onWithOptions)
import List.Zipper exposing (Zipper)


view : Zipper Image -> Html Msg
view images =
    div [ class "image-selector-page" ]
        [ h3 [] [ text "Please select an image to annotate:" ]
        , viewGallery images
        ]


viewGallery : Zipper Image -> Html Msg
viewGallery images =
    images
        |> List.Zipper.toList
        |> List.map viewImageOption
        |> div [ class "image-selector" ]


viewImageOption : Image -> Html Msg
viewImageOption image =
    button
        [ class "image-option"
        , Html.Attributes.width <| round image.width
        , Html.Attributes.height <| round image.height
        , onClick <| SelectImage image
        ]
        [ img [ src image.url, Html.Attributes.height <| round image.height, Html.Attributes.width <| round image.width ] []
        , div [ onClick <| SelectImage image, class "image-edit-pencil" ]
            [ Icons.viewPencil
            ]
        ]
