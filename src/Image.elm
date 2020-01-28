module Image exposing (Image, editLink)

import Html exposing (Html, button, div, img)
import Html.Attributes exposing (class, height, src, width)
import Html.Events exposing (onClick)
import Icons


type alias Image =
    { id : String
    , url : String
    , width : Float
    , height : Float
    , originalWidth : Float
    , originalHeight : Float
    }


editLink : (Image -> msg) -> Image -> Html msg
editLink onSelect image =
    button
        [ class "image-option"
        , width <| round image.width
        , height <| round image.height
        , onClick <| onSelect image
        ]
        [ img [ src image.url, height <| round image.height, width <| round image.width ] []
        , div [ onClick <| onSelect image, class "image-edit-pencil" ]
            [ Icons.viewPencil
            ]
        ]
