module Page exposing (Page(..), view, viewErrors)

import Browser exposing (Document)
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)


type Page
    = Other
    | Upload
    | Gallery
    | Annotate


view : Page -> { title : String, content : Html msg } -> Document msg
view page { title, content } =
    { title = title ++ " - G.O.A.T."
    , body = [ content ]
    }


viewErrors : msg -> List String -> Html msg
viewErrors dismissErrors errors =
    if List.isEmpty errors then
        Html.text ""

    else
        div
            [ class "error-messages"
            , style "position" "fixed"
            , style "top" "0"
            , style "background" "rgb(250, 250, 250)"
            , style "padding" "20px"
            , style "border" "1px solid"
            ]
        <|
            List.map (\error -> p [] [ text error ]) errors
                ++ [ button [ onClick dismissErrors ] [ text "Ok" ] ]
