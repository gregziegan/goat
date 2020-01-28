module Page.Upload exposing (view)

import Environment exposing (Platform(..))



-- VIEW


view : Platform -> { title : String, content : Html msg }
view platform =
    case platform of
        Zendesk ->
            { title = "Annotation App", content = viewEmptyImagesScreen }

        Web ->
            { title = "GOAT - Upload an image!", content = viewInfoScreen }


viewEmptyImagesScreen : Html Msg
viewEmptyImagesScreen =
    div
        [ class "no-images-page" ]
        [ h2 [] [ text "Welcome to G.O.A.T." ]
        , p [] [ text "Please upload an image to the comment editor. The image will show up here for annotating." ]
        ]


viewInfoScreen : Html Msg
viewInfoScreen =
    div [ class "droparea" ]
        [ div [ class "info-text" ]
            [ h2 [] [ text "Please drag and drop an image onto the page" ]
            , div [ class "goat-time" ]
                [ p [] [ text "Or, show me the goats!" ]
                , button [ class "goat-button", onClick ShowMeTheGoats ] [ text "🐐" ]
                ]
            ]
        ]
