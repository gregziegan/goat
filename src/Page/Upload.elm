module Page.Upload exposing (Model, Msg, init, toSession, update, view)

import Environment exposing (Environment, Platform(..))
import Html exposing (Html, a, div, h2, p, text)
import Html.Attributes exposing (class)
import Route
import Session exposing (Session)


type alias Model =
    { session : Session }


type Msg
    = NoOp


init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Environment -> Model -> { title : String, content : Html Msg }
view environment model =
    { title = "Image Upload"
    , content = content environment
    }


content : Environment -> Html Msg
content environment =
    case environment.platform of
        Zendesk ->
            div
                [ class "no-images-page" ]
                [ h2 [] [ text "Welcome to G.O.A.T." ]
                , p [] [ text "Please upload an image to the comment editor. The image will show up here for annotating." ]
                ]

        Web ->
            div [ class "droparea" ]
                [ div [ class "info-text" ]
                    [ h2 [] [ text "Please drag and drop an image onto the page" ]
                    , div [ class "goat-time" ]
                        [ p [] [ text "Or, show me the goats!" ]
                        , a [ class "goat-button", Route.href Route.Goats ] [ text "🐐" ]
                        ]
                    ]
                ]


toSession : Model -> Session
toSession model =
    model.session
