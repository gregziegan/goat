module Page.Gallery exposing (Model, Msg, init, toSession, update, view)

import Html exposing (Html, a, div, h3, img, text)
import Html.Attributes exposing (class, src)
import Icons as Icons
import Image exposing (Image)
import Route
import Session exposing (Session)


type alias Model =
    { session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )


view : List Image -> Model -> { title : String, content : Html Msg }
view images model =
    { title = "Image Gallery"
    , content = content images
    }


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


content : List Image -> Html Msg
content images =
    div [ class "image-selector-page" ]
        [ h3 [] [ text "Please select an image to annotate:" ]
        , viewGallery images
        ]


viewGallery : List Image -> Html Msg
viewGallery images =
    div [ class "image-selector" ]
        (List.map viewImageOption images)


viewImageOption : Image -> Html Msg
viewImageOption image =
    a
        [ class "image-option"
        , (Route.href << Route.Annotate << Just) image.id
        , Html.Attributes.width <| round image.width
        , Html.Attributes.height <| round image.height
        ]
        [ img [ src image.url, Html.Attributes.height <| round image.height, Html.Attributes.width <| round image.width ] []
        , div [ class "image-edit-pencil" ]
            [ Icons.viewPencil
            ]
        ]


toSession : Model -> Session
toSession model =
    model.session
