module Goat.Subscriptions exposing (subscriptions)

import Json.Decode as Json exposing (Value, field)
import Goat.EditState as EditState exposing (SubscriptionConfig)
import Goat.Model exposing (Image, Model)
import Goat.Ports as Ports
import Goat.Update exposing (Msg(..))
import Time exposing (second)


decodeImage : Json.Decoder Image
decodeImage =
    Json.map6 Image
        (field "id" Json.string)
        (field "url" Json.string)
        (field "width" Json.float)
        (field "height" Json.float)
        (field "originalWidth" Json.float)
        (field "originalHeight" Json.float)


valueToImage : Value -> Result String Image
valueToImage =
    Json.decodeValue decodeImage


valueToImages : Value -> Result String (List Image)
valueToImages =
    Json.decodeValue
        (Json.list decodeImage)


editStateConfig : SubscriptionConfig Msg
editStateConfig =
    { drawToMsg = ContinueDrawing
    , resizeToMsg = ResizeAnnotation
    , moveToMsg = MoveAnnotation
    , keyboardToMsg = KeyboardMsg
    }


imageAnnotationSubscriptions : Model -> Sub Msg
imageAnnotationSubscriptions model =
    if model.imageSelected then
        EditState.subscriptions editStateConfig model.editState
    else
        Sub.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.setImages (SetImages << valueToImages)
        , Ports.newImage (SelectImage << valueToImage)
        , Ports.reset (\_ -> Reset)
        , imageAnnotationSubscriptions model
        , case model.waitingForDropdownToggle of
            Nothing ->
                Sub.none

            Just attributeDropdown ->
                Time.every (0.2 * second) (\_ -> ToggleDropdown attributeDropdown)
        ]
