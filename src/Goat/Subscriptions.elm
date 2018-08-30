module Goat.Subscriptions exposing (subscriptions)

import Goat.EditState as EditState exposing (SubscriptionConfig)
import Goat.Model exposing (Image, Model)
import Goat.Ports as Ports
import Goat.Update exposing (Msg(..))
import Json.Decode as Json exposing (Value, field)
import List.Selection as Selection
import Time


decodeImage : Json.Decoder Image
decodeImage =
    Json.map6 Image
        (field "id" Json.string)
        (field "url" Json.string)
        (field "width" Json.float)
        (field "height" Json.float)
        (field "originalWidth" Json.float)
        (field "originalHeight" Json.float)


valueToImage : Value -> Result Json.Error Image
valueToImage =
    Json.decodeValue decodeImage


valueToImages : Value -> Result Json.Error (List Image)
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
    model.images
        |> Maybe.andThen Selection.selected
        |> Maybe.map (\_ -> EditState.subscriptions editStateConfig model.editState)
        |> Maybe.withDefault Sub.none


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
                Time.every 200 (\_ -> ToggleDropdown attributeDropdown)
        ]
