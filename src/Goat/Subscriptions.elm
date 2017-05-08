module Goat.Subscriptions exposing (subscriptions)

import Goat.EditState as EditState exposing (SubscriptionConfig)
import Goat.Model exposing (Model)
import Goat.Ports as Ports
import Goat.Update exposing (Msg(..))
import Time exposing (second)


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
        [ Ports.setImages SetImages
        , Ports.newImage SelectImage
        , Ports.reset (\_ -> Reset)
        , imageAnnotationSubscriptions model
        , case model.waitingForDropdownToggle of
            Nothing ->
                Sub.none

            Just attributeDropdown ->
                Time.every (0.2 * second) (\_ -> ToggleDropdown attributeDropdown)
        ]
