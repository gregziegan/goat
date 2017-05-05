module Goat.Subscriptions exposing (subscriptions)

import Goat.EditState as EditState
import Goat.Model exposing (Model)
import Goat.Ports as Ports
import Goat.Update exposing (Msg(..))
import Goat.Utils exposing (toDrawingPosition)
import Time exposing (second)


editStateConfig : EditState.Config Msg ()
editStateConfig =
    { drawToMsg = ContinueDrawing << toDrawingPosition
    , resizeToMsg = ResizeAnnotation << toDrawingPosition
    , moveToMsg = MoveAnnotation << toDrawingPosition
    , keyboardToMsg = KeyboardMsg
    , whenNotSelecting = \_ -> ()
    , whenDrawing = \_ -> ()
    , whenSelecting = \_ _ -> ()
    , whenMoving = \_ -> ()
    , whenResizing = \_ -> ()
    , whenEditingText = \_ _ -> ()
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
