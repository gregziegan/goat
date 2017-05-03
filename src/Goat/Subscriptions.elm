module Goat.Subscriptions exposing (subscriptions)

import Goat.Model exposing (AnnotationState(..), Model)
import Goat.Ports as Ports
import Goat.Update exposing (Msg(..))
import Goat.Utils exposing (toDrawingPosition)
import Keyboard.Extra as Keyboard
import Mouse
import Time exposing (second)


imageAnnotationSubscriptions : Model -> List (Sub Msg)
imageAnnotationSubscriptions model =
    if model.imageSelected then
        case model.annotationState of
            DrawingAnnotation _ _ _ ->
                [ Mouse.moves (ContinueDrawing << toDrawingPosition)
                , Sub.map KeyboardMsg Keyboard.subscriptions
                ]

            ResizingAnnotation _ _ ->
                [ Mouse.moves (ResizeAnnotation << toDrawingPosition)
                , Sub.map KeyboardMsg Keyboard.subscriptions
                ]

            MovingAnnotation _ _ _ _ ->
                [ Mouse.moves (MoveAnnotation << toDrawingPosition) ]

            _ ->
                [ Sub.map KeyboardMsg Keyboard.subscriptions ]
    else
        []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.setImages SetImages
        , Ports.newImage SelectImage
        , Ports.reset (\_ -> Reset)
        , Sub.batch (imageAnnotationSubscriptions model)
        , case model.waitingForDropdownToggle of
            Nothing ->
                Sub.none

            Just attributeDropdown ->
                Time.every (0.2 * second) (\_ -> ToggleDropdown attributeDropdown)
        ]
