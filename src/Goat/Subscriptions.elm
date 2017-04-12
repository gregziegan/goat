module Goat.Subscriptions exposing (subscriptions)

import Goat.Helpers exposing (toDrawingPosition)
import Goat.Model exposing (AnnotationState(..), Flags, Model, init)
import Goat.Ports as Ports
import Goat.Update exposing (Msg(..))
import Keyboard.Extra as Keyboard
import Mouse


imageUploadSubscriptions : Model -> Sub Msg
imageUploadSubscriptions model =
    Sub.batch
        [ Ports.setImages SetImages
        , Ports.newImage (SetImages << List.singleton)
        ]


imageAnnotationSubscriptions : Model -> Sub Msg
imageAnnotationSubscriptions model =
    if model.imageSelected then
        Sub.batch <|
            case model.annotationState of
                DrawingAnnotation _ _ ->
                    [ Mouse.moves (ContinueDrawing << toDrawingPosition)
                    , Sub.map KeyboardMsg Keyboard.subscriptions
                    ]

                ResizingAnnotation _ ->
                    [ Mouse.moves (ResizeAnnotation << toDrawingPosition)
                    , Sub.map KeyboardMsg Keyboard.subscriptions
                    ]

                MovingAnnotation _ _ _ ->
                    [ Mouse.moves (MoveAnnotation << toDrawingPosition) ]

                _ ->
                    [ Sub.map KeyboardMsg Keyboard.subscriptions ]
    else
        Sub.none


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.imageSelected then
        imageAnnotationSubscriptions model
    else
        imageUploadSubscriptions model
