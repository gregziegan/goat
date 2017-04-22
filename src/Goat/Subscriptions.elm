module Goat.Subscriptions exposing (subscriptions)

import Goat.Utils exposing (toDrawingPosition)
import Goat.Model exposing (AnnotationState(DrawingAnnotation, ResizingAnnotation, MovingAnnotation), Model)
import Goat.Ports as Ports
import Goat.Update exposing (Msg(KeyboardMsg, ContinueDrawing, ResizeAnnotation, MoveAnnotation, Reset, SetImages, SelectImage))
import Keyboard.Extra as Keyboard
import Mouse


imageAnnotationSubscriptions : Model -> Sub Msg
imageAnnotationSubscriptions model =
    if model.imageSelected then
        Sub.batch <|
            case model.annotationState of
                DrawingAnnotation _ _ ->
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
        Sub.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.setImages SetImages
        , Ports.newImage SelectImage
        , Ports.reset (\_ -> Reset)
        , if model.imageSelected then
            imageAnnotationSubscriptions model
          else
            Sub.none
        ]
