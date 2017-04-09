module TestUtil exposing (..)

import Array.Hamt as Array
import Fuzz exposing (Fuzzer)
import Goat.Model exposing (Annotation, AnnotationState(..), Model)
import Mouse exposing (Position)
import Random.Pcg as Random
import Shrink


getFirstAnnotation : Model -> Maybe Annotation
getFirstAnnotation model =
    model
        |> .edits
        |> .present
        |> Array.get 0


position : Fuzzer Position
position =
    Fuzz.custom
        (Random.map2 Position (Random.int -100 100) (Random.int -100 100))
        (\{ x, y } -> Shrink.map Position (Shrink.int x) |> Shrink.andMap (Shrink.int y))


getDrawingStateCurPos : AnnotationState -> Maybe Position
getDrawingStateCurPos annotationState =
    case annotationState of
        DrawingAnnotation _ curPos ->
            Just curPos

        _ ->
            Nothing
