module TestUtil exposing (getFirstAnnotation, isAnnotationMovedByCorrectAmount, position)

import Array
import Fuzz exposing (Fuzzer)
import Annotation as Annotation exposing (Annotation, EndPosition, StartPosition, shiftPosition)
import Model exposing (Model)
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


isAnnotationMovedByCorrectAmount : Position -> Position -> ( StartPosition, EndPosition ) -> Annotation -> Bool
isAnnotationMovedByCorrectAmount start end ( origStart, origEnd ) shiftedAnnotation =
    let
        ( shiftedStart, shiftedEnd ) =
            Annotation.startAndEnd shiftedAnnotation

        dx =
            shiftedEnd.x - shiftedStart.x

        dy =
            shiftedEnd.y - shiftedStart.y
    in
    shiftPosition dx dy origStart
        == shiftedStart
        && shiftPosition dx dy origEnd
        == shiftedEnd
