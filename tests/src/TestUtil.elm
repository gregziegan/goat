module TestUtil exposing (..)

import Array.Hamt as Array
import Fuzz exposing (Fuzzer)
import Goat.Annotation exposing (Annotation(TextBox), Vertex(..), positions)
import Goat.Model exposing (EndPosition, Model, StartPosition)
import Goat.Utils exposing (shiftPosition)
import Mouse exposing (Position)
import Random.Pcg as Random
import Shrink


getFirstAnnotation : Model -> Maybe Annotation
getFirstAnnotation model =
    model
        |> .edits
        |> .present
        |> Array.get 0


getAnnotationText : Annotation -> Maybe String
getAnnotationText annotation =
    case annotation of
        TextBox { text } ->
            Just text

        _ ->
            Nothing


position : Fuzzer Position
position =
    Fuzz.custom
        (Random.map2 Position (Random.int -100 100) (Random.int -100 100))
        (\{ x, y } -> Shrink.map Position (Shrink.int x) |> Shrink.andMap (Shrink.int y))


isAnnotationMovedByCorrectAmount : Position -> Position -> ( StartPosition, EndPosition ) -> Annotation -> Bool
isAnnotationMovedByCorrectAmount start end ( origStart, origEnd ) shiftedAnnotation =
    let
        ( shiftedStart, shiftedEnd ) =
            positions shiftedAnnotation

        dx =
            shiftedEnd.x - shiftedStart.x

        dy =
            shiftedEnd.y - shiftedStart.y
    in
        shiftPosition dx dy origStart
            == shiftedStart
            && shiftPosition dx dy origEnd
            == shiftedEnd
