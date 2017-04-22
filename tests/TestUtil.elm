module TestUtil exposing (..)

import Array.Hamt as Array
import Fuzz exposing (Fuzzer)
import Goat.Model exposing (Annotation, AnnotationState(..), EndPosition, Model, ResizingData, StartPosition, Vertex(..))
import Goat.Utils exposing (getPositions, shiftPosition)
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


isAnnotationMovedByCorrectAmount : Position -> Position -> ( StartPosition, EndPosition ) -> Annotation -> Bool
isAnnotationMovedByCorrectAmount start end ( origStart, origEnd ) shiftedAnnotation =
    let
        ( shiftedStart, shiftedEnd ) =
            getPositions shiftedAnnotation

        dx =
            shiftedEnd.x - shiftedStart.x

        dy =
            shiftedEnd.y - shiftedStart.y
    in
        shiftPosition dx dy origStart
            == shiftedStart
            && shiftPosition dx dy origEnd
            == shiftedEnd



-- isAnnotationResizedByCorrectAmount : ResizingData -> Annotation -> ( StartPosition, EndPosition )
-- isAnnotationResizedByCorrectAmount { start, curPos, vertex, originalCoords } annotation =
--     let
--         ( origStart, origEnd ) =
--             originalCoords
--
--         ( resizedStart, resizedEnd ) =
--             getPositions annotation
--
--         dx =
--             curPos.x - start.x
--
--         dy =
--             curPos.y - start.y
--     in
--         case vertex of
--             Start ->
--                 ( shiftPosition dx dy origStart, origEnd )
--
--             _ ->
--                 -- TODO: implement test
--                 ( origStart, origEnd )
