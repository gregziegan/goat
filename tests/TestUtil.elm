module TestUtil exposing (..)

import Array.Hamt as Array
import Goat.Model exposing (Annotation, Model)


getFirstAnnotation : Model -> Maybe Annotation
getFirstAnnotation model =
    model
        |> .edits
        |> .present
        |> Array.get 0
