module Annotation.Position exposing (Position)

import Mouse


type alias StartPosition =
    Mouse.Position


type alias EndPosition =
    Mouse.Position


type alias Position =
    { start : StartPosition
    , end : EndPosition
    }
