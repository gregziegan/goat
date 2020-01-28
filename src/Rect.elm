module Rect exposing (Rect)

import Annotation.Position exposing (Position)


type alias Rect =
    { width : Int
    , height : Int
    , x : Int
    , y : Int
    }


fromPosition : Position -> Rect
fromPosition { start, end } =
    { width = abs (end.x - start.x)
    , height = abs (end.y - start.y)
    , x = Basics.min start.x end.x
    , y = Basics.min start.y end.y
    }
