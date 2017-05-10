module Goat.Annotation.Shared exposing (AnnotationAttributes, DrawingInfo, SelectingInfo, MovingInfo, ResizingInfo, EditingTextInfo, StrokeStyle(..), Vertex(..), Vertices(..))

{-| This module includes data shared by both the EditState module and the Annotation module.

TODO: investigate whether we can provide type variables to EditState to eliminate the need for
this Shared module.

-}

import Color exposing (Color)
import Mouse exposing (Position)


{-| Vertices are classified by their relationship to the `start` and `end`
mouse positions that created the annotation.

e.g: (assume a top-left to bottom-right draw)

Start StartPlusX
+----------+
|**********|
|**********|
|**********|
|**********|
|**********|
+----------+
StartPlusY End

-}
type Vertex
    = Start
    | End
    | StartPlusX
    | StartPlusY


type Vertices
    = Rectangular
    | Linear


type StrokeStyle
    = SolidThin
    | SolidMedium
    | SolidThick
    | SolidVeryThick
    | DashedThin
    | DashedMedium
    | DashedThick
    | DashedVeryThick


type alias AnnotationAttributes =
    { strokeColor : Color
    , fill : Maybe Color
    , strokeStyle : StrokeStyle
    , fontSize : Int
    }


type alias DrawingInfo =
    { start : Position
    , curPos : Position
    , positions : List Position
    }


type alias SelectingInfo =
    { id : Int
    , attributes : AnnotationAttributes
    }


type alias MovingInfo =
    { id : Int
    , start : Position
    , translate : ( Int, Int )
    , attributes : AnnotationAttributes
    }


type alias ResizingInfo =
    { id : Int
    , start : Position
    , curPos : Position
    , vertex : Vertex
    , originalCoords : ( Position, Position )
    , attributes : AnnotationAttributes
    }


type alias EditingTextInfo =
    { id : Int
    , attributes : AnnotationAttributes
    }
