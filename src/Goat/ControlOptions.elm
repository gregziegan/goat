module Goat.ControlOptions exposing (strokeColors, fills, strokeStyles, fontSizes, controlUIWidth)

import Color exposing (Color)
import Goat.Annotation as Annotation
import Goat.Annotation.Shared exposing (StrokeStyle)


strokeColors : List Color
strokeColors =
    [ Color.rgb 255 0 212
    , Color.rgb 255 0 0
    , Color.rgb 73 0 255
    , Color.rgb 0 202 255
    , Color.rgb 16 255 0
    , Color.rgb 255 226 0
    , Color.rgb 255 129 0
    , Color.black
    , Color.white
    ]


fills : List (Maybe Color)
fills =
    [ Nothing
    , Just (Color.rgb 255 0 212)
    , Just (Color.rgb 255 0 0)
    , Just (Color.rgb 73 0 255)
    , Just (Color.rgb 0 202 255)
    , Just (Color.rgb 16 255 0)
    , Just (Color.rgb 255 226 0)
    , Just (Color.rgb 255 129 0)
    , Just Color.black
    , Just Color.white
    ]


strokeStyles : List StrokeStyle
strokeStyles =
    Annotation.strokeStyles


fontSizes : List Int
fontSizes =
    [ 14
    , 16
    , 20
    , 26
    , 32
    , 40
    ]


{-| The sidebar is a hard-coded width. This offset is used to shift the incoming mouse position.
TODO: investigate whether this can be skipped by using position: relative, or some
other CSS rule.
-}
controlUIWidth : number
controlUIWidth =
    83
