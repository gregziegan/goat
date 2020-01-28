module Annotation.Attributes exposing (Attributes, Fill(..), StrokeStyle, Vertex)

import Color exposing (Color)


{-| Vertices are classified by their relationship to the `start` and `end`
mouse positions that created the annotation.

e.g: (assume a top-left to bottom-right draw)

Start StartPlusX
+----------+
|----------|
|----------|
|----------|
|----------|
+----------+
StartPlusY End

-}
type Vertex
    = Start
    | End
    | StartPlusX
    | StartPlusY



-- goalToAnnotation : Goal -> StartPosition -> Annotation
-- goalToAnnotation goal position =
--   case goal of
--     Arrow ->
--       Annotation.arrow position position
--     Line ->
--       Annotation.line position position
--     FreeHand ->
--       Annotation.freeHand position position
--     Rect ->
--       Annotation.rect position position
--     RoundedRect ->
--       Annotation.roundedRect position position
--     Ellipse ->
--       Annotation.ellipse position position
--     TextBox ->
--       Annotation.textBot position position
--     SpotlightRect ->
--       Annotation.
-- type Goal
--     = Arrow
--     | Line
--     | FreeHand
--     | Rect
--     | RoundedRect
--     | Ellipse
--     | TextBox
--     | SpotlightRect
--     | SpotlightRoundedRect
--     | SpotlightEllipse
--     | Pixelate


type StrokeStyle
    = SolidThin
    | SolidMedium
    | SolidThick
    | SolidVeryThick
    | DashedThin
    | DashedMedium
    | DashedThick
    | DashedVeryThick


type Fill
    = Spotlight
    | Filled Color
    | Transparent


type alias Attributes =
    { strokeColor : Color
    , strokeStyle : StrokeStyle
    , fill : Fill
    , fontSize : Int
    }
