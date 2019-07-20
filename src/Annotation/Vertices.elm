module Annotation.Vertices exposing (ResizeDirection, Vertex(..), Vertices(..), view)

import Color
import Palette
import Position exposing (EndPosition, StartPosition)
import Svg exposing (Svg, circle)
import Svg.Attributes exposing (class, cx, cy, fill, filter, r, stroke, strokeWidth)


{-| Vertices are classified by their relationship to the `start` and `end`
mouse positions that created the annotation.

e.g: (assume a top-left to bottom-right draw)

Start - - - - - - - StartPlusX
+--------------------+
|--------------------|
|--------------------|
|--------------------|
|--------------------|
|--------------------|
+--------------------+
StartPlusY - - - - - End

-}
type Vertex
    = Start
    | End
    | StartPlusX
    | StartPlusY


type Vertices
    = Rectangular
    | Linear


type ResizeDirection
    = NWSE
    | NESW
    | Move


type alias Config msg =
    { kind : Vertices
    , start : StartPosition
    , end : EndPosition
    , eventsForVertex : Vertex -> List (Svg.Attribute msg)
    }


viewVertex : List (Svg.Attribute msg) -> ResizeDirection -> Int -> Int -> Svg msg
viewVertex vertexEvents direction x y =
    circle
        ([ cx <| String.fromInt x
         , cy <| String.fromInt y
         , r "5"
         , fill <| Color.toHexString Palette.blue
         , stroke "white"
         , strokeWidth "2"
         , filter "url(#dropShadow)"
         , class (directionToCursor direction)
         ]
            ++ vertexEvents
        )
        []


type alias ResizeDirections =
    { startVert : ResizeDirection
    , startPlusXVert : ResizeDirection
    , startPlusYVert : ResizeDirection
    , endVert : ResizeDirection
    }


shapeVerticesHelp : StartPosition -> EndPosition -> ResizeDirections
shapeVerticesHelp start end =
    if start.x < end.x && start.y > end.y then
        ResizeDirections NESW NWSE NWSE NESW

    else if start.x < end.x && start.y < end.y then
        ResizeDirections NWSE NESW NESW NWSE

    else if start.x > end.x && start.y > end.y then
        ResizeDirections NWSE NESW NESW NWSE

    else
        ResizeDirections NESW NWSE NWSE NESW


shapeVertices : (Vertex -> List (Svg.Attribute msg)) -> StartPosition -> EndPosition -> Svg msg
shapeVertices toVertexEvents start end =
    let
        { startVert, startPlusXVert, startPlusYVert, endVert } =
            shapeVerticesHelp start end
    in
    Svg.g []
        [ viewVertex (toVertexEvents Start) startVert start.x start.y
        , viewVertex (toVertexEvents StartPlusX) startPlusXVert end.x start.y
        , viewVertex (toVertexEvents StartPlusY) startPlusYVert start.x end.y
        , viewVertex (toVertexEvents End) endVert end.x end.y
        ]


lineVertices : (Vertex -> List (Svg.Attribute msg)) -> StartPosition -> EndPosition -> Svg msg
lineVertices toVertexEvents start end =
    Svg.g []
        [ viewVertex (toVertexEvents Start) Move start.x start.y
        , viewVertex (toVertexEvents End) Move end.x end.y
        ]


view : Config msg -> Svg msg
view { kind, start, end, eventsForVertex } =
    case kind of
        Rectangular ->
            shapeVertices eventsForVertex start end

        Linear ->
            lineVertices eventsForVertex start end


directionToCursor : ResizeDirection -> String
directionToCursor direction =
    case direction of
        NWSE ->
            "northWestCursor"

        NESW ->
            "northEastCursor"

        Move ->
            "moveCursor"
