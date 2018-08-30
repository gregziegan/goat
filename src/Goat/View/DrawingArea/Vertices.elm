module Goat.View.DrawingArea.Vertices exposing (ResizeDirection, viewVertices)

import Color
import Goat.Annotation exposing (EndPosition, SelectState(..), StartPosition)
import Goat.Annotation.Shared exposing (Vertex(..), Vertices(..))
import Svg exposing (Svg, circle, defs, foreignObject, marker, rect, svg)
import Svg.Attributes as Attr


type ResizeDirection
    = NWSE
    | NESW
    | Move


viewVertex : List (Svg.Attribute msg) -> ResizeDirection -> Int -> Int -> Svg msg
viewVertex vertexEvents direction x y =
    circle
        ([ Attr.cx <| String.fromInt x
         , Attr.cy <| String.fromInt y
         , Attr.r "5"
         , Attr.fill <| Color.blue
         , Attr.stroke "white"
         , Attr.strokeWidth "2"
         , Attr.filter "url(#dropShadow)"
         , Attr.class (directionToCursor direction)
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


viewVertices : Vertices -> StartPosition -> EndPosition -> (Vertex -> List (Svg.Attribute msg)) -> SelectState -> Maybe (Svg msg)
viewVertices vertices start end toVertexEvents selectState =
    let
        toVertices =
            case vertices of
                Rectangular ->
                    shapeVertices

                Linear ->
                    lineVertices
    in
    if selectState == SelectedWithVertices then
        Just (toVertices toVertexEvents start end)

    else
        Nothing


directionToCursor : ResizeDirection -> String
directionToCursor direction =
    case direction of
        NWSE ->
            "northWestCursor"

        NESW ->
            "northEastCursor"

        Move ->
            "moveCursor"
