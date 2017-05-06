module Goat.View.DrawingArea.Vertices exposing (..)

import Color exposing (Color)
import Color.Convert
import Goat.Annotation exposing (SelectState(..))
import Goat.EditState exposing (Vertex(..))
import Goat.Model exposing (..)
import Goat.Update exposing (Msg(..), autoExpandConfig)
import Svg exposing (Svg, circle, defs, foreignObject, marker, rect, svg)
import Svg.Attributes as Attr


viewVertex : List (Svg.Attribute Msg) -> Int -> Int -> Svg Msg
viewVertex vertexEvents x y =
    circle
        ([ Attr.cx <| toString x
         , Attr.cy <| toString y
         , Attr.r "5"
         , Attr.fill <| Color.Convert.colorToHex Color.blue
         , Attr.stroke "white"
         , Attr.strokeWidth "2"
         , Attr.filter "url(#dropShadow)"
         ]
            ++ vertexEvents
        )
        []


shapeVertices : (Vertex -> ResizeDirection -> List (Svg.Attribute Msg)) -> StartPosition -> EndPosition -> Svg Msg
shapeVertices toVertexEvents start end =
    let
        ( resizeDir1, resizeDir2, resizeDir3, resizeDir4 ) =
            if start.x < end.x && start.y > end.y then
                ( NESW, NWSE, NWSE, NESW )
            else if start.x < end.x && start.y < end.y then
                ( NWSE, NESW, NESW, NWSE )
            else if start.x > end.x && start.y > end.y then
                ( NWSE, NESW, NESW, NWSE )
            else
                ( NESW, NWSE, NWSE, NESW )
    in
        Svg.g []
            [ viewVertex (toVertexEvents Start resizeDir1) start.x start.y
            , viewVertex (toVertexEvents StartPlusX resizeDir2) end.x start.y
            , viewVertex (toVertexEvents StartPlusY resizeDir3) start.x end.y
            , viewVertex (toVertexEvents End resizeDir4) end.x end.y
            ]


lineVertices : (Vertex -> ResizeDirection -> List (Svg.Attribute Msg)) -> StartPosition -> EndPosition -> Svg Msg
lineVertices toVertexEvents start end =
    Svg.g []
        [ viewVertex (toVertexEvents Start Move) start.x start.y
        , viewVertex (toVertexEvents End Move) end.x end.y
        ]


viewVertices : Vertices -> StartPosition -> EndPosition -> (Vertex -> ResizeDirection -> List (Svg.Attribute Msg)) -> SelectState -> Maybe (Svg Msg)
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
