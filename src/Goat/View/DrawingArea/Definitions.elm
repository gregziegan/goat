module Goat.View.DrawingArea.Definitions exposing (..)

import Array.Hamt as Array exposing (Array)
import Color exposing (Color)
import Goat.AnnotationAttributes exposing (Annotation, ShapeType, Shape, spotlightToMaskCutout)
import Goat.EditState exposing (EditState)
import Goat.Model exposing (..)
import Goat.Update exposing (Msg(..), autoExpandConfig)
import Goat.View.DrawingArea.Annotation as Annotation
import Goat.View.Utils exposing (..)
import Html.Attributes exposing (attribute, class, classList, disabled, id, src, style)
import Svg exposing (Svg, circle, defs, foreignObject, marker, rect, svg)
import Svg.Attributes as Attr


viewNonSpotlightAnnotations : EditState -> Array Annotation -> List (Svg Msg)
viewNonSpotlightAnnotations editState annotations =
    let
        annotationsAndVertices =
            annotations
                |> Array.toList
                |> List.indexedMap (Annotation.viewAnnotation editState)
    in
        List.map Tuple.first annotationsAndVertices
            ++ List.filterMap Tuple.second annotationsAndVertices


viewMaskCutOut : EditState -> ( Int, ShapeType, Shape ) -> Svg Msg
viewMaskCutOut editState ( index, shapeType, shape ) =
    Annotation.viewShape (Annotation.editStateAttributes index editState) shapeType (Just Color.black) shape


viewSpotlights : EditState -> Array Annotation -> List (Svg Msg)
viewSpotlights editState annotations =
    annotations
        |> Array.toIndexedList
        |> List.filterMap (Maybe.map (viewMaskCutOut editState) << spotlightToMaskCutout)


viewPixelates : EditState -> Array Annotation -> List (Svg Msg)
viewPixelates editState annotations =
    annotations
        |> Array.toIndexedList
        |> List.filterMap (uncurry (Annotation.viewPixelate editState))
        |> List.concat


maskDefinition : Float -> Float -> List (Svg Msg) -> Svg Msg
maskDefinition width height shapes =
    rect
        ([ Attr.x "0"
         , Attr.y "0"
         , Attr.width <| toString width
         , Attr.height <| toString height
         , Attr.opacity "0.5"
         , Attr.fill "white"
         ]
        )
        []
        :: shapes
        |> Svg.mask [ Attr.id "Mask" ]


pixelateMaskDefinition : List (Svg Msg) -> Svg Msg
pixelateMaskDefinition shapes =
    rect
        ([ Attr.x "0"
         , Attr.y "0"
         , Attr.width "100%"
         , Attr.height "100%"
         , Attr.fill "white"
         ]
        )
        []
        :: shapes
        |> Svg.mask [ Attr.id "pixelateMask" ]


{-| TODO: fix these filters for lines. Lines/Arrows render very strangely with this filter.
-}
viewSvgFilters : List (Svg Msg)
viewSvgFilters =
    [ Svg.filter [ id "pixelate", Attr.x "0", Attr.y "0" ]
        [ Svg.feFlood [ Attr.height "2", Attr.width "2", Attr.x "4", Attr.y "4" ] []
        , Svg.feComposite [ Attr.height "10", Attr.width "10" ] []
        , Svg.feTile [ Attr.result "a" ] []
        , Svg.feComposite [ Attr.in_ "SourceGraphic", Attr.in2 "a", Attr.operator "in" ] []
        , Svg.feMorphology [ Attr.operator "dilate", Attr.radius "5" ] []
        ]
    , Svg.filter [ Attr.id "dropShadow", Attr.x "-20%", Attr.y "-20%", Attr.width "200%", Attr.height "200%" ]
        [ Svg.feGaussianBlur [ Attr.in_ "SourceAlpha", Attr.stdDeviation "2.2" ] []
        , Svg.feOffset [ Attr.dx "2", Attr.dy "2", Attr.result "offsetblur" ] []
        , Svg.feComponentTransfer []
            [ Svg.feFuncA [ Attr.type_ "linear", Attr.slope "0.2" ] []
            ]
        , Svg.feMerge []
            [ Svg.feMergeNode [] []
            , Svg.feMergeNode [ Attr.in_ "SourceGraphic" ] []
            ]
        ]
    ]


viewDefinitions : Float -> Float -> List (Svg Msg) -> List (Svg Msg) -> List (Svg Msg)
viewDefinitions width height spotlightCutOuts blurCutOuts =
    [ Svg.linearGradient [ id "striped", Attr.x1 "50%", Attr.x2 "50%", Attr.y1 "0%", Attr.y2 "100%" ]
        [ Svg.stop [ Attr.offset "0%", Attr.stopColor "#FFF", Attr.stopOpacity ".5" ] []
        , Svg.stop [ Attr.offset "100%", Attr.stopOpacity ".5" ] []
        ]
    , maskDefinition width height spotlightCutOuts
    , pixelateMaskDefinition blurCutOuts
    , Svg.path [ Attr.d "M0 0h28v28H0V0zm6 7v14h16V7H6z", id "rect" ] []
    , Svg.mask [ Attr.fill "#fff", id "rectMask" ]
        [ Svg.use [ Attr.xlinkHref "#rect" ] [] ]
    , Svg.path [ Attr.d "M0 0h28v28H0V0zm14 21c4.418 0 8-3.134 8-7s-3.582-7-8-7-8 3.134-8 7 3.582 7 8 7z", id "ellipse" ] []
    , Svg.mask [ Attr.fill "#fff", id "ellipseMask" ]
        [ Svg.use [ Attr.xlinkHref "#ellipse" ] [] ]
    , Svg.path [ Attr.d "M0 0h28v28H0V0zm6 10.993v6.014C6 19.213 7.79 21 9.996 21h8.008C20.21 21 22 19.212 22 17.007v-6.014C22 8.787 20.21 7 18.004 7H9.996C7.79 7 6 8.788 6 10.993z", id "roundedRect" ] []
    , Svg.mask [ Attr.fill "#fff", id "roundedRectMask" ]
        [ Svg.use [ Attr.xlinkHref "#roundedRect" ] [] ]
    ]
        |> flip List.append viewSvgFilters
        |> defs []
        |> List.singleton
