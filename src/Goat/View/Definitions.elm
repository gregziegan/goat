module Goat.View.Definitions exposing (viewDefinitions)

import Color exposing (Color)
import Color.Convert
import Goat.ControlOptions as ControlOptions exposing (fontSizes)
import Goat.Update exposing (Msg(..), autoExpandConfig)
import Html.Attributes exposing (attribute, class, classList, disabled, id, src, style)
import Svg exposing (Svg, circle, defs, foreignObject, marker, rect, svg)
import Svg.Attributes as Attr


viewArrowHeadDefinition : Color -> Svg Msg
viewArrowHeadDefinition color =
    marker
        [ Attr.id <| "arrow-head--" ++ Color.Convert.colorToHex color
        , Attr.orient "auto"
        , Attr.markerWidth "6"
        , Attr.markerHeight "6"
        , Attr.refX "65"
        , Attr.refY "39"
        , Attr.class "pointerCursor"
        , Attr.viewBox "0 0 82 77"
        , Attr.filter "url(#dropShadow)"
        ]
        [ Svg.path [ Attr.d "M20.5,38.5L2.5,-2.5L79.5,38.5L2.5,79.5", Attr.fill <| Color.Convert.colorToHex color ] []
        ]


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
    ControlOptions.strokeColors
        |> List.map viewArrowHeadDefinition
        |> (::) (maskDefinition width height spotlightCutOuts)
        |> (::) (pixelateMaskDefinition blurCutOuts)
        |> flip List.append viewSvgFilters
        |> defs []
        |> List.singleton
