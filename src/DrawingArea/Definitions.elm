module DrawingArea.Definitions exposing (view)

import Svg exposing (..)
import Svg.Attributes exposing (..)


maskDefinition : List (Svg msg) -> Svg msg
maskDefinition shapes =
    rect
        [ x "0"
        , y "0"
        , width "100%"
        , height "100%"
        , opacity "0.5"
        , fill "white"
        ]
        []
        :: shapes
        |> Svg.mask [ id "Mask" ]


pixelateMaskDefinition : List (Svg msg) -> Svg msg
pixelateMaskDefinition shapes =
    rect
        [ x "0"
        , y "0"
        , width "100%"
        , height "100%"
        , fill "white"
        ]
        []
        :: shapes
        |> Svg.mask [ id "pixelateMask" ]


viewSvgFilters : List (Svg msg)
viewSvgFilters =
    [ Svg.filter [ id "pixelate", x "0", y "0" ]
        [ feFlood [ height "2", width "2", x "4", y "4" ] []
        , feComposite [ height "10", width "10" ] []
        , feTile [ result "a" ] []
        , feComposite [ in_ "SourceGraphic", in2 "a", operator "in" ] []
        , feMorphology [ operator "dilate", radius "5" ] []
        ]
    , Svg.filter [ id "dropShadow", x "-20%", y "-20%", width "200%", height "200%" ]
        [ feGaussianBlur [ in_ "SourceAlpha", stdDeviation "2.2" ] []
        , feOffset [ dx "2", dy "2", result "offsetblur" ] []
        , feComponentTransfer []
            [ Svg.feFuncA [ type_ "linear", slope "0.2" ] []
            ]
        , feMerge []
            [ feMergeNode [] []
            , feMergeNode [ in_ "SourceGraphic" ] []
            ]
        ]
    ]


viewSvgIconDefinitions : List (Svg msg)
viewSvgIconDefinitions =
    [ linearGradient [ id "striped", x1 "50%", x2 "50%", y1 "0%", y2 "100%" ]
        [ stop [ offset "0%", stopColor "#FFF", stopOpacity ".5" ] []
        , stop [ offset "100%", stopOpacity ".5" ] []
        ]
    , Svg.path [ d "M0 0h28v28H0V0zm6 7v14h16V7H6z", id "rect" ] []
    , Svg.mask [ fill "#fff", id "rectMask" ]
        [ use [ xlinkHref "#rect" ] [] ]
    , Svg.path [ d "M0 0h28v28H0V0zm14 21c4.418 0 8-3.134 8-7s-3.582-7-8-7-8 3.134-8 7 3.582 7 8 7z", id "ellipse" ] []
    , Svg.mask [ fill "#fff", id "ellipseMask" ]
        [ use [ xlinkHref "#ellipse" ] [] ]
    , Svg.path [ d "M0 0h28v28H0V0zm6 10.993v6.014C6 19.213 7.79 21 9.996 21h8.008C20.21 21 22 19.212 22 17.007v-6.014C22 8.787 20.21 7 18.004 7H9.996C7.79 7 6 8.788 6 10.993z", id "roundedRect" ] []
    , Svg.mask [ fill "#fff", id "roundedRectMask" ]
        [ use [ xlinkHref "#roundedRect" ] [] ]
    ]


viewMasks : List (Svg msg) -> List (Svg msg) -> List (Svg msg)
viewMasks spotlightCutOuts blurCutOuts =
    [ maskDefinition spotlightCutOuts, pixelateMaskDefinition blurCutOuts ]


view : List (Svg msg) -> List (Svg msg) -> Svg msg
view spotlightCutOuts blurCutOuts =
    [ viewMasks spotlightCutOuts blurCutOuts
    , viewSvgIconDefinitions
    , viewSvgFilters
    ]
        |> List.concat
        |> defs []
