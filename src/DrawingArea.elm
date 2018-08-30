module DrawingArea exposing (viewMask)

import Svg exposing (Svg, rect)
import Svg.Attributes as Attr


viewMask : Svg msg
viewMask =
    rect
        [ Attr.x "0"
        , Attr.y "0"
        , Attr.height "100%"
        , Attr.width "100%"
        , Attr.mask "url(#Mask)"
        , Attr.style "pointer-events: none;"
        ]
        []
