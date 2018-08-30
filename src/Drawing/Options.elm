module Drawing.Options exposing (DrawingStyles, Fill, FontSize, StrokeColor, StrokeStyle(..), defaults, fills, fontSizes, strokeColors, strokeStyles)

import Color exposing (Color)
import Palette


type alias DrawingStyles =
    { strokeColor : StrokeColor
    , fill : Fill
    , strokeStyle : StrokeStyle
    , fontSize : FontSize
    }


type alias StrokeColor =
    Color


type alias Fill =
    Maybe Color


type alias FontSize =
    Int


type StrokeStyle
    = SolidThin
    | SolidMedium
    | SolidThick
    | SolidVeryThick
    | DashedThin
    | DashedMedium
    | DashedThick
    | DashedVeryThick


strokeColors : List StrokeColor
strokeColors =
    [ Palette.purple
    , Palette.red
    , Palette.blue
    , Palette.cyan
    , Palette.green
    , Palette.yellow
    , Palette.orange
    , Palette.black
    , Palette.white
    ]


fills : List Fill
fills =
    [ Nothing
    , Just Palette.purple
    , Just Palette.red
    , Just Palette.blue
    , Just Palette.cyan
    , Just Palette.green
    , Just Palette.yellow
    , Just Palette.orange
    , Just Palette.black
    , Just Palette.white
    ]


strokeStyles : List StrokeStyle
strokeStyles =
    [ SolidThin
    , SolidMedium
    , SolidThick
    , SolidVeryThick
    , DashedThin
    , DashedMedium
    , DashedThick
    , DashedVeryThick
    ]


fontSizes : List FontSize
fontSizes =
    [ 14
    , 16
    , 20
    , 26
    , 32
    , 40
    ]


defaults : DrawingStyles
defaults =
    DrawingStyles Palette.purple Nothing SolidMedium 20
