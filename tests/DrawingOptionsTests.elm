module DrawingOptionsTests exposing (suite)

import Color exposing (Color)
import Drawing.Options as Options exposing (StrokeStyle(..))
import Expect exposing (Expectation)
import Fuzz exposing (int)
import Palette
import Test exposing (..)


fills : List (Maybe Color)
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


strokeColors : List Color
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


fontSizes : List Int
fontSizes =
    [ 14
    , 16
    , 20
    , 26
    , 32
    , 40
    ]


suite : Test
suite =
    describe "Drawing.Options module"
        [ describe "strokeColors"
            [ test "returns the rainbow" <|
                \_ ->
                    Expect.equalLists strokeColors Options.strokeColors
            ]
        , describe "fills"
            [ test "returns the rainbow and nothingnes" <|
                \_ ->
                    Expect.equalLists fills Options.fills
            ]
        , describe "strokeStyles"
            [ test "provides four solid and four dashed options" <|
                \_ ->
                    Expect.equalLists strokeStyles Options.strokeStyles
            ]
        , describe "fontSizes"
            [ test "provides six options in increasing order" <|
                \_ ->
                    Expect.equalLists fontSizes Options.fontSizes
            ]
        ]
