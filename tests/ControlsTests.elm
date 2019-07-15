module ControlsTests exposing (suite)

import Color exposing (Color)
import Controls exposing (initialState)
import Drawing exposing (Drawing(..), LineType(..), ShapeType(..))
import Drawing.Options as Options exposing (StrokeStyle(..))
import Expect exposing (Expectation)
import Fuzz exposing (int)
import Palette
import Test exposing (..)


drawingStylesSuite : Test
drawingStylesSuite =
    let
        defaults =
            initialState.drawingStyles
    in
    describe "drawingStyles"
        [ test "has a purple stroke color" <|
            \_ ->
                Expect.equal Palette.purple defaults.strokeColor
        , test "has an empty fill" <|
            \_ ->
                Expect.equal Nothing defaults.fill
        , test "has a solid stroke style" <|
            \_ ->
                Expect.equal SolidMedium defaults.strokeStyle
        , test "has a medium font size" <|
            \_ ->
                Expect.equal 20 defaults.fontSize
        ]


suite : Test
suite =
    describe "Controls module"
        [ describe "initialState"
            [ test "selects an arrow" <|
                \_ ->
                    Expect.equal (DrawLine Arrow) initialState.drawing
            , test "has a one-click rounded rectangle" <|
                \_ ->
                    Expect.equal (DrawShape RoundedRect) initialState.shape
            , test "has a one-click rounded rectangle spotlight" <|
                \_ ->
                    Expect.equal (DrawSpotlight RoundedRect) initialState.spotlight
            , test "has no selected dropdown" <|
                \_ ->
                    Expect.equal Nothing initialState.currentDropdown
            , test "is not waiting for a dropdown" <|
                \_ ->
                    Expect.equal Nothing initialState.waitingForDropdownToggle
            , drawingStylesSuite
            ]
        ]
