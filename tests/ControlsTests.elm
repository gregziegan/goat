module ControlsTests exposing (suite)

import Color exposing (Color)
import Controls exposing (State, initialState)
import Drawing exposing (AttributeDropdown(..), Drawing(..), LineType(..), ShapeType(..))
import Drawing.Options as Options exposing (DrawingStyles, StrokeStyle(..))
import Expect exposing (Expectation)
import Fuzz exposing (int)
import Keyboard exposing (Key(..))
import Palette
import Test exposing (..)


key : String -> Key
key character =
    Character character


withDropdown : AttributeDropdown -> State -> State
withDropdown dropdown state =
    { state | currentDropdown = Just dropdown }


defaultDrawingStylesSuite : DrawingStyles -> Test
defaultDrawingStylesSuite defaults =
    describe "defaultDrawingStyles"
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
            , defaultDrawingStylesSuite initialState.drawingStyles
            ]
        , describe "closeDropdown" <|
            [ test "unselects dropdown" <|
                \_ ->
                    initialState
                        |> withDropdown ShapesDropdown
                        |> Controls.closeDropdown
                        |> .currentDropdown
                        |> Expect.equal Nothing
            ]
        , defaultDrawingStylesSuite Controls.defaultDrawingStyles
        , describe "onKeyDown" <|
            [ describe "when A is pressed" <|
                [ test "the arrow is selected" <|
                    \_ ->
                        { initialState | drawing = DrawFreeHand }
                            |> Controls.onKeyDown (key "A")
                            |> .drawing
                            |> Expect.equal (DrawLine Arrow)
                ]
            , describe "when C is pressed" <|
                [ test "selects the rounded rectangle spotlight" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "C")
                            |> .drawing
                            |> Expect.equal (DrawSpotlight RoundedRect)
                , test "selects the rounded rectangle as the new spotlight dropdown default" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "C")
                            |> .spotlight
                            |> Expect.equal (DrawSpotlight RoundedRect)
                ]
            , describe "when H is pressed" <|
                [ test "selects free hand" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "H")
                            |> .drawing
                            |> Expect.equal DrawFreeHand
                ]
            , describe "when L is pressed" <|
                [ test "selects the line" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "L")
                            |> .drawing
                            |> Expect.equal (DrawLine StraightLine)
                ]
            , describe "when R is pressed" <|
                [ test "selects the rectangle" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "R")
                            |> .drawing
                            |> Expect.equal (DrawShape Rect)
                , test "selects the rectangle as the new shape dropdown default" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "R")
                            |> .shape
                            |> Expect.equal (DrawShape Rect)
                ]
            , describe "when O is pressed" <|
                [ test "selects the rounded rectangle" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "O")
                            |> .drawing
                            |> Expect.equal (DrawShape RoundedRect)
                , test "selects the rounded rectangle as the new shape dropdown default" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "O")
                            |> .shape
                            |> Expect.equal (DrawShape RoundedRect)
                ]
            , describe "when E is pressed" <|
                [ test "selects the ellipse" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "E")
                            |> .drawing
                            |> Expect.equal (DrawShape Ellipse)
                , test "selects the ellipse as the new shape dropdown default" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "E")
                            |> .shape
                            |> Expect.equal (DrawShape Ellipse)
                ]
            , describe "when T is pressed" <|
                [ test "selects the text box" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "T")
                            |> .drawing
                            |> Expect.equal DrawTextBox
                ]
            , describe "when G is pressed"
                [ test "selects the rectangle spotlight" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "G")
                            |> .drawing
                            |> Expect.equal (DrawSpotlight Rect)
                , test "selects the rectangle as the new spotlight dropdown default" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "G")
                            |> .spotlight
                            |> Expect.equal (DrawSpotlight Rect)
                ]
            , describe "when I is pressed"
                [ test "selects the ellipse spotlight" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "I")
                            |> .drawing
                            |> Expect.equal (DrawSpotlight Ellipse)
                , test "selects the ellipse as the new spotlight dropdown default" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "I")
                            |> .spotlight
                            |> Expect.equal (DrawSpotlight Ellipse)
                ]
            , describe "when P is pressed"
                [ test "selects pixelate" <|
                    \_ ->
                        initialState
                            |> Controls.onKeyDown (key "P")
                            |> .drawing
                            |> Expect.equal DrawPixelate
                ]
            , describe "when N is pressed"
                [ describe "and the fonts dropdown is open"
                    [ test "closes the dropdown" <|
                        \_ ->
                            initialState
                                |> withDropdown Fonts
                                |> Controls.onKeyDown (key "N")
                                |> .currentDropdown
                                |> Expect.equal Nothing
                    ]
                , describe "and the fonts dropdown is closed"
                    [ test "opens the fonts dropdown" <|
                        \_ ->
                            initialState
                                |> Controls.onKeyDown (key "N")
                                |> .currentDropdown
                                |> Expect.equal (Just Fonts)
                    , test "selects the textbox" <|
                        \_ ->
                            initialState
                                |> Controls.onKeyDown (key "N")
                                |> .drawing
                                |> Expect.equal DrawTextBox
                    ]
                , describe "and another dropdown is open"
                    [ test "opens the fonts dropdown" <|
                        \_ ->
                            initialState
                                |> withDropdown Fills
                                |> Controls.onKeyDown (key "N")
                                |> .currentDropdown
                                |> Expect.equal (Just Fonts)
                    , test "selects the textbox" <|
                        \_ ->
                            initialState
                                |> withDropdown Fills
                                |> Controls.onKeyDown (key "N")
                                |> .drawing
                                |> Expect.equal DrawTextBox
                    ]
                ]
            , describe "when K is pressed"
                [ describe "and the stroke colors dropdown is open"
                    [ test "closes the dropdown" <|
                        \_ ->
                            initialState
                                |> withDropdown StrokeColors
                                |> Controls.onKeyDown (key "K")
                                |> .currentDropdown
                                |> Expect.equal Nothing
                    ]
                , describe "and the stroke colors dropdown is closed"
                    [ test "opens the stroke colors dropdown" <|
                        \_ ->
                            initialState
                                |> Controls.onKeyDown (key "K")
                                |> .currentDropdown
                                |> Expect.equal (Just StrokeColors)
                    ]
                , describe "and another dropdown is open"
                    [ test "opens the stroke colors dropdown" <|
                        \_ ->
                            initialState
                                |> withDropdown Fills
                                |> Controls.onKeyDown (key "K")
                                |> .currentDropdown
                                |> Expect.equal (Just StrokeColors)
                    ]
                ]
            , describe "when F is pressed"
                [ describe "and the fills dropdown is open"
                    [ test "closes the dropdown" <|
                        \_ ->
                            initialState
                                |> withDropdown Fills
                                |> Controls.onKeyDown (key "F")
                                |> .currentDropdown
                                |> Expect.equal Nothing
                    ]
                , describe "and the fills dropdown is closed"
                    [ test "opens the fills dropdown" <|
                        \_ ->
                            initialState
                                |> Controls.onKeyDown (key "F")
                                |> .currentDropdown
                                |> Expect.equal (Just Fills)
                    ]
                , describe "and another dropdown is open"
                    [ test "opens the fills dropdown" <|
                        \_ ->
                            initialState
                                |> withDropdown StrokeColors
                                |> Controls.onKeyDown (key "F")
                                |> .currentDropdown
                                |> Expect.equal (Just Fills)
                    ]
                ]
            , describe "when S is pressed"
                [ describe "and the strokes dropdown is open"
                    [ test "closes the dropdown" <|
                        \_ ->
                            initialState
                                |> withDropdown Strokes
                                |> Controls.onKeyDown (key "S")
                                |> .currentDropdown
                                |> Expect.equal Nothing
                    ]
                , describe "and the strokes dropdown is closed"
                    [ test "opens the strokes dropdown" <|
                        \_ ->
                            initialState
                                |> Controls.onKeyDown (key "S")
                                |> .currentDropdown
                                |> Expect.equal (Just Strokes)
                    ]
                , describe "and another dropdown is open"
                    [ test "opens the strokes dropdown" <|
                        \_ ->
                            initialState
                                |> withDropdown StrokeColors
                                |> Controls.onKeyDown (key "S")
                                |> .currentDropdown
                                |> Expect.equal (Just Strokes)
                    ]
                ]
            ]
        ]
