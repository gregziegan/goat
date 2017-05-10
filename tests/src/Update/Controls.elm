module Update.Controls exposing (all)

import Expect exposing (Expectation)
import Fixtures exposing (aTextArea, end, model, start)
import Goat.Annotation exposing (Drawing(..))
import Goat.Model exposing (AttributeDropdown(..))
import Goat.Update exposing (Msg(ChangeDrawing, CloseDropdown, ToggleDropdown), changeDrawing, toggleDropdown, update)
import Test exposing (..)


all : Test
all =
    describe "Control UI tests"
        [ toggleDropdownTests
        , controlsUpdateTests
        ]


toggleDropdownTests : Test
toggleDropdownTests =
    describe "toggleDropdown"
        [ test "shows supplied dropdown if no dropdown is currently showing" <|
            \() ->
                model
                    |> toggleDropdown Fills
                    |> .currentDropdown
                    |> Expect.equal (Just Fills)
        , test "removes dropdown if toggled with same dropdown as currentDropdown" <|
            \() ->
                model
                    |> toggleDropdown Fills
                    |> toggleDropdown Fills
                    |> .currentDropdown
                    |> Expect.equal Nothing
        , test "changes dropdown if toggled with different dropdown than currentDropdown" <|
            \() ->
                model
                    |> toggleDropdown Fills
                    |> toggleDropdown Strokes
                    |> .currentDropdown
                    |> Expect.equal (Just Strokes)
        , test "changes drawing to DrawTextBox if dropdown to toggle is Fonts" <|
            \() ->
                model
                    |> toggleDropdown Fonts
                    |> .drawing
                    |> Expect.equal DrawTextBox
        ]


controlsUpdateTests : Test
controlsUpdateTests =
    describe "Updates from Control UI Messages"
        [ test "ToggleDropdown has the same output as toggleDropdown" <|
            \() ->
                model
                    |> update (ToggleDropdown Fills)
                    |> Tuple.first
                    |> Expect.equal (toggleDropdown Fills model)
        , test "ChangeDrawing changes the drawing" <|
            \() ->
                model
                    |> update (ChangeDrawing DrawTextBox)
                    |> Tuple.first
                    |> .drawing
                    |> Expect.equal DrawTextBox
        , test "CloseDropdown sets currentDropdown to Nothing" <|
            \() ->
                model
                    |> changeDrawing DrawPixelate
                    |> update CloseDropdown
                    |> Tuple.first
                    |> .currentDropdown
                    |> Expect.equal Nothing
        ]
