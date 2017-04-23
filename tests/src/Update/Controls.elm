module Update.Controls exposing (all)

import Expect exposing (Expectation)
import Fixtures exposing (aTextArea, end, model, start)
import Goat.Model exposing (AttributeDropdown(..), Drawing(..))
import Goat.Update exposing (toggleDropdown)
import Test exposing (..)


all : Test
all =
    describe "Control UI tests"
        [ toggleDropdownTests
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
