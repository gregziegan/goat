module Tests exposing (..)

import Test exposing (..)
import Expect
import Annotator exposing (..)


doNotTrackMouseStates : List Drawing
doNotTrackMouseStates =
    [ DrawRect NoRect, DrawRoundedRect NoRoundedRect, DrawEllipse NoEllipse, DrawArrow NoArrow, DrawLine NoLine, DrawTextBox NoText, Selection ]


all : Test
all =
    describe "Annotation App Suite"
        [ describe "Utils"
            [ test "does not track mouse when unnecessary" <|
                \() ->
                    doNotTrackMouseStates
                        |> List.all (not << trackMouse)
                        |> Expect.true "draw state should not be tracking mouse position"
            ]
        ]
