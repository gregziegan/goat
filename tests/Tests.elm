module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer)
import Annotator exposing (..)
import Random.Pcg as Random
import Shrink


doNotTrackMouseStates : List Drawing
doNotTrackMouseStates =
    [ DrawRect NoRect, DrawRoundedRect NoRoundedRect, DrawEllipse NoEllipse, DrawArrow NoArrow, DrawLine NoLine, DrawTextBox NoText, Selection ]


position : Fuzzer Position
position =
    Fuzz.custom
        (Random.map2 (,) (Random.float -100.0 100.0) (Random.float -100.0 100.0))
        (\( x, y ) -> Shrink.map (,) (Shrink.float x) |> Shrink.andMap (Shrink.float y))


all : Test
all =
    describe "Annotation App Suite"
        [ describe "Utils"
            [ test "does not track mouse when unnecessary" <|
                \() ->
                    doNotTrackMouseStates
                        |> List.all (not << trackMouse)
                        |> Expect.true "draw state should not be tracking mouse position"
            , fuzz2 position position "mouse step function works properly" <|
                \pos1 pos2 ->
                    stepMouse pos1 pos2
                        |> arrowAngle pos1
                        |> round
                        |> flip (%) (round (pi / 4))
                        |> Expect.equal 0
            ]
        ]
