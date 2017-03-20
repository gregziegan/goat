module Tests exposing (..)

import Color
import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer)
import Annotator exposing (..)
import Mouse exposing (Position)
import Random.Pcg as Random
import Shrink


doNotTrackMouseStates : List Drawing
doNotTrackMouseStates =
    [ DrawTextBox <| EditingText (TextBox { x = 0, y = 0 } { x = 0, y = 0 } "" Color.red VeryThin 0.0 0.0) ]


position : Fuzzer Position
position =
    Fuzz.custom
        (Random.map2 Position (Random.int -100 100) (Random.int -100 100))
        (\{ x, y } -> Shrink.map Position (Shrink.int x) |> Shrink.andMap (Shrink.int y))


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
