module Utils exposing (all)

import Expect exposing (Expectation)
import Goat.Utils exposing (stepMouse, arrowAngle)
import Test exposing (..)
import TestUtil exposing (getFirstAnnotation, position)


all : Test
all =
    describe "Utils module"
        [ fuzz2 position position "mouse step function works properly" <|
            \pos1 pos2 ->
                stepMouse pos1 pos2
                    |> arrowAngle pos1
                    |> round
                    |> flip (%) (round (pi / 4))
                    |> Expect.equal 0
        ]
