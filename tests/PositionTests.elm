module PositionTests exposing (suite)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (int)
import Position exposing (Position, angle, calcDistance, shift, step)
import Test exposing (..)
import TestUtil exposing (getFirstAnnotation, position)


fourtyFiveDeg : Int
fourtyFiveDeg =
    round (pi / 4)


suite : Test
suite =
    describe "Position module"
        [ describe "angle"
            [ fuzz2 position position "returns radians for a positive-only coordinate system" <|
                \pos1 pos2 ->
                    let
                        theta =
                            atan2 (toFloat (pos2.y - pos1.y)) (toFloat (pos2.x - pos1.x))

                        radians =
                            if theta < 0 then
                                (2 * pi) + theta

                            else
                                theta
                    in
                    Expect.within (Relative 0.1) radians (Position.angle pos1 pos2)
            ]
        , describe "calcDistance"
            [ fuzz2 position position "returns the straight line distance" <|
                \pos1 pos2 ->
                    let
                        distance =
                            sqrt <| toFloat <| (pos2.x - pos1.x) ^ 2 + (pos2.y - pos1.y) ^ 2
                    in
                    Expect.within (Relative 0.1) distance (calcDistance pos1 pos2)
            ]
        , describe "shift"
            [ fuzz3 int int position "shifts x and y" <|
                \dx dy pos ->
                    let
                        shifted =
                            shift dx dy pos
                    in
                    Expect.equal pos { shifted | x = shifted.x - dx, y = shifted.y - dy }
            ]
        , describe "step"
            [ fuzz2 position position "returns a multiple of 45 degrees" <|
                \pos1 pos2 ->
                    let
                        resultingAngle =
                            step pos1 pos2
                                |> angle pos1
                                |> round
                    in
                    Expect.equal 0 (modBy fourtyFiveDeg resultingAngle)
            , fuzz2 position position "returns roughly the same magnitude" <|
                \pos1 pos2 ->
                    step pos1 pos2
                        |> calcDistance pos1
                        |> Expect.within (Relative 0.25) (calcDistance pos1 pos2)
            ]
        , describe "toString"
            [ test "returns a co-ordinate pair separated by a comma" <|
                \_ ->
                    Position 0 1
                        |> Position.toString
                        |> Expect.equal "0,1"
            ]
        ]
