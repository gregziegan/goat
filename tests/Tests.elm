module Tests exposing (..)

import Array.Hamt as Array exposing (Array)
import DrawingAnnotations
import Expect
import Fixtures exposing (..)
import Goat.Helpers exposing (..)
import Goat.Model exposing (..)
import Goat.Update exposing (..)
import Test exposing (..)
import TestUtil exposing (getFirstAnnotation)
import View.Annotation


all : Test
all =
    describe "Annotation App Suite"
        [ DrawingAnnotations.all
        , describe "updateAnySelectedAnnotations"
            [ test "updates a Selected annotation" <|
                \() ->
                    model
                        |> addAnnotation (Lines Arrow aLine)
                        |> selectAnnotation 0
                        |> updateAnySelectedAnnotations (updateStrokeColor testColor)
                        |> getFirstAnnotation
                        |> Maybe.map (Expect.equal (Lines Arrow <| Line start end testColor model.strokeStyle))
                        |> Maybe.withDefault (Expect.fail "Array missing desired annotation")
            , test "does not update a NotSelected annotation" <|
                \() ->
                    model
                        |> addAnnotation (Lines Arrow aLine)
                        |> updateAnySelectedAnnotations (updateStrokeStyle DashedMedium)
                        |> getFirstAnnotation
                        |> Maybe.map (Expect.equal (Lines Arrow aLine))
                        |> Maybe.withDefault (Expect.fail "Array missing desired annotation")
            , test "only updates the Selected annotation" <|
                \() ->
                    model
                        |> addAnnotation (Lines Arrow aLine)
                        |> addAnnotation (Shapes Rect aShape)
                        |> addAnnotation (Shapes Ellipse aShape)
                        |> selectAnnotation 1
                        |> updateAnySelectedAnnotations (updateFill (SolidFill testColor))
                        |> .edits
                        |> .present
                        |> Array.get 2
                        |> Maybe.map (Expect.equal (Shapes Ellipse aShape))
                        |> Maybe.withDefault (Expect.fail "Array missing desired annotation")
            ]
        , View.Annotation.all
        , describe "Helpers"
            [ fuzz2 position position "mouse step function works properly" <|
                \pos1 pos2 ->
                    stepMouse pos1 pos2
                        |> arrowAngle pos1
                        |> round
                        |> flip (%) (round (pi / 4))
                        |> Expect.equal 0
            ]
        ]
