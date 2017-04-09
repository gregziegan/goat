module DrawingAnnotations exposing (all)

import Expect exposing (Expectation)
import Fixtures exposing (end, model, start)
import Fuzz exposing (Fuzzer)
import Goat.Model
    exposing
        ( Annotation(..)
        , Drawing(..)
        , Line
        , LineMode(..)
        , LineType(..)
        , Fill(..)
        , Shape
        , ShapeMode(..)
        , ShapeType(..)
        )
import Goat.Update exposing (finishLineDrawing, finishShapeDrawing, finishSpotlightDrawing)
import Test exposing (..)
import TestUtil exposing (getFirstAnnotation)


all : Test
all =
    describe "drawing"
        [ finishDrawingTests ]


finishDrawingTests : Test
finishDrawingTests =
    describe "finishDrawing"
        [ test "should add a line annotation to the edit history" <|
            \() ->
                model
                    |> finishLineDrawing start end StraightLine DrawingLine
                    |> getFirstAnnotation
                    |> Maybe.map (Expect.equal (Lines StraightLine <| Line start end model.strokeColor model.strokeStyle))
                    |> Maybe.withDefault (Expect.fail "Array missing line annotation")
        , test "should add an arrow annotation to the edit history" <|
            \() ->
                model
                    |> finishLineDrawing start end Arrow DrawingLine
                    |> getFirstAnnotation
                    |> Maybe.map (Expect.equal (Lines Arrow <| Line start end model.strokeColor model.strokeStyle))
                    |> Maybe.withDefault (Expect.fail "Array missing arrow annotation")
        , test "should add a shape annotation to the edit history" <|
            \() ->
                model
                    |> finishShapeDrawing start end Rect DrawingShape
                    |> getFirstAnnotation
                    |> Maybe.map (Expect.equal (Shapes Rect <| Shape start end model.fill model.strokeColor model.strokeStyle))
                    |> Maybe.withDefault (Expect.fail "Array missing rect annotation")
        , test "should add a spotlight annotation with a spotlight fill to the edit history" <|
            \() ->
                model
                    |> finishSpotlightDrawing start end Rect DrawingShape
                    |> getFirstAnnotation
                    |> Maybe.map (Expect.equal (Spotlight Rect <| Shape start end SpotlightFill model.strokeColor model.strokeStyle))
                    |> Maybe.withDefault (Expect.fail "Array missing spotlight rect annotation")
        ]
