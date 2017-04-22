module DrawingAnnotation exposing (all)

import Expect exposing (Expectation)
import Fixtures exposing (end, model, start)
import Goat.Model
    exposing
        ( Annotation(..)
        , AnnotationState(..)
        , Drawing(..)
        , LineType(..)
        , Shape
        , ShapeType(..)
        )
import Goat.Update exposing (continueDrawing, finishLineDrawing, finishPixelateDrawing, finishShapeDrawing, finishSpotlightDrawing, startDrawing)
import Test exposing (..)
import TestUtil exposing (getFirstAnnotation, getDrawingStateCurPos)


all : Test
all =
    describe "drawing"
        [ startDrawingTests
        , continueDrawingTests
        , finishDrawingTests
        ]


startDrawingTests : Test
startDrawingTests =
    describe "startDrawing"
        [ test "should change the annotationState to DrawingAnnotation" <|
            \() ->
                model
                    |> startDrawing start
                    |> .annotationState
                    |> Expect.equal (DrawingAnnotation start start)
        ]


continueDrawingTests : Test
continueDrawingTests =
    describe "continueDrawing"
        [ test "should update the current mouse position in DrawingAnnotation" <|
            \() ->
                model
                    |> startDrawing start
                    |> continueDrawing end
                    |> .annotationState
                    |> Expect.equal (DrawingAnnotation start end)
        , test "should not update the mouse position if not drawing an annotation" <|
            \() ->
                model
                    |> continueDrawing end
                    |> .annotationState
                    |> getDrawingStateCurPos
                    |> Expect.equal Nothing
        ]


finishDrawingTests : Test
finishDrawingTests =
    describe "finishDrawing"
        [ test "should add a line annotation to the edit history" <|
            \() ->
                model
                    |> finishLineDrawing start end StraightLine
                    |> getFirstAnnotation
                    |> Maybe.map (Expect.equal (Lines StraightLine <| Shape start end model.strokeColor model.strokeStyle))
                    |> Maybe.withDefault (Expect.fail "Array missing line annotation")
        , test "should add an arrow annotation to the edit history" <|
            \() ->
                model
                    |> finishLineDrawing start end Arrow
                    |> getFirstAnnotation
                    |> Maybe.map (Expect.equal (Lines Arrow <| Shape start end model.strokeColor model.strokeStyle))
                    |> Maybe.withDefault (Expect.fail "Array missing arrow annotation")
        , test "should add a shape annotation to the edit history" <|
            \() ->
                model
                    |> finishShapeDrawing start end Rect
                    |> getFirstAnnotation
                    |> Maybe.map (Expect.equal (Shapes Rect model.fill <| Shape start end model.strokeColor model.strokeStyle))
                    |> Maybe.withDefault (Expect.fail "Array missing rect annotation")
        , test "should add a spotlight annotation with a spotlight fill to the edit history" <|
            \() ->
                model
                    |> finishSpotlightDrawing start end Rect
                    |> getFirstAnnotation
                    |> Maybe.map (Expect.equal (Spotlight Rect <| Shape start end model.strokeColor model.strokeStyle))
                    |> Maybe.withDefault (Expect.fail "Array missing spotlight rect annotation")
        , test "should add a pixelate annotation to the edit history" <|
            \() ->
                model
                    |> finishPixelateDrawing start end
                    |> getFirstAnnotation
                    |> Expect.equal (Just (Pixelate start end))
        ]
