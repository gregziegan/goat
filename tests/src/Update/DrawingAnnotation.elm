module Update.DrawingAnnotation exposing (all)

import Array.Hamt as Array
import Expect exposing (Expectation)
import Fixtures exposing (end, model, start, aTextArea, firstFreeDrawPosition, secondFreeDrawPosition)
import Goat.Model exposing (Annotation(..), AnnotationState(..), Drawing(..), LineType(..), Shape, ShapeType(..))
import Goat.Update exposing (changeDrawing, continueDrawing, finishDrawing, finishLineDrawing, finishPixelateDrawing, finishTextBoxDrawing, finishShapeDrawing, finishSpotlightDrawing, startDrawing, finishFreeDrawing)
import Goat.Utils exposing (shiftPosition)
import Test exposing (..)
import TestUtil exposing (getDrawingStateCurPos, getFirstAnnotation)


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
        , test "should add mouse positions to the free draw path list" <|
            \() ->
                model
                    |> changeDrawing DrawFreeHand
                    |> startDrawing start
                    |> continueDrawing firstFreeDrawPosition
                    |> continueDrawing secondFreeDrawPosition
                    |> .freeDrawPositions
                    |> Expect.equalLists [ secondFreeDrawPosition, firstFreeDrawPosition ]
        ]


finishDrawingTests : Test
finishDrawingTests =
    describe "finishDrawing"
        [ test "should add a line annotation to the edit history" <|
            \() ->
                model
                    |> finishLineDrawing start end StraightLine
                    |> getFirstAnnotation
                    |> Expect.equal (Just (Lines StraightLine (Shape start end model.strokeColor model.strokeStyle)))
        , test "should add an arrow annotation to the edit history" <|
            \() ->
                model
                    |> finishLineDrawing start end Arrow
                    |> getFirstAnnotation
                    |> Expect.equal (Just (Lines Arrow (Shape start end model.strokeColor model.strokeStyle)))
        , test "should add a free hand draw annotation to the edit history" <|
            \() ->
                model
                    |> finishFreeDrawing start end [ secondFreeDrawPosition, firstFreeDrawPosition ]
                    |> getFirstAnnotation
                    |> Expect.equal (Just (FreeDraw (Shape start end model.strokeColor model.strokeStyle) [ secondFreeDrawPosition, firstFreeDrawPosition ]))
        , test "should empty free drawing positions list" <|
            \() ->
                model
                    |> finishFreeDrawing start end [ secondFreeDrawPosition, firstFreeDrawPosition ]
                    |> .freeDrawPositions
                    |> Expect.equal []
        , test "should add a shape annotation to the edit history" <|
            \() ->
                model
                    |> finishShapeDrawing start end Rect
                    |> getFirstAnnotation
                    |> Expect.equal (Just (Shapes Rect model.fill (Shape start end model.strokeColor model.strokeStyle)))
        , test "should add a textbox annotation to the edit history " <|
            \() ->
                model
                    |> finishTextBoxDrawing start end
                    |> getFirstAnnotation
                    |> Expect.equal (Just (TextBox aTextArea))
        , test "should add a spotlight annotation with a spotlight fill to the edit history" <|
            \() ->
                model
                    |> finishSpotlightDrawing start end Rect
                    |> getFirstAnnotation
                    |> Expect.equal (Just (Spotlight Rect (Shape start end model.strokeColor model.strokeStyle)))
        , test "should add a pixelate annotation to the edit history" <|
            \() ->
                model
                    |> finishPixelateDrawing start end
                    |> getFirstAnnotation
                    |> Expect.equal (Just (Pixelate start end))
        , test "should cancel drawing if it is too small" <|
            \() ->
                model
                    |> changeDrawing (DrawLine Arrow)
                    |> startDrawing start
                    |> continueDrawing (shiftPosition 1 1 start)
                    |> finishDrawing (shiftPosition 2 2 start)
                    |> Tuple.first
                    |> .edits
                    |> .present
                    |> Array.isEmpty
                    |> Expect.true "drawing should not be added to array"
        , test "should cancel drawing if it is too small, with more tolerance for spotlights" <|
            \() ->
                model
                    |> changeDrawing (DrawSpotlight Rect)
                    |> startDrawing start
                    |> continueDrawing (shiftPosition 4 4 start)
                    |> finishDrawing (shiftPosition 6 6 start)
                    |> Tuple.first
                    |> .edits
                    |> .present
                    |> Array.isEmpty
                    |> Expect.true "spotlight drawing should not be added to array"
        ]
