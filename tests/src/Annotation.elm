module Annotation exposing (all)

import Array.Hamt as Array
import Expect exposing (Expectation)
import Fixtures exposing (aShape, drawingInfo, end, model, start, testColor)
import Goat.Annotation as Annotation
import Goat.EditState exposing (updateAnySelectedAnnotations)
import Goat.Model exposing (Model)
import Goat.Update exposing (Msg(..), update, extractAnnotationAttributes, selectAnnotation)
import Test exposing (..)
import TestUtil exposing (getFirstAnnotation)


all : Test
all =
    describe "SelectedAnnotation Updates"
        [ updateAnySelectedAnnotationsTests
        , bringToFrontTests
        , sendToBackTests
        ]



-- initAnnotation : Model -> Annotation.Annotation


initAnnotation model =
    Annotation.fromDrawing False model.drawing (extractAnnotationAttributes model) drawingInfo


updateAnySelectedAnnotationsTests : Test
updateAnySelectedAnnotationsTests =
    describe "updateAnySelectedAnnotations"
        [ test "updates a Selected annotation" <|
            \() ->
                model
                    -- |> update (SelectStrokeColor Color.red)
                    |> update (StartDrawing start)
                    |> update (FinishDrawing end)
                    |> update (SelectAnnotation 0)
                    -- |> addAnnotation initAnnotation
                    -- |> selectAnnotation 0
                    -- |> updateAnySelectedAnnotations (updateStrokeColor testColor)
                    |> getFirstAnnotation
                    |> Expect.equal (initAnnotation model)
        , test "does not update a NotSelected annotation" <|
            \() ->
                model
                    |> addAnnotation (Lines Arrow aShape)
                    |> updateAnySelectedAnnotations (updateStrokeStyle DashedMedium)
                    |> getFirstAnnotation
                    |> Expect.equal (Just (Lines Arrow aShape))
        , test "only updates the Selected annotation" <|
            \() ->
                model
                    |> addAnnotation (Lines Arrow aShape)
                    |> addAnnotation (Shapes Rect (Just testColor) aShape)
                    |> addAnnotation (Shapes Ellipse (Just testColor) aShape)
                    |> selectAnnotation 1
                    |> updateAnySelectedAnnotations (updateFill (Just testColor))
                    |> .edits
                    |> .present
                    |> Array.get 2
                    |> Expect.equal (Just (Shapes Ellipse (Just testColor) aShape))
        ]


bringToFrontTests : Test
bringToFrontTests =
    describe "bringAnnotationToFront" <|
        [ test "pushes selected annotation to end of array" <|
            \() ->
                model
                    |> addAnnotation (Lines Arrow aShape)
                    |> addAnnotation (Shapes Rect Nothing aShape)
                    |> bringAnnotationToFront 0
                    |> .edits
                    |> .present
                    |> Array.get 1
                    |> Expect.equal (Just (Lines Arrow aShape))
        ]


sendToBackTests : Test
sendToBackTests =
    describe "sendAnnotationToBack"
        [ test "brings annotation to front of array" <|
            \() ->
                model
                    |> addAnnotation (Lines Arrow aShape)
                    |> addAnnotation (Shapes Rect Nothing aShape)
                    |> sendAnnotationToBack 1
                    |> getFirstAnnotation
                    |> Expect.equal (Just (Shapes Rect Nothing aShape))
        ]



--
-- updateAnySelectedAnnotationsTests : Test
-- updateAnySelectedAnnotationsTests =
--     describe "updateAnySelectedAnnotations"
--         [ test "updates a Selected annotation" <|
--             \() ->
--                 model
--                     -- |> update (SelectStrokeColor Color.red)
--                     |> update (StartDrawing start)
--                     |> update (FinishDrawing end)
--                     |> update (SelectAnnotation 0)
--                     -- |> addAnnotation initAnnotation
--                     -- |> selectAnnotation 0
--                     -- |> updateAnySelectedAnnotations (updateStrokeColor testColor)
--                     |> getFirstAnnotation
--                     |> Expect.equal (initAnnotation model)
--         , test "does not update a NotSelected annotation" <|
--             \() ->
--                 model
--                     |> addAnnotation (Lines Arrow aShape)
--                     |> updateAnySelectedAnnotations (updateStrokeStyle DashedMedium)
--                     |> getFirstAnnotation
--                     |> Expect.equal (Just (Lines Arrow aShape))
--         , test "only updates the Selected annotation" <|
--             \() ->
--                 model
--                     |> addAnnotation (Lines Arrow aShape)
--                     |> addAnnotation (Shapes Rect (Just testColor) aShape)
--                     |> addAnnotation (Shapes Ellipse (Just testColor) aShape)
--                     |> selectAnnotation 1
--                     |> updateAnySelectedAnnotations (updateFill (Just testColor))
--                     |> .edits
--                     |> .present
--                     |> Array.get 2
--                     |> Expect.equal (Just (Shapes Ellipse (Just testColor) aShape))
--         ]
--
--
-- bringToFrontTests : Test
-- bringToFrontTests =
--     describe "bringAnnotationToFront" <|
--         [ test "pushes selected annotation to end of array" <|
--             \() ->
--                 model
--                     |> addAnnotation (Lines Arrow aShape)
--                     |> addAnnotation (Shapes Rect Nothing aShape)
--                     |> bringAnnotationToFront 0
--                     |> .edits
--                     |> .present
--                     |> Array.get 1
--                     |> Expect.equal (Just (Lines Arrow aShape))
--         ]
--
--
-- sendToBackTests : Test
-- sendToBackTests =
--     describe "sendAnnotationToBack"
--         [ test "brings annotation to front of array" <|
--             \() ->
--                 model
--                     |> addAnnotation (Lines Arrow aShape)
--                     |> addAnnotation (Shapes Rect Nothing aShape)
--                     |> sendAnnotationToBack 1
--                     |> getFirstAnnotation
--                     |> Expect.equal (Just (Shapes Rect Nothing aShape))
--         ]
