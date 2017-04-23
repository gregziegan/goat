module Update.ResizingAnnotation exposing (all)

import Expect exposing (Expectation)
import Fixtures exposing (aShape, end, model, start)
import Goat.Model exposing (Annotation(..), AnnotationState(..), Drawing(..), LineType(..), ResizingData, AnnotationState(SelectedAnnotation), Shape, ShapeType(..), Vertex(..))
import Goat.Update exposing (addAnnotation, finishMovingAnnotation, finishResizingAnnotation, moveAnnotation, resize, resizeAnnotation, startMovingAnnotation, startResizingAnnotation)
import Goat.Utils exposing (currentAnnotationAttributes)
import Test exposing (..)
import TestUtil exposing (getFirstAnnotation)


resizingData : ResizingData
resizingData =
    { index = 0
    , start = start
    , curPos = start
    , vertex = Start
    , originalCoords = ( start, end )
    }


all : Test
all =
    describe "ResizingAnnotation state"
        [ startResizingTests
        , resizeAnnotationTests
        , finishResizingAnnotationTests
        ]


startResizingTests : Test
startResizingTests =
    describe "startResizingAnnotation"
        [ test "should change the annotationState to ResizingAnnotation" <|
            \() ->
                model
                    |> addAnnotation (Lines Arrow aShape)
                    |> startResizingAnnotation 0 Start start
                    |> .annotationState
                    |> Expect.equal (ResizingAnnotation resizingData (currentAnnotationAttributes model))
        , test "should not change the annotationState to ResizingAnnotation if annotation doesn't exist" <|
            \() ->
                model
                    |> startResizingAnnotation 0 Start start
                    |> .annotationState
                    |> Expect.equal model.annotationState
        ]


resizeAnnotationTests : Test
resizeAnnotationTests =
    describe "resizeAnnotation"
        [ test "should update the ResizingAnnotation state with new position" <|
            \() ->
                model
                    |> addAnnotation (Lines Arrow aShape)
                    |> startResizingAnnotation 0 Start start
                    |> resizeAnnotation end
                    |> .annotationState
                    |> Expect.equal (ResizingAnnotation { resizingData | curPos = end } (currentAnnotationAttributes model))
        , test "should resize annotation by updating its start and end with resize" <|
            \() ->
                model
                    |> addAnnotation (Lines Arrow aShape)
                    |> startResizingAnnotation 0 Start start
                    |> resizeAnnotation end
                    |> getFirstAnnotation
                    |> Expect.equal (Just (resize False { resizingData | curPos = end } (Lines Arrow aShape)))
        ]


finishResizingAnnotationTests : Test
finishResizingAnnotationTests =
    describe "finishResizingAnnotation"
        [ test "should set the annotationState to SelectedAnnotation" <|
            \() ->
                model
                    |> addAnnotation (Lines Arrow aShape)
                    |> startResizingAnnotation 0 Start start
                    |> resizeAnnotation end
                    |> finishResizingAnnotation
                    |> .annotationState
                    |> Expect.equal (SelectedAnnotation 0 (currentAnnotationAttributes model))
        , test "should resize annotation by updating its start and end with resize fn" <|
            \() ->
                model
                    |> addAnnotation (Lines Arrow aShape)
                    |> startResizingAnnotation 0 Start start
                    |> resizeAnnotation end
                    |> finishResizingAnnotation
                    |> getFirstAnnotation
                    |> Expect.equal (Just (resize False { resizingData | curPos = end } (Lines Arrow aShape)))
        ]
