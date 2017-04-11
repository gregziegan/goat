module ResizingAnnotation exposing (all)

import Expect exposing (Expectation)
import Fixtures exposing (aShape, end, model, start)
import Goat.Model exposing (Annotation(..), AnnotationState(..), Drawing(..), LineMode(..), LineType(..), ResizingData, AnnotationState(SelectedAnnotation), Shape, ShapeMode(..), ShapeType(..), Vertex(..))
import Goat.Update exposing (addAnnotation, finishMovingAnnotation, finishResizingAnnotation, getPositions, moveAnnotation, resize, resizeAnnotation, startMovingAnnotation, startResizingAnnotation)
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
                    |> Expect.equal (ResizingAnnotation resizingData)
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
                    |> Expect.equal (ResizingAnnotation { resizingData | curPos = end })
        , test "should resize annotation by updating its start and end with resize" <|
            \() ->
                model
                    |> addAnnotation (Lines Arrow aShape)
                    |> startResizingAnnotation 0 Start start
                    |> resizeAnnotation end
                    |> getFirstAnnotation
                    |> Maybe.map (Expect.equal (resize { resizingData | curPos = end } (Lines Arrow aShape)))
                    |> Maybe.withDefault (Expect.fail "resized annotation is missing!")
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
                    |> Expect.equal (SelectedAnnotation 0)
        , test "should resize annotation by updating its start and end with resize fn" <|
            \() ->
                model
                    |> addAnnotation (Lines Arrow aShape)
                    |> startResizingAnnotation 0 Start start
                    |> resizeAnnotation end
                    |> finishResizingAnnotation
                    |> getFirstAnnotation
                    |> Maybe.map (Expect.equal (resize { resizingData | curPos = end } (Lines Arrow aShape)))
                    |> Maybe.withDefault (Expect.fail "resized annotation is missing!")
        ]
