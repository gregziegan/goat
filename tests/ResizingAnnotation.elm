module ResizingAnnotation exposing (all)

import Expect exposing (Expectation)
import Fixtures exposing (aShape, end, model, start)
import Goat.Model
    exposing
        ( Annotation(..)
        , AnnotationState(..)
        , Drawing(..)
        , Fill(..)
        , Line
        , LineMode(..)
        , LineType(..)
        , ResizingData
        , Shape
        , ShapeMode(..)
        , ShapeType(..)
        , Vertex(..)
        )
import Goat.Update exposing (addAnnotation, finishMovingAnnotation, getPositions, moveAnnotation, startMovingAnnotation, startResizingAnnotation)
import Test exposing (..)


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
        ]


startResizingTests : Test
startResizingTests =
    describe "startResizingAnnotation"
        [ test "should change the annotationState to ResizingAnnotation" <|
            \() ->
                model
                    |> addAnnotation (Shapes Rect aShape)
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
