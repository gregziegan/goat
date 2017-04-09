module MovingAnnotation exposing (all)

import Expect exposing (Expectation)
import Fixtures exposing (aShape, end, model, start)
import Goat.Model exposing (Annotation(..), AnnotationState(..), Drawing(..), Fill(..), Line, LineMode(..), LineType(..), Shape, ShapeMode(..), ShapeType(..))
import Goat.Update exposing (addAnnotation, finishMovingAnnotation, getPositions, moveAnnotation, startMovingAnnotation)
import Test exposing (..)
import TestUtil exposing (getFirstAnnotation, isAnnotationMovedByCorrectAmount)


all : Test
all =
    describe "MovingAnnotation state"
        [ startMovingTests
        , moveAnnotationTests
        , finishMovingAnnotationTests
        ]


startMovingTests : Test
startMovingTests =
    describe "startMovingAnnotation"
        [ test "should change the annotationState to MovingAnnotation" <|
            \() ->
                model
                    |> addAnnotation (Shapes Rect aShape)
                    |> startMovingAnnotation 0 start
                    |> .annotationState
                    |> Expect.equal (MovingAnnotation 0 start ( 0, 0 ))
        , test "should not change the annotationState to MovingAnnotation if annotation doesn't exist" <|
            \() ->
                model
                    |> startMovingAnnotation 0 start
                    |> .annotationState
                    |> Expect.equal model.annotationState
        ]


moveAnnotationTests : Test
moveAnnotationTests =
    describe "moveAnnotation"
        [ test "should change the annotationState to MovingAnnotation" <|
            \() ->
                model
                    |> addAnnotation (Shapes Rect aShape)
                    |> startMovingAnnotation 0 start
                    |> moveAnnotation end
                    |> .annotationState
                    |> Expect.equal (MovingAnnotation 0 start ( end.x - start.x, end.y - start.y ))
        ]


finishMovingAnnotationTests : Test
finishMovingAnnotationTests =
    describe "finishMovingAnnotation"
        [ test "should set the annotationState to SelectedAnnotation" <|
            \() ->
                model
                    |> addAnnotation (Shapes Rect aShape)
                    |> startMovingAnnotation 0 start
                    |> finishMovingAnnotation
                    |> .annotationState
                    |> Expect.equal (SelectedAnnotation 0)
        , test "should move the original annotation by the correct amount" <|
            \() ->
                model
                    |> addAnnotation (Shapes Rect aShape)
                    |> startMovingAnnotation 0 start
                    |> moveAnnotation end
                    |> finishMovingAnnotation
                    |> getFirstAnnotation
                    |> Maybe.map (Expect.true "annotation shifted properly" << isAnnotationMovedByCorrectAmount start end ( aShape.start, aShape.end ))
                    |> Maybe.withDefault (Expect.fail "moved annotation is missing!")
        ]
