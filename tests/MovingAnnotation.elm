module MovingAnnotation exposing (all)

import Expect exposing (Expectation)
import Fixtures exposing (aShape, end, model, start)
import Goat.Model exposing (Annotation(..), AnnotationState(..), Drawing(..), LineMode(..), LineType(..), Shape, ShapeMode(..), ShapeType(..))
import Goat.Update exposing (addAnnotation, finishMovingAnnotation, move, moveAnnotation, startMovingAnnotation)
import Goat.Helpers exposing (getPositions, shiftPosition)
import Test exposing (..)
import TestUtil exposing (getFirstAnnotation, isAnnotationMovedByCorrectAmount)


all : Test
all =
    describe "MovingAnnotation state"
        [ startMovingTests
        , moveTests
        , moveAnnotationTests
        , finishMovingAnnotationTests
        ]


startMovingTests : Test
startMovingTests =
    describe "startMovingAnnotation"
        [ test "should change the annotationState to MovingAnnotation" <|
            \() ->
                model
                    |> addAnnotation (Lines Arrow aShape)
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


moveTests : Test
moveTests =
    describe "move"
        [ test "should move an annotation by the correct amount" <|
            \() ->
                let
                    dx =
                        end.x - start.x

                    dy =
                        end.y - start.y
                in
                    (Lines Arrow aShape)
                        |> move ( dx, dy )
                        |> getPositions
                        |> Expect.equal ( shiftPosition dx dy aShape.start, shiftPosition dx dy aShape.end )
        ]


moveAnnotationTests : Test
moveAnnotationTests =
    describe "moveAnnotation"
        [ test "should update the MovingAnnotation state with new position" <|
            \() ->
                model
                    |> addAnnotation (Lines Arrow aShape)
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
                    |> addAnnotation (Lines Arrow aShape)
                    |> startMovingAnnotation 0 start
                    |> finishMovingAnnotation
                    |> .annotationState
                    |> Expect.equal (SelectedAnnotation 0)
        , test "should move annotation by updating its start and end with move fn" <|
            \() ->
                model
                    |> addAnnotation (Lines Arrow aShape)
                    |> startMovingAnnotation 0 start
                    |> moveAnnotation end
                    |> finishMovingAnnotation
                    |> getFirstAnnotation
                    |> Maybe.map (Expect.equal (move ( end.x - start.x, end.y - start.y ) (Lines Arrow aShape)))
                    |> Maybe.withDefault (Expect.fail "moved annotation is missing!")
        ]
