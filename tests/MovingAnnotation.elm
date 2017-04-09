module MovingAnnotation exposing (all)

import Expect exposing (Expectation)
import Fixtures exposing (end, model, start, aShape)
import Goat.Model
    exposing
        ( Annotation(..)
        , AnnotationState(..)
        , Drawing(..)
        , Fill(..)
        , Line
        , LineMode(..)
        , LineType(..)
        , Shape
        , ShapeMode(..)
        , ShapeType(..)
        )
import Goat.Update exposing (addAnnotation, startMovingAnnotation)
import Test exposing (..)


all : Test
all =
    describe "drawing"
        [ startMovingTests ]


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
