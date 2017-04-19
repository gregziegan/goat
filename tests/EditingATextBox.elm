module EditingATextBox exposing (all)

import Array.Hamt as Array
import Expect exposing (Expectation)
import Fixtures exposing (aShape, aTextArea, autoExpand, end, model, start)
import Goat.Model exposing (Annotation(..), AnnotationState(..), Drawing(..), LineType(..), Shape, ShapeType(..))
import Goat.Update exposing (addAnnotation, autoExpandAnnotation, editTextBoxAnnotation, finishEditingText, startEditingText)
import Goat.Helpers exposing (currentAnnotationAttributes)
import Test exposing (..)
import TestUtil exposing (getFirstAnnotation)


all : Test
all =
    describe "EditingATextBox state"
        [ startEditingTextTests
        , finishEditingTextTests
        ]


startEditingTextTests : Test
startEditingTextTests =
    describe "startEditingText"
        [ test "should set state to EditingATextBox" <|
            \() ->
                model
                    |> addAnnotation (TextBox aTextArea)
                    |> startEditingText 0
                    |> .annotationState
                    |> Expect.equal (EditingATextBox 0 (currentAnnotationAttributes model))
        ]


finishEditingTextTests : Test
finishEditingTextTests =
    describe "switchToEditingText"
        [ test "should set state to ReadyToDraw and keep modified textbox if text is not empty" <|
            \() ->
                model
                    |> addAnnotation (TextBox aTextArea)
                    |> startEditingText 0
                    |> editTextBoxAnnotation 0 autoExpand "goats"
                    |> finishEditingText 0
                    |> Expect.all
                        [ Expect.equal ReadyToDraw << .annotationState
                        , Expect.equal (Just (autoExpandAnnotation autoExpand "goats" (TextBox aTextArea))) << getFirstAnnotation
                        ]
        , test "should remove textbox if text is empty" <|
            \() ->
                model
                    |> addAnnotation (TextBox aTextArea)
                    |> startEditingText 0
                    |> editTextBoxAnnotation 0 autoExpand ""
                    |> finishEditingText 0
                    |> .edits
                    |> .present
                    |> Array.isEmpty
                    |> Expect.true "the empty textbox should be gone!"
        ]
