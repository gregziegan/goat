module Update.EditingATextBox exposing (all)

import Array.Hamt as Array
import Expect exposing (Expectation)
import Fixtures exposing (aShape, aTextArea, autoExpand, end, model, start)
import Goat.Model exposing (Annotation(..), AnnotationState(..), Drawing(..), LineType(..), Shape, ShapeType(..), Annotation(TextBox))
import Goat.Update exposing (addAnnotation, autoExpandAnnotation, editTextBoxAnnotation, finishEditingText, startEditingText)
import Goat.Utils exposing (currentAnnotationAttributes)
import Test exposing (..)
import TestUtil exposing (getAnnotationText, getFirstAnnotation)


all : Test
all =
    describe "EditingATextBox state"
        [ startEditingTextTests
        , editTextBoxTests
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


editTextBoxTests : Test
editTextBoxTests =
    describe "editTextBoxAnnotation"
        [ test "editing a textbox's text should not add to the undo history" <|
            \() ->
                model
                    |> addAnnotation (TextBox aTextArea)
                    |> startEditingText 0
                    |> editTextBoxAnnotation 0 aTextArea.autoexpand "New Text"
                    |> .edits
                    |> .past
                    |> List.length
                    |> Expect.equal 1
        , test "editing a textbox's test should update the texbox's text field" <|
            \() ->
                model
                    |> addAnnotation (TextBox aTextArea)
                    |> startEditingText 0
                    |> editTextBoxAnnotation 0 aTextArea.autoexpand "New Text"
                    |> getFirstAnnotation
                    |> Maybe.map (Expect.equal (Just "New Text") << getAnnotationText)
                    |> Maybe.withDefault (Expect.fail "Annotation does not exist or is the wrong type")
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
