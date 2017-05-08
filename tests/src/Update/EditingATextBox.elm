module Update.EditingATextBox exposing (all)

import Array.Hamt as Array
import Expect exposing (Expectation)
import Fixtures exposing (aShape, aTextArea, autoExpand, end, model, start)
import Goat.Annotation exposing (Annotation(..), LineType(..), Shape, ShapeType(..))
import Goat.EditState as EditState
import Goat.Model exposing (Drawing(..))
import Goat.Update exposing (addAnnotation, editTextBoxAnnotation, finishEditingText, startEditingText, annotationAttributesInModel)
import Test exposing (..)
import TestUtil exposing (getAnnotationText, getFirstAnnotation)


all : Test
all =
    describe "EditingATextBox state"
        [ startEditingTextTests
        , editTextBoxTests
        , finishEditingTextTests
        ]


curAttributes =
    annotationAttributesInModel model


startEditingTextTests : Test
startEditingTextTests =
    describe "startEditingText"
        [ test "should set state to EditingATextBox" <|
            \() ->
                model
                    |> addAnnotation (TextBox aTextArea)
                    |> startEditingText 0
                    |> .editState
                    |> EditState.whenEditingText
                        (Expect.equal { id = 0, attributes = curAttributes })
                        (Expect.fail "Should be editing text")
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
                    |> EditState.whenNotSelecting
                        (Expect.equal (Just (Annotation.autoExpand autoExpand "goats" (TextBox aTextArea))) << getFirstAnnotation)
                        (Expect.fail "should return to not selecting")
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
