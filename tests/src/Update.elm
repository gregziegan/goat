module Update exposing (all)

import Array exposing (Array)
import Expect exposing (Expectation)
import Fixtures exposing (aShape, aTextArea, autoExpand, drawingInfo, end, model, resizingInfo, start, testColor)
import Annotation as Annotation exposing (Annotation(..), Drawing(..), LineType(..), ShapeType(..), shiftPosition)
import Annotation exposing (Vertex(Start))
import Model exposing (Model)

import Test exposing (..)
import TestUtil exposing (getFirstAnnotation)


update : Msg -> Model -> Model
update msg model =
    Tuple.first (Update.update msg model)


initAnnotation : Model -> Maybe Annotation
initAnnotation model =
    Annotation.fromDrawing False model.drawing (extractAnnotationAttributes model) drawingInfo


modelWithOneAnnotation : Model
modelWithOneAnnotation =
    model
        |> update (StartDrawing start)
        |> update (ContinueDrawing (shiftPosition 10 10 start))
        |> update (FinishDrawing end)


modelWithAlteredSelectedAnnotation : Model
modelWithAlteredSelectedAnnotation =
    modelWithOneAnnotation
        |> update (SelectAnnotation 0)
        |> update (SelectStrokeColor Color.red)


modelWithGoatsText : Model
modelWithGoatsText =
    model
        |> update (ChangeDrawing DrawTextBox)
        |> update (StartDrawing start)
        |> update (FinishDrawing end)
        |> update (TextBoxInput 0 { state = autoExpand, textValue = "Goats!" })


shiftedAnnotation : Maybe Annotation
shiftedAnnotation =
    initAnnotation model
        |> Maybe.map (Annotation.move ( 15, 20 ))


resizedAnnotation : Maybe Annotation
resizedAnnotation =
    initAnnotation model
        |> Maybe.map (Annotation.resize False resizingInfo)


goatsTextArea : Annotation
goatsTextArea =
    Annotation.newTextBox TextBoxInput 0 (extractAnnotationAttributes model) drawingInfo
        |> Annotation.updateTextArea autoExpand "Goats!"


all : Test
all =
    describe "update"
        [ test "can create an annotation" <|
            \() ->
                modelWithOneAnnotation
                    |> getFirstAnnotation
                    |> Expect.equal (initAnnotation modelWithOneAnnotation)
        , test "can change a selected annotation's attributes" <|
            \() ->
                modelWithOneAnnotation
                    |> update (SelectAnnotation 0)
                    |> update (SelectStrokeColor Color.red)
                    |> getFirstAnnotation
                    |> Expect.equal (initAnnotation modelWithAlteredSelectedAnnotation)
        , test "can move a selected annotation " <|
            \() ->
                modelWithOneAnnotation
                    |> update (SelectAnnotation 0)
                    |> update (StartMovingAnnotation 0 start)
                    |> update (FinishMovingAnnotation (shiftPosition 15 20 start))
                    |> getFirstAnnotation
                    |> Expect.equal shiftedAnnotation
        , test "can resize a selected annotation" <|
            \() ->
                modelWithOneAnnotation
                    |> update (SelectAnnotation 0)
                    |> update (StartResizingAnnotation 0 Start start)
                    |> update (FinishResizingAnnotation (shiftPosition -10 -10 start))
                    |> getFirstAnnotation
                    |> Expect.equal resizedAnnotation
        , test "can edit a text box" <|
            \() ->
                modelWithGoatsText
                    |> getFirstAnnotation
                    |> Expect.equal (Just goatsTextArea)
        , test "should cancel drawing if it is too small" <|
            \() ->
                model
                    |> update (ChangeDrawing (DrawLine Arrow))
                    |> update (StartDrawing start)
                    |> update (FinishDrawing (shiftPosition 2 2 start))
                    |> .edits
                    |> .present
                    |> Array.isEmpty
                    |> Expect.true "drawing should not be added to array"
        , test "should cancel drawing for tiny free-draw drawing" <|
            \() ->
                model
                    |> update (ChangeDrawing DrawFreeHand)
                    |> update (StartDrawing start)
                    |> update (FinishDrawing (shiftPosition 2 2 start))
                    |> .edits
                    |> .present
                    |> Array.isEmpty
                    |> Expect.true "drawing should not be added to array"
        , test "should not cancel drawing for big one-step free-draw drawing" <|
            \() ->
                model
                    |> update (ChangeDrawing DrawFreeHand)
                    |> update (StartDrawing start)
                    |> update (FinishDrawing (shiftPosition 10 10 start))
                    |> .edits
                    |> .present
                    |> Array.isEmpty
                    |> Expect.false "annotation should be big enough"
        , test "should not cancel drawing for big free-draw drawing that ends where it started" <|
            \() ->
                model
                    |> update (ChangeDrawing DrawFreeHand)
                    |> update (StartDrawing start)
                    |> update (ContinueDrawing (shiftPosition 10 10 start))
                    |> update (FinishDrawing start)
                    |> .edits
                    |> .present
                    |> Array.isEmpty
                    |> Expect.false "annotation should be big enough"
        , test "should cancel drawing if it is too small, with more tolerance for spotlights" <|
            \() ->
                model
                    |> update (ChangeDrawing (DrawSpotlight Rect))
                    |> update (StartDrawing start)
                    |> update (FinishDrawing (shiftPosition 6 6 start))
                    |> .edits
                    |> .present
                    |> Array.isEmpty
                    |> Expect.true "spotlight drawing should not be added to array"
        , test "editing a textbox's text should not add to the undo history" <|
            \() ->
                model
                    |> update (ChangeDrawing DrawTextBox)
                    |> update (StartDrawing start)
                    |> update (FinishDrawing end)
                    |> update (TextBoxInput 0 { state = aTextArea.autoexpand, textValue = "New Text" })
                    |> .edits
                    |> .past
                    |> List.length
                    |> Expect.equal 1
        , test "should remove textbox if text only has spaces/empty" <|
            \() ->
                model
                    |> update (ChangeDrawing DrawTextBox)
                    |> update (StartDrawing start)
                    |> update (FinishDrawing end)
                    |> update (TextBoxInput 0 { state = aTextArea.autoexpand, textValue = " " })
                    |> update (FinishEditingText 0)
                    |> .edits
                    |> .present
                    |> Array.isEmpty
                    |> Expect.true "the empty textbox should be gone!"
        ]
