module Update exposing (all)

import Color
import Expect exposing (Expectation)
import Fixtures exposing (aShape, drawingInfo, end, model, start, testColor, resizingInfo)
import Goat.Annotation as Annotation exposing (Annotation, shiftPosition)
import Goat.Annotation.Shared exposing (Vertex(Start))
import Goat.Model exposing (Model)
import Goat.Update exposing (Msg(..), Msg(SelectStrokeColor), extractAnnotationAttributes, moveAnnotation, selectAnnotation)
import Test exposing (..)
import TestUtil exposing (getFirstAnnotation)


update : Msg -> Model -> Model
update msg model =
    Tuple.first (Goat.Update.update msg model)


initAnnotation : Model -> Maybe Annotation
initAnnotation model =
    Annotation.fromDrawing False model.drawing (extractAnnotationAttributes model) drawingInfo


modelWithOneAnnotation : Model
modelWithOneAnnotation =
    model
        |> update (StartDrawing start)
        |> update (ContinueDrawing (shiftPosition 10 10 start))
        |> update (FinishDrawing end)
        |> Debug.log "model"


modelWithAlteredSelectedAnnotation : Model
modelWithAlteredSelectedAnnotation =
    modelWithOneAnnotation
        |> update (SelectAnnotation 0)
        |> update (SelectStrokeColor Color.red)


shiftedAnnotation : Maybe Annotation
shiftedAnnotation =
    initAnnotation model
        |> Maybe.map (Annotation.move ( 15, 20 ))


resizedAnnotation : Maybe Annotation
resizedAnnotation =
    initAnnotation model
        |> Maybe.map (Annotation.resize False resizingInfo)


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
        ]
