module Goat.Update exposing (..)

import Array.Hamt as Array exposing (Array)
import AutoExpand
import Color exposing (Color)
import Dom
import Goat.Helpers exposing (..)
import Goat.Model exposing (..)
import Goat.Ports as Ports
import Html.Attributes as Attr
import Http exposing (jsonBody)
import Json.Decode exposing (decodeString, field)
import Json.Encode exposing (object, string, list)
import Keyboard.Extra as Keyboard exposing (Key(..), KeyChange, KeyChange(..), isPressed)
import List.Zipper exposing (Zipper)
import Mouse exposing (Position)
import Rocket exposing ((=>))
import Task exposing (succeed)
import UndoList exposing (UndoList)


update : Msg -> BugReportModel -> ( BugReportModel, List (Cmd Msg) )
update msg bugReportModel =
    updateApp bugReportModel.messages bugReportModel.bugReportState msg bugReportModel.app
        |> Tuple.mapFirst (addMessage msg bugReportModel)


updateApp : List Msg -> Model -> Msg -> Model -> ( Model, List (Cmd Msg) )
updateApp messages bugState msg ({ edits, fill, fontSize, strokeColor, strokeStyle, mouse, images, keyboardState, drawing } as model) =
    let
        annotations =
            edits.present

        ( width, height ) =
            case images of
                Just imageZipper ->
                    (List.Zipper.current imageZipper).width => (List.Zipper.current imageZipper).height

                Nothing ->
                    ( 0, 0 )
    in
        case msg of
            StartDrawing pos ->
                model
                    |> startAnnotation pos images
                    => []

            FinishDrawing start end ->
                if isDrawingTooSmall (isSpotlightDrawing model.drawing) start end then
                    model
                        |> cancelDrawing
                        => []
                else
                    finishDrawing start end model

            StartEditingText index textArea ->
                annotations
                    |> Array.set index (TextBox textArea)
                    |> logChange model
                    |> startEditingText index
                    => [ "text-box-edit--"
                            ++ toString index
                            |> Dom.focus
                            |> Task.attempt (tryToEdit index)
                       ]

            SwitchToEditingText index ->
                model
                    |> startEditingText index
                    => []

            SetText index textBox newText ->
                annotations
                    |> Array.set index (TextBox { textBox | text = newText })
                    |> skipChange model
                    => []

            FinishEditingText index ->
                model
                    |> finishEditingText index
                    => []

            AutoExpandInput index { state, textValue } ->
                annotations
                    |> Array.get index
                    |> Maybe.map (\ann -> Array.set index (autoExpandAnnotation state textValue ann) annotations)
                    |> Maybe.withDefault annotations
                    |> skipChange model
                    => []

            ContinueDrawing pos ->
                model
                    |> setMouse pos
                    => []

            SetImages images ->
                { model | images = List.Zipper.fromList images }
                    => []

            Goat.Model.Cancel ->
                { model | imageSelected = False, edits = UndoList.reset model.edits }
                    => []

            KeyboardMsg keyMsg ->
                let
                    ( keyboardState, maybeKeyChange ) =
                        Keyboard.updateWithKeyChange keyMsg model.keyboardState
                in
                    { model | keyboardState = keyboardState }
                        |> alterDrawingsWithKeyboard maybeKeyChange

            SelectImage image ->
                case model.images of
                    Just images ->
                        { model
                            | images =
                                images
                                    |> List.Zipper.first
                                    |> List.Zipper.find ((==) image.url << .url)
                            , imageSelected = True
                        }
                            => []

                    Nothing ->
                        model => []

            ChangeDrawing drawing ->
                { model | drawing = drawing }
                    |> closeDropdown
                    => []

            SelectFill fill ->
                model
                    |> updateAnySelectedAnnotations (updateFill fill)
                    |> setFill fill
                    |> closeDropdown
                    => []

            SelectStrokeColor strokeColor ->
                model
                    |> updateAnySelectedAnnotations (updateStrokeColor strokeColor)
                    |> setStrokeColor strokeColor
                    |> closeDropdown
                    => []

            SelectStrokeStyle strokeStyle ->
                model
                    |> updateAnySelectedAnnotations (updateStrokeStyle strokeStyle)
                    |> setStrokeStyle strokeStyle
                    |> closeDropdown
                    => []

            SelectFontSize fontSize ->
                model
                    |> updateAnySelectedAnnotations (updateFontSize fontSize)
                    |> setFontSize fontSize
                    |> closeDropdown
                    => []

            ToggleDropdown editOption ->
                model
                    |> toggleDropdown editOption
                    => []

            CloseDropdown ->
                model
                    |> closeDropdown
                    => []

            SelectAnnotation index annotation start ->
                model
                    |> selectAnnotation index annotation
                    |> startMovingAnnotation index annotation start
                    => []

            ResetToReadyToDraw ->
                model
                    |> resetToReadyToDraw
                    => []

            StartMovingAnnotation index annotation start ->
                model
                    |> setMouse start
                    |> selectAnnotation index annotation
                    |> startMovingAnnotation index annotation start
                    => []

            MoveAnnotation index annotation oldPos newPos ->
                model
                    |> moveAnnotation index annotation oldPos newPos
                    => []

            FinishMovingAnnotation index annotation start end ->
                model
                    |> moveAnnotation index annotation start end
                    |> finishMovingAnnotation index (move start end annotation)
                    => []

            StartResizingAnnotation index annotation vertex start ->
                model
                    |> setMouse start
                    |> selectAnnotation index annotation
                    |> startResizingAnnotation index annotation vertex start
                    |> resizeAnnotation index annotation vertex start start
                    => []

            ResizeAnnotation index annotation vertex start end ->
                model
                    |> resizeAnnotation index annotation vertex start end
                    => []

            FinishResizingAnnotation index annotation vertex start end ->
                model
                    |> resizeAnnotation index annotation vertex start end
                    |> selectAnnotation index (resize start end vertex annotation)
                    => []

            Undo ->
                { model | edits = UndoList.undo model.edits }
                    => []

            Redo ->
                { model | edits = UndoList.redo model.edits }
                    => []

            Save ->
                model
                    |> resetToReadyToDraw
                    => [ case model.images of
                            Just images ->
                                Ports.exportToImage <| List.Zipper.current images

                            Nothing ->
                                Cmd.none
                       ]

            BugReports bugReportMsg ->
                case bugReportMsg of
                    ShowBugReportInput ->
                        model
                            |> showBugReportInput
                            => [ Dom.focus "bug-report-text"
                                    |> Task.attempt tryToFocusBugReport
                               ]

                    BugReportInput text ->
                        model
                            |> setBugReportInput text
                            => []

                    SubmitBugReport ->
                        model
                            |> dismissBugReportDialog
                            => [ logBugReport (Maybe.withDefault "" model.bugReportText) messages bugState ]

                    CancelBugReport ->
                        model
                            |> dismissBugReportDialog
                            => []

                    SubmittedBugReport _ ->
                        model
                            => []

            ShowMeTheGoats ->
                { model
                    | images = List.Zipper.fromList theGoats
                }
                    => []


{-| Add this editState change to app history
-}
logChange : Model -> Array Annotation -> Model
logChange model editState =
    { model | edits = UndoList.new editState model.edits }


{-| Do not add this editState change to app history
-}
skipChange : Model -> Array Annotation -> Model
skipChange model editState =
    { model | edits = UndoList.mapPresent (always editState) model.edits }


startAnnotation : Position -> Maybe (Zipper Image) -> Model -> Model
startAnnotation start images model =
    model
        |> startDrawing start
        |> updateMouse images start


resetSelection : Model -> Model
resetSelection model =
    { model | annotationState = ReadyToDraw }


startDrawing : StartPosition -> Model -> Model
startDrawing start model =
    { model | annotationState = DrawingAnnotation start }


resetToReadyToDraw : Model -> Model
resetToReadyToDraw model =
    { model | annotationState = ReadyToDraw }


finishMovingAnnotation : Int -> Annotation -> Model -> Model
finishMovingAnnotation index annotation model =
    { model | annotationState = SelectedAnnotation index annotation }


updateAnySelectedAnnotations : (Annotation -> Annotation) -> Model -> Model
updateAnySelectedAnnotations fn model =
    case model.annotationState of
        SelectedAnnotation index annotation ->
            { model
                | edits = UndoList.new (Array.set index (fn annotation) model.edits.present) model.edits
                , annotationState = SelectedAnnotation index (fn annotation)
            }

        _ ->
            model


autoExpandAnnotation : AutoExpand.State -> String -> Annotation -> Annotation
autoExpandAnnotation state textValue annotation =
    case annotation of
        TextBox textBox ->
            TextBox { textBox | autoexpand = state, text = textValue }

        _ ->
            annotation


selectAnnotation : Int -> Annotation -> Model -> Model
selectAnnotation index annotation model =
    { model | annotationState = SelectedAnnotation index annotation }


addAnnotation : Annotation -> Model -> Model
addAnnotation annotation model =
    { model
        | edits = UndoList.new (Array.push annotation model.edits.present) model.edits
        , annotationState = ReadyToDraw
    }


finishLineDrawing : StartPosition -> EndPosition -> LineType -> LineMode -> Model -> Model
finishLineDrawing start end lineType lineMode model =
    model
        |> addAnnotation (Lines lineType (Line start (calcLinePos start end lineMode) model.strokeColor model.strokeStyle))


finishShapeDrawing : StartPosition -> EndPosition -> ShapeType -> ShapeMode -> Model -> Model
finishShapeDrawing start end shapeType shapeMode model =
    model
        |> addAnnotation (Shapes shapeType (Shape start (calcShapePos start end shapeMode) model.fill model.strokeColor model.strokeStyle))


finishSpotlightDrawing : StartPosition -> EndPosition -> ShapeType -> ShapeMode -> Model -> Model
finishSpotlightDrawing start end shapeType shapeMode model =
    model
        |> addAnnotation (Spotlight shapeType (Shape start (calcShapePos start end shapeMode) SpotlightFill model.strokeColor model.strokeStyle))


finishDrawing : StartPosition -> EndPosition -> Model -> ( Model, List (Cmd Msg) )
finishDrawing start end ({ fill, strokeColor, strokeStyle, fontSize } as model) =
    let
        numAnnotations =
            Array.length model.edits.present

        initialTextBox =
            TextBox <| TextArea start end strokeColor fontSize "Text" 0 (AutoExpand.initState (config numAnnotations fontSize))
    in
        case model.drawing of
            DrawLine lineType lineMode ->
                finishLineDrawing start end lineType lineMode model
                    => []

            DrawShape shapeType shapeMode ->
                finishShapeDrawing start end shapeType shapeMode model
                    => []

            DrawTextBox ->
                model
                    |> addAnnotation (TextBox <| TextArea start (calcShapePos start end DrawingShape) strokeColor fontSize "Text" 0 (AutoExpand.initState (config numAnnotations fontSize)))
                    |> startEditingText numAnnotations
                    => [ "text-box-edit--"
                            ++ toString numAnnotations
                            |> Dom.focus
                            |> Task.attempt (tryToEdit numAnnotations)
                       ]

            DrawSpotlight shapeType shapeMode ->
                finishSpotlightDrawing start end shapeType shapeMode model
                    => []


startEditingText : Int -> Model -> Model
startEditingText index model =
    { model | annotationState = EditingATextBox index }


updateStrokeColor : Color -> Annotation -> Annotation
updateStrokeColor strokeColor annotation =
    case annotation of
        Lines lineType line ->
            Lines lineType { line | strokeColor = strokeColor }

        Shapes shapeType shape ->
            Shapes shapeType { shape | strokeColor = strokeColor }

        TextBox textBox ->
            TextBox { textBox | fill = strokeColor }

        Spotlight shapeType shape ->
            Spotlight shapeType { shape | strokeColor = strokeColor }


updateFill : Fill -> Annotation -> Annotation
updateFill fill annotation =
    case annotation of
        Lines _ _ ->
            annotation

        Shapes shapeType shape ->
            Shapes shapeType { shape | fill = fill }

        TextBox textBox ->
            annotation

        Spotlight shapeType shape ->
            annotation


updateStrokeStyle : StrokeStyle -> Annotation -> Annotation
updateStrokeStyle strokeStyle annotation =
    case annotation of
        Lines lineType line ->
            Lines lineType { line | strokeStyle = strokeStyle }

        Shapes shapeType shape ->
            Shapes shapeType { shape | strokeStyle = strokeStyle }

        TextBox textBox ->
            annotation

        Spotlight shapeType shape ->
            Spotlight shapeType { shape | strokeStyle = strokeStyle }


updateFontSize : Float -> Annotation -> Annotation
updateFontSize fontSize annotation =
    case annotation of
        TextBox textBox ->
            TextBox { textBox | fontSize = fontSize }

        _ ->
            annotation


setFill : Fill -> Model -> Model
setFill fill model =
    { model | fill = fill }


setFontSize : Float -> Model -> Model
setFontSize fontSize model =
    { model | fontSize = fontSize }


setStrokeStyle : StrokeStyle -> Model -> Model
setStrokeStyle strokeStyle model =
    { model | strokeStyle = strokeStyle }


setStrokeColor : Color -> Model -> Model
setStrokeColor strokeColor model =
    { model | strokeColor = strokeColor }


startMovingAnnotation : Int -> Annotation -> StartPosition -> Model -> Model
startMovingAnnotation index annotation start model =
    { model | annotationState = MovingAnnotation index annotation start }


moveAnnotation : Int -> Annotation -> StartPosition -> EndPosition -> Model -> Model
moveAnnotation index annotation oldPos newPos model =
    { model
        | edits = UndoList.mapPresent (Array.set index (move oldPos newPos annotation)) model.edits
    }


startResizingAnnotation : Int -> Annotation -> Vertex -> StartPosition -> Model -> Model
startResizingAnnotation index annotation vertex start model =
    { model | annotationState = ResizingAnnotation index annotation start vertex }


resizeAnnotation : Int -> Annotation -> Vertex -> StartPosition -> EndPosition -> Model -> Model
resizeAnnotation index annotation vertex oldPos newPos model =
    { model
        | edits = UndoList.mapPresent (Array.set index (resize oldPos newPos vertex annotation)) model.edits
    }


resizeVertices : Position -> Vertex -> { a | start : Position, end : Position } -> { a | start : Position, end : Position }
resizeVertices pos vertex annotation =
    case vertex of
        Start ->
            { annotation | start = pos }

        Goat.Model.End ->
            { annotation | end = pos }

        StartPlusX ->
            { annotation | start = pos, end = Position annotation.start.x annotation.end.y }

        StartPlusY ->
            { annotation | start = pos, end = Position annotation.end.x annotation.start.y }


resize : StartPosition -> EndPosition -> Vertex -> Annotation -> Annotation
resize start end vertex annotation =
    case annotation of
        Lines lineType line ->
            Lines lineType (resizeVertices end vertex line)

        Shapes shapeType shape ->
            Shapes shapeType (resizeVertices end vertex shape)

        TextBox textArea ->
            TextBox (resizeVertices end vertex textArea)

        Spotlight shapeType shape ->
            Spotlight shapeType (resizeVertices end vertex shape)


move : StartPosition -> EndPosition -> Annotation -> Annotation
move oldPos newPos annotation =
    let
        dX =
            newPos.x - oldPos.x

        dY =
            newPos.y - oldPos.y

        shift drawing =
            { drawing | start = shiftPosition dX dY drawing.start, end = shiftPosition dX dY drawing.end }
    in
        case annotation of
            Lines lineType line ->
                Lines lineType (shift line)

            Shapes shapeType shape ->
                Shapes shapeType (shift shape)

            TextBox textArea ->
                TextBox (shift textArea)

            Spotlight shapeType shape ->
                Spotlight shapeType (shift shape)


shiftPosition : Int -> Int -> Mouse.Position -> Mouse.Position
shiftPosition dx dy pos =
    { pos | x = pos.x + dx, y = pos.y + dy }


closeDropdown : Model -> Model
closeDropdown model =
    { model | currentDropdown = Nothing }


toggleDropdown : AttributeDropdown -> Model -> Model
toggleDropdown editOption model =
    { model
        | currentDropdown =
            case model.currentDropdown of
                Just dropdown ->
                    if dropdown == editOption then
                        Nothing
                    else
                        Just editOption

                Nothing ->
                    Just editOption
        , drawing =
            case editOption of
                Fonts ->
                    DrawTextBox

                _ ->
                    model.drawing
    }


selectShape : ShapeType -> Keyboard.State -> Drawing
selectShape shapeType keyboardState =
    DrawShape shapeType <|
        if isPressed Shift keyboardState then
            DrawingEqualizedShape
        else
            DrawingShape


updateMouse : Maybe (Zipper Image) -> Position -> Model -> Model
updateMouse maybeImages mouse model =
    case maybeImages of
        Nothing ->
            model

        Just images ->
            let
                { width, height } =
                    List.Zipper.current images
            in
                { model | mouse = mouse }


setMouse : Mouse.Position -> Model -> Model
setMouse mouse model =
    { model | mouse = mouse }


transitionOnShift : Drawing -> Drawing
transitionOnShift drawing =
    case drawing of
        DrawLine lineType lineMode ->
            case lineMode of
                DrawingLine ->
                    DrawLine lineType DrawingDiscreteLine

                DrawingDiscreteLine ->
                    DrawLine lineType DrawingLine

        DrawShape shapeType shapeMode ->
            case shapeMode of
                DrawingShape ->
                    DrawShape shapeType DrawingEqualizedShape

                DrawingEqualizedShape ->
                    DrawShape shapeType DrawingShape

        DrawTextBox ->
            drawing

        DrawSpotlight shapeType shapeMode ->
            case shapeMode of
                DrawingShape ->
                    DrawSpotlight shapeType DrawingEqualizedShape

                DrawingEqualizedShape ->
                    DrawSpotlight shapeType DrawingShape


cancelDrawing : Model -> Model
cancelDrawing model =
    { model | annotationState = ReadyToDraw }


finishEditingText : Int -> Model -> Model
finishEditingText index model =
    { model | annotationState = ReadyToDraw }


alterDrawingsWithKeyboard : Maybe KeyChange -> Model -> ( Model, List (Cmd Msg) )
alterDrawingsWithKeyboard maybeKeyChange ({ keyboardState } as model) =
    case model.annotationState of
        EditingATextBox index ->
            alterTextBoxDrawing maybeKeyChange index model

        _ ->
            alterDrawing maybeKeyChange model
                => []


alterTextBoxDrawing : Maybe KeyChange -> Int -> Model -> ( Model, List (Cmd Msg) )
alterTextBoxDrawing maybeKeyChange index model =
    case maybeKeyChange of
        Just keyChange ->
            case keyChange of
                KeyDown key ->
                    case key of
                        Escape ->
                            finishEditingText index model
                                => [ "text-box-edit--"
                                        ++ toString index
                                        |> Dom.blur
                                        |> Task.attempt tryToBlur
                                   ]

                        _ ->
                            model => []

                KeyUp key ->
                    model => []

        Nothing ->
            model => []


deleteSelectedDrawing : Model -> Model
deleteSelectedDrawing model =
    case model.annotationState of
        SelectedAnnotation index _ ->
            { model
                | edits = UndoList.new (removeItem index model.edits.present) model.edits
                , annotationState = ReadyToDraw
            }

        _ ->
            model


removeItem : Int -> Array a -> Array a
removeItem index arr =
    Array.append (Array.slice 0 index arr) (Array.slice (index + 1) (Array.length arr) arr)


changeDrawing : Drawing -> Model -> Model
changeDrawing drawing model =
    { model | drawing = drawing }


alterDrawing : Maybe KeyChange -> Model -> Model
alterDrawing maybeKeyChange ({ keyboardState } as model) =
    let
        controlKey =
            case model.operatingSystem of
                MacOS ->
                    Super

                Windows ->
                    Control
    in
        case maybeKeyChange of
            Just keyChange ->
                case keyChange of
                    KeyDown key ->
                        case key of
                            Shift ->
                                changeDrawing (transitionOnShift model.drawing) model

                            Escape ->
                                cancelDrawing model

                            Delete ->
                                deleteSelectedDrawing model

                            BackSpace ->
                                deleteSelectedDrawing model

                            CharZ ->
                                if isPressed Shift keyboardState && isPressed controlKey keyboardState then
                                    { model | edits = UndoList.redo model.edits, keyboardState = Keyboard.forceRelease [ CharZ ] model.keyboardState }
                                else if isPressed controlKey keyboardState then
                                    { model | edits = UndoList.undo model.edits, keyboardState = Keyboard.forceRelease [ CharZ ] model.keyboardState }
                                else
                                    model

                            Control ->
                                if model.operatingSystem == MacOS then
                                    model
                                else if isPressed Shift keyboardState && isPressed CharZ keyboardState then
                                    { model | edits = UndoList.redo model.edits, keyboardState = Keyboard.forceRelease [ CharZ ] model.keyboardState }
                                else if isPressed CharZ keyboardState then
                                    { model | edits = UndoList.undo model.edits, keyboardState = Keyboard.forceRelease [ CharZ ] model.keyboardState }
                                else
                                    model

                            Super ->
                                if model.operatingSystem == Windows then
                                    model
                                else if isPressed Shift keyboardState && isPressed CharZ keyboardState then
                                    { model | edits = UndoList.redo model.edits, keyboardState = Keyboard.forceRelease [ CharZ ] model.keyboardState }
                                else if isPressed CharZ keyboardState then
                                    { model | edits = UndoList.undo model.edits, keyboardState = Keyboard.forceRelease [ CharZ ] model.keyboardState }
                                else
                                    model

                            _ ->
                                model

                    KeyUp key ->
                        case key of
                            Shift ->
                                changeDrawing (transitionOnShift model.drawing) model

                            _ ->
                                model

            Nothing ->
                model


tryToFocusBugReport : Result Dom.Error () -> Msg
tryToFocusBugReport result =
    BugReports <| BugReportInput ""


tryToEdit : Int -> Result Dom.Error () -> Msg
tryToEdit index result =
    case result of
        Ok _ ->
            SwitchToEditingText index

        Err _ ->
            Undo


tryToBlur : Result Dom.Error () -> Msg
tryToBlur result =
    case result of
        Ok _ ->
            ResetToReadyToDraw

        Err _ ->
            Undo


showBugReportInput : Model -> Model
showBugReportInput model =
    { model | bugReportText = Just "" }


setBugReportInput : String -> Model -> Model
setBugReportInput text model =
    { model | bugReportText = Just text }


dismissBugReportDialog : Model -> Model
dismissBugReportDialog model =
    { model | bugReportText = Nothing }


logBugReport : String -> List Msg -> Model -> Cmd Msg
logBugReport reason messages state =
    Http.post "https://goat-b4988.firebaseio.com/.json" (jsonBody (object [ "reason" => string reason, "history" => list (List.map (string << toString) messages), "state" => elmToJs state ])) (field "name" Json.Decode.string)
        |> Http.send (BugReports << SubmittedBugReport)


addMessage : Msg -> BugReportModel -> Model -> BugReportModel
addMessage msg bugReportModel model =
    case msg of
        BugReports _ ->
            { bugReportModel | app = model }

        _ ->
            if List.length bugReportModel.messages < 50 then
                { bugReportModel | messages = msg :: bugReportModel.messages, app = model }
            else
                { bugReportModel
                    | messages = [ msg ]
                    , bugReportState = model
                    , app = model
                }


config : Int -> Float -> AutoExpand.Config Msg
config index fontSize =
    AutoExpand.config
        { onInput = AutoExpandInput index
        , padding = 2
        , lineHeight = fontSize * 1.2
        , minRows = 1
        , maxRows = 4
        , attributes =
            [ Attr.id <| "text-box-edit--" ++ toString index
            , Attr.class "text-box-textarea"
            , Attr.style [ "font-size" => toPx fontSize ]
            , Attr.attribute "onfocus" "this.select();"
            ]
        }
