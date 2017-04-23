module Goat.Update exposing (..)

import Array.Hamt as Array exposing (Array)
import AutoExpand
import Color exposing (Color)
import Dom
import Goat.Flags exposing (Image)
import Goat.Utils exposing (calcLinePos, calcShapePos, currentAnnotationAttributes, getAnnotationAttributes, getPositions, isDrawingTooSmall, isEmptyTextBox, isSpotlightDrawing, mapAtIndex, positionMap, positionMapX, removeItem, removeItemIf, shiftPosition)
import Goat.Model exposing (..)
import Goat.Ports as Ports
import Html.Attributes as Attr
import Keyboard.Extra as Keyboard exposing (Key(..), KeyChange, KeyChange(KeyDown, KeyUp), isPressed)
import List.Zipper exposing (Zipper)
import Mouse exposing (Position)
import Rocket exposing ((=>))
import Task exposing (succeed)
import UndoList exposing (UndoList)


type Msg
    = StartDrawing Position
    | ContinueDrawing Position
    | FinishDrawing Position
      -- TextArea Updates
    | FocusTextArea Int
    | StartEditingText Int
    | PreventTextMouseDown
    | TextBoxInput Int { textValue : String, state : AutoExpand.State }
    | FinishEditingText Int
      -- Annotation Attribute updates
    | SelectFill (Maybe Color)
    | SelectStrokeColor Color
    | SelectStrokeStyle StrokeStyle
    | SelectFontSize Int
      -- Control UI updates
    | ToggleDropdown AttributeDropdown
    | ChangeDrawing Drawing
    | CloseDropdown
      -- Selection Updates
    | ResetToReadyToDraw
    | SelectAnnotation Int
    | SelectAndMoveAnnotation Int StartPosition
      -- Move updates
    | StartMovingAnnotation Int StartPosition
    | MoveAnnotation Position
    | FinishMovingAnnotation EndPosition
      -- Resize updates
    | StartResizingAnnotation Int Vertex StartPosition
    | ResizeAnnotation Position
    | FinishResizingAnnotation Position
      -- Annotation menu updates
    | ToggleAnnotationMenu Position
    | ToggleSelectedAnnotationMenu Int Position
    | BringAnnotationToFront Int
    | SendAnnotationToBack Int
      -- History updates
    | Undo
    | Redo
    | Save
      -- Image Selection updates
    | Reset
    | SelectImage Image
    | SetImages (List Image)
    | ReturnToImageSelection
      -- Keyboard updates
    | KeyboardMsg Keyboard.Msg
      -- Modal updates
    | CloseAllMenus
      -- !!! GOATS !!!
    | ShowMeTheGoats


update : Msg -> Model -> ( Model, List (Cmd Msg) )
update msg ({ fill, fontSize, strokeColor, strokeStyle, images, keyboardState, drawing } as model) =
    case msg of
        StartDrawing pos ->
            model
                |> startDrawing pos
                => []

        ContinueDrawing pos ->
            model
                |> continueDrawing pos
                => []

        FinishDrawing pos ->
            finishDrawing pos model

        FocusTextArea index ->
            model
                |> startEditingText index
                => [ "text-box-edit--"
                        ++ toString index
                        |> Dom.focus
                        |> Task.attempt (tryToEdit index)
                   ]

        StartEditingText index ->
            model
                |> startEditingText index
                => []

        PreventTextMouseDown ->
            model
                => []

        TextBoxInput index { state, textValue } ->
            model
                |> editTextBoxAnnotation index state textValue
                => []

        FinishEditingText index ->
            model
                |> finishEditingText index
                => []

        SetImages images ->
            { model | images = List.Zipper.fromList images, imageSelected = False }
                => []

        Reset ->
            { model
                | images = List.Zipper.fromList []
                , imageSelected = False
            }
                => []

        ReturnToImageSelection ->
            model
                |> returnToImageSelection
                => []

        KeyboardMsg keyMsg ->
            let
                ( keyboardState, maybeKeyChange ) =
                    Keyboard.updateWithKeyChange keyMsg model.keyboardState
            in
                { model | keyboardState = keyboardState }
                    |> alterDrawingsWithKeyboard maybeKeyChange

        CloseAllMenus ->
            model
                |> closeAllMenus
                => []

        SelectImage image ->
            model
                |> selectImage image
                => []

        ChangeDrawing drawing ->
            model
                |> changeDrawing drawing
                |> closeDropdown
                |> resetToReadyToDraw
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

        SelectAnnotation index ->
            model
                |> selectAnnotation index
                => []

        SelectAndMoveAnnotation index start ->
            model
                |> selectAnnotation index
                |> startMovingAnnotation index start
                => []

        ResetToReadyToDraw ->
            model
                |> resetToReadyToDraw
                => []

        StartMovingAnnotation index start ->
            model
                |> selectAnnotation index
                |> startMovingAnnotation index start
                => []

        MoveAnnotation newPos ->
            model
                |> moveAnnotation newPos
                => []

        FinishMovingAnnotation endPos ->
            model
                |> moveAnnotation endPos
                |> finishMovingAnnotation
                => []

        StartResizingAnnotation index vertex start ->
            model
                |> startResizingAnnotation index vertex start
                |> resizeAnnotation start
                => []

        ResizeAnnotation pos ->
            model
                |> resizeAnnotation pos
                => []

        FinishResizingAnnotation pos ->
            model
                |> resizeAnnotation pos
                |> finishResizingAnnotation
                => []

        BringAnnotationToFront index ->
            model
                |> bringAnnotationToFront index
                => []

        SendAnnotationToBack index ->
            model
                |> sendAnnotationToBack index
                => []

        ToggleAnnotationMenu pos ->
            model
                |> toggleAnnotationMenu Nothing pos
                => []

        ToggleSelectedAnnotationMenu index pos ->
            model
                |> toggleAnnotationMenu (Just index) pos
                |> selectAnnotation index
                => []

        Undo ->
            undoEdit model
                => []

        Redo ->
            redoEdit model
                => []

        Save ->
            model
                |> resetToReadyToDraw
                => [ case model.images of
                        Just images ->
                            Ports.exportToImage (List.Zipper.current images).id

                        Nothing ->
                            Cmd.none
                   ]

        ShowMeTheGoats ->
            model
                => [ Ports.requestImages () ]


{-| Do not add this annotations array change change to undo history
-}
skipChange : Model -> Array Annotation -> Model
skipChange model annotations =
    { model | edits = UndoList.mapPresent (\_ -> annotations) model.edits }



-- DRAWING


startDrawing : StartPosition -> Model -> Model
startDrawing start model =
    { model
        | annotationState = DrawingAnnotation start start
    }


continueDrawing : Position -> Model -> Model
continueDrawing pos model =
    case model.annotationState of
        DrawingAnnotation start _ ->
            { model | annotationState = DrawingAnnotation start pos }

        _ ->
            model


finishDrawing : Position -> Model -> ( Model, List (Cmd Msg) )
finishDrawing pos ({ fill, strokeColor, strokeStyle, fontSize } as model) =
    case model.annotationState of
        DrawingAnnotation start _ ->
            if isDrawingTooSmall (isSpotlightDrawing model.drawing) start pos then
                model
                    |> cancelDrawing
                    => []
            else
                case model.drawing of
                    DrawLine lineType ->
                        finishLineDrawing start pos lineType model
                            => []

                    DrawShape shapeType ->
                        finishShapeDrawing start pos shapeType model
                            => []

                    DrawTextBox ->
                        let
                            numAnnotations =
                                Array.length model.edits.present
                        in
                            finishTextBoxDrawing start pos model
                                => [ "text-box-edit--"
                                        ++ toString numAnnotations
                                        |> Dom.focus
                                        |> Task.attempt (tryToEdit numAnnotations)
                                   ]

                    DrawSpotlight shapeType ->
                        finishSpotlightDrawing start pos shapeType model
                            => []

                    DrawPixelate ->
                        finishPixelateDrawing start pos model
                            => []

        _ ->
            model => []


resetToReadyToDraw : Model -> Model
resetToReadyToDraw model =
    { model | annotationState = ReadyToDraw }


finishMovingAnnotation : Model -> Model
finishMovingAnnotation model =
    case model.annotationState of
        MovingAnnotation index _ translate _ ->
            { model | edits = UndoList.new (mapAtIndex index (move translate) model.edits.present) model.edits }
                |> selectAnnotation index

        _ ->
            model


updateAnySelectedAnnotations : (Annotation -> Annotation) -> Model -> Model
updateAnySelectedAnnotations fn model =
    case model.annotationState of
        SelectedAnnotation index _ ->
            { model
                | edits = UndoList.new (mapAtIndex index fn model.edits.present) model.edits
            }

        EditingATextBox index _ ->
            { model
                | edits = UndoList.mapPresent (mapAtIndex index fn) model.edits
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


selectAnnotation : Int -> Model -> Model
selectAnnotation index model =
    case Array.get index model.edits.present of
        Just annotation ->
            { model | annotationState = SelectedAnnotation index (getAnnotationAttributes annotation (currentAnnotationAttributes model)) }

        Nothing ->
            model


addAnnotation : Annotation -> Model -> Model
addAnnotation annotation model =
    { model
        | edits = UndoList.new (Array.push annotation model.edits.present) model.edits
        , annotationState = ReadyToDraw
    }


finishLineDrawing : StartPosition -> EndPosition -> LineType -> Model -> Model
finishLineDrawing start end lineType model =
    model
        |> addAnnotation (Lines lineType (Shape start (calcLinePos (isPressed Shift model.keyboardState) start end) model.strokeColor model.strokeStyle))


finishPixelateDrawing : StartPosition -> EndPosition -> Model -> Model
finishPixelateDrawing start end model =
    model
        |> addAnnotation (Pixelate start end)


finishShapeDrawing : StartPosition -> EndPosition -> ShapeType -> Model -> Model
finishShapeDrawing start end shapeType model =
    model
        |> addAnnotation (Shapes shapeType model.fill (Shape start (calcShapePos (isPressed Shift model.keyboardState) start end) model.strokeColor model.strokeStyle))


finishTextBoxDrawing : StartPosition -> EndPosition -> Model -> Model
finishTextBoxDrawing start end model =
    let
        numAnnotations =
            Array.length model.edits.present
    in
        model
            |> addAnnotation (TextBox (TextArea start end model.strokeColor model.fontSize "Text" 0 (AutoExpand.initState (autoExpandConfig numAnnotations))))
            |> startEditingText numAnnotations


finishSpotlightDrawing : StartPosition -> EndPosition -> ShapeType -> Model -> Model
finishSpotlightDrawing start end shapeType model =
    model
        |> addAnnotation (Spotlight shapeType (Shape start (calcShapePos (isPressed Shift model.keyboardState) start end) model.strokeColor model.strokeStyle))


startEditingText : Int -> Model -> Model
startEditingText index model =
    case Array.get index model.edits.present of
        Just annotation ->
            { model | annotationState = EditingATextBox index (getAnnotationAttributes annotation (currentAnnotationAttributes model)) }

        Nothing ->
            model


editTextBoxAnnotation : Int -> AutoExpand.State -> String -> Model -> Model
editTextBoxAnnotation index autoExpandState autoExpandText model =
    model.edits.present
        |> mapAtIndex index (autoExpandAnnotation autoExpandState autoExpandText)
        |> skipChange model


updateStrokeColor : Color -> Annotation -> Annotation
updateStrokeColor strokeColor annotation =
    case annotation of
        Lines lineType line ->
            Lines lineType { line | strokeColor = strokeColor }

        Shapes shapeType fill shape ->
            Shapes shapeType fill { shape | strokeColor = strokeColor }

        TextBox textBox ->
            TextBox { textBox | fill = strokeColor }

        Spotlight shapeType shape ->
            Spotlight shapeType { shape | strokeColor = strokeColor }

        Pixelate _ _ ->
            annotation


updateFill : Maybe Color -> Annotation -> Annotation
updateFill fill annotation =
    case annotation of
        Lines _ _ ->
            annotation

        Shapes shapeType _ shape ->
            Shapes shapeType fill shape

        TextBox textBox ->
            annotation

        Spotlight shapeType shape ->
            annotation

        Pixelate _ _ ->
            annotation


updateStrokeStyle : StrokeStyle -> Annotation -> Annotation
updateStrokeStyle strokeStyle annotation =
    case annotation of
        Lines lineType line ->
            Lines lineType { line | strokeStyle = strokeStyle }

        Shapes shapeType fill shape ->
            Shapes shapeType fill { shape | strokeStyle = strokeStyle }

        TextBox textBox ->
            annotation

        Spotlight shapeType shape ->
            Spotlight shapeType { shape | strokeStyle = strokeStyle }

        Pixelate _ _ ->
            annotation


updateFontSize : Int -> Annotation -> Annotation
updateFontSize fontSize annotation =
    case annotation of
        TextBox textBox ->
            TextBox { textBox | fontSize = fontSize }

        _ ->
            annotation


setFill : Maybe Color -> Model -> Model
setFill fill model =
    case model.annotationState of
        SelectedAnnotation index annotationAttrs ->
            { model | annotationState = SelectedAnnotation index { annotationAttrs | fill = fill }, fill = fill }

        _ ->
            { model | fill = fill }


setFontSize : Int -> Model -> Model
setFontSize fontSize model =
    case model.annotationState of
        SelectedAnnotation index annotationAttrs ->
            { model | annotationState = SelectedAnnotation index { annotationAttrs | fontSize = fontSize }, fontSize = fontSize }

        EditingATextBox index annotationAttrs ->
            { model | annotationState = EditingATextBox index { annotationAttrs | fontSize = fontSize }, fontSize = fontSize }

        _ ->
            { model | fontSize = fontSize }


setStrokeStyle : StrokeStyle -> Model -> Model
setStrokeStyle strokeStyle model =
    case model.annotationState of
        SelectedAnnotation index annotationAttrs ->
            { model | annotationState = SelectedAnnotation index { annotationAttrs | strokeStyle = strokeStyle }, strokeStyle = strokeStyle }

        _ ->
            { model | strokeStyle = strokeStyle }


setStrokeColor : Color -> Model -> Model
setStrokeColor strokeColor model =
    case model.annotationState of
        SelectedAnnotation index annotationAttrs ->
            { model | annotationState = SelectedAnnotation index { annotationAttrs | strokeColor = strokeColor }, strokeColor = strokeColor }

        EditingATextBox index annotationAttrs ->
            { model | annotationState = EditingATextBox index { annotationAttrs | strokeColor = strokeColor }, strokeColor = strokeColor }

        _ ->
            { model | strokeColor = strokeColor }


startMovingAnnotation : Int -> Position -> Model -> Model
startMovingAnnotation index newPos model =
    case Array.get index model.edits.present of
        Just annotation ->
            { model
                | annotationState = MovingAnnotation index newPos ( 0, 0 ) (getAnnotationAttributes annotation (currentAnnotationAttributes model))
            }

        Nothing ->
            model


moveAnnotation : Position -> Model -> Model
moveAnnotation newPos model =
    case model.annotationState of
        MovingAnnotation index start ( dx, dy ) annotationAttrs ->
            { model | annotationState = MovingAnnotation index start ( newPos.x - start.x, newPos.y - start.y ) annotationAttrs }

        _ ->
            model


startResizingAnnotation : Int -> Vertex -> StartPosition -> Model -> Model
startResizingAnnotation index vertex start model =
    case Array.get index model.edits.present of
        Just annotation ->
            { model | annotationState = ResizingAnnotation (ResizingData index start start vertex (getPositions annotation)) (getAnnotationAttributes annotation (currentAnnotationAttributes model)) }

        Nothing ->
            model


resizeAnnotation : Position -> Model -> Model
resizeAnnotation curPos model =
    case model.annotationState of
        ResizingAnnotation resizingData annotationAttrs ->
            { model
                | edits = UndoList.mapPresent (mapAtIndex resizingData.index (resize (isPressed Shift model.keyboardState) { resizingData | curPos = curPos })) model.edits
                , annotationState = ResizingAnnotation { resizingData | curPos = curPos } annotationAttrs
            }

        _ ->
            model


finishResizingAnnotation : Model -> Model
finishResizingAnnotation model =
    case model.annotationState of
        ResizingAnnotation { index } _ ->
            selectAnnotation index model

        _ ->
            model


resizeVertices : (StartPosition -> EndPosition -> Position) -> ResizingData -> { a | start : Position, end : Position } -> { a | start : Position, end : Position }
resizeVertices discretize { curPos, vertex, originalCoords } annotation =
    let
        ( start, end ) =
            originalCoords
    in
        case vertex of
            Start ->
                { annotation | start = discretize annotation.end curPos }

            Goat.Model.End ->
                { annotation | end = discretize annotation.start curPos }

            StartPlusX ->
                { annotation | start = discretize annotation.end curPos, end = Position start.x end.y }

            StartPlusY ->
                { annotation | start = discretize annotation.end curPos, end = Position end.x start.y }


resize : Bool -> ResizingData -> Annotation -> Annotation
resize discretize resizingData annotation =
    case annotation of
        Lines lineType shape ->
            Lines lineType (resizeVertices (calcLinePos discretize) resizingData shape)

        Shapes shapeType fill shape ->
            Shapes shapeType fill (resizeVertices (calcShapePos discretize) resizingData shape)

        TextBox textArea ->
            TextBox (resizeVertices (calcShapePos False) resizingData textArea)

        Spotlight shapeType shape ->
            Spotlight shapeType (resizeVertices (calcShapePos discretize) resizingData shape)

        Pixelate start end ->
            Pixelate (resizeVertices (calcShapePos discretize) resizingData { start = start, end = end }).start (resizeVertices (calcShapePos discretize) resizingData { start = start, end = end }).end


shift :
    ( Int, Int )
    -> { a | start : Position, end : Position }
    -> { a | end : Position, start : Position }
shift ( dx, dy ) drawing =
    { drawing
        | start = shiftPosition dx dy drawing.start
        , end = shiftPosition dx dy drawing.end
    }


move : ( Int, Int ) -> Annotation -> Annotation
move translate annotation =
    case annotation of
        Lines lineType line ->
            Lines lineType (shift translate line)

        Shapes shapeType fill shape ->
            Shapes shapeType fill (shift translate shape)

        TextBox textArea ->
            TextBox (shift translate textArea)

        Spotlight shapeType shape ->
            Spotlight shapeType (shift translate shape)

        Pixelate start end ->
            Pixelate (shiftPosition (Tuple.first translate) (Tuple.second translate) start) (shiftPosition (Tuple.first translate) (Tuple.second translate) end)


closeDropdown : Model -> Model
closeDropdown model =
    { model | currentDropdown = Nothing }


toggleDropdown : AttributeDropdown -> Model -> Model
toggleDropdown attributeDropdown model =
    { model
        | currentDropdown =
            case model.currentDropdown of
                Just dropdown ->
                    if dropdown == attributeDropdown then
                        Nothing
                    else
                        Just attributeDropdown

                Nothing ->
                    Just attributeDropdown
        , drawing =
            case attributeDropdown of
                Fonts ->
                    DrawTextBox

                _ ->
                    model.drawing
    }


cancelDrawing : Model -> Model
cancelDrawing model =
    { model | annotationState = ReadyToDraw }


finishEditingText : Int -> Model -> Model
finishEditingText index model =
    { model
        | annotationState = ReadyToDraw
        , edits = UndoList.new (removeItemIf isEmptyTextBox index model.edits.present) model.edits
    }


handleKeyboardShortcuts : KeyChange -> Model -> Model
handleKeyboardShortcuts keyChange model =
    case keyChange of
        KeyDown key ->
            case key of
                CharA ->
                    { model | drawing = DrawLine Arrow }

                CharL ->
                    { model | drawing = DrawLine StraightLine }

                CharR ->
                    { model | drawing = DrawShape Rect }

                CharO ->
                    { model | drawing = DrawShape RoundedRect }

                CharE ->
                    { model | drawing = DrawShape Ellipse }

                CharT ->
                    { model | drawing = DrawTextBox }

                CharG ->
                    { model | drawing = DrawSpotlight Rect }

                CharC ->
                    if isPressed Shift model.keyboardState then
                        model
                    else
                        { model | drawing = DrawSpotlight RoundedRect }

                CharI ->
                    { model | drawing = DrawSpotlight Ellipse }

                CharP ->
                    { model | drawing = DrawPixelate }

                CharN ->
                    toggleDropdown Fonts model

                CharK ->
                    toggleDropdown StrokeColors model

                CharF ->
                    toggleDropdown Fills model

                CharS ->
                    toggleDropdown Strokes model

                _ ->
                    model

        KeyUp key ->
            model


copySelectedAnnotation : Int -> Model -> Model
copySelectedAnnotation index model =
    case Array.get index model.edits.present of
        Just annotation ->
            { model | clipboard = Just annotation }

        Nothing ->
            model


cutSelectedAnnotation : Int -> Model -> Model
cutSelectedAnnotation index model =
    case Array.get index model.edits.present of
        Just annotation ->
            { model
                | clipboard = Just annotation
                , edits = UndoList.new (removeItem index model.edits.present) model.edits
            }

        Nothing ->
            model


handleCopyKey : Int -> OperatingSystem -> Model -> Model
handleCopyKey index osToIgnore model =
    if model.operatingSystem == osToIgnore then
        model
    else if isPressed CharC model.keyboardState then
        copySelectedAnnotation index model
    else
        model


handleSelectedAnnotationKeyboard : Int -> Bool -> KeyChange -> Model -> Model
handleSelectedAnnotationKeyboard index ctrlPressed keyChange model =
    case keyChange of
        KeyDown key ->
            case key of
                CharC ->
                    if ctrlPressed then
                        copySelectedAnnotation index model
                    else
                        model

                CharX ->
                    if ctrlPressed then
                        cutSelectedAnnotation index model
                    else
                        model

                Control ->
                    handleCopyKey index MacOS model

                Super ->
                    handleCopyKey index Windows model

                ContextMenu ->
                    handleCopyKey index Windows model

                _ ->
                    model

        KeyUp key ->
            model


shiftForPaste : Annotation -> Annotation
shiftForPaste annotation =
    case annotation of
        Lines lineType line ->
            Lines lineType { line | start = positionMap ((+) 10) line.start, end = positionMap ((+) 10) line.end }

        Shapes shapeType fill shape ->
            Shapes shapeType fill { shape | start = positionMap ((+) 10) shape.start, end = positionMap ((+) 10) shape.end }

        TextBox textArea ->
            TextBox { textArea | start = positionMap ((+) 10) textArea.start, end = positionMap ((+) 10) textArea.end }

        Spotlight shapeType shape ->
            Spotlight shapeType { shape | start = positionMap ((+) 10) shape.start, end = positionMap ((+) 10) shape.end }

        Pixelate start end ->
            Pixelate (positionMap ((+) 10) start) (positionMap ((+) 10) end)


pasteAnnotation : Model -> Model
pasteAnnotation model =
    case model.clipboard of
        Just annotation ->
            { model
                | edits = UndoList.new (Array.push (shiftForPaste annotation) model.edits.present) model.edits
                , clipboard = Just (shiftForPaste annotation)
            }
                |> selectAnnotation (Array.length model.edits.present)

        Nothing ->
            model


handlePaste : Bool -> KeyChange -> Model -> Model
handlePaste ctrlPressed keyChange model =
    case keyChange of
        KeyDown key ->
            case key of
                CharV ->
                    if ctrlPressed then
                        pasteAnnotation model
                            |> releaseKey CharV
                    else
                        model

                Control ->
                    if model.operatingSystem == MacOS then
                        model
                    else if isPressed CharV model.keyboardState then
                        pasteAnnotation model
                            |> releaseKey CharV
                    else
                        model

                Super ->
                    if model.operatingSystem == Windows then
                        model
                    else if isPressed CharV model.keyboardState then
                        pasteAnnotation model
                            |> releaseKey CharV
                    else
                        model

                _ ->
                    model

        KeyUp key ->
            model


alterDrawingsWithKeyboard : Maybe KeyChange -> Model -> ( Model, List (Cmd Msg) )
alterDrawingsWithKeyboard maybeKeyChange ({ keyboardState } as model) =
    let
        controlKeys =
            case model.operatingSystem of
                MacOS ->
                    [ Super, ContextMenu ]

                Windows ->
                    [ Control ]

        ctrlPressed =
            List.any (\key -> isPressed key model.keyboardState) controlKeys
    in
        case maybeKeyChange of
            Just keyChange ->
                case model.annotationState of
                    ReadyToDraw ->
                        alterDrawing ctrlPressed keyChange model
                            |> handleKeyboardShortcuts keyChange
                            |> handlePaste ctrlPressed keyChange
                            => []

                    DrawingAnnotation _ _ ->
                        alterDrawing ctrlPressed keyChange model
                            => []

                    SelectedAnnotation index _ ->
                        alterDrawing ctrlPressed keyChange model
                            |> handleSelectedAnnotationKeyboard index ctrlPressed keyChange
                            |> handlePaste ctrlPressed keyChange
                            => []

                    MovingAnnotation _ _ _ _ ->
                        alterDrawing ctrlPressed keyChange model
                            => []

                    ResizingAnnotation _ _ ->
                        alterDrawing ctrlPressed keyChange model
                            => []

                    EditingATextBox index _ ->
                        alterTextBoxDrawing keyChange index model

            Nothing ->
                model => []


alterTextBoxDrawing : KeyChange -> Int -> Model -> ( Model, List (Cmd Msg) )
alterTextBoxDrawing keyChange index model =
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


changeDrawing : Drawing -> Model -> Model
changeDrawing drawing model =
    { model | drawing = drawing }


releaseKey : Key -> Model -> Model
releaseKey key model =
    { model | keyboardState = Keyboard.forceRelease [ key ] model.keyboardState }


undoEdit : Model -> Model
undoEdit model =
    { model | edits = UndoList.undo model.edits }


redoEdit : Model -> Model
redoEdit model =
    { model | edits = UndoList.redo model.edits }


alterDrawing : Bool -> KeyChange -> Model -> Model
alterDrawing ctrlPressed keyChange ({ keyboardState } as model) =
    case keyChange of
        KeyDown key ->
            case key of
                Escape ->
                    cancelDrawing model

                Delete ->
                    deleteSelectedDrawing model

                BackSpace ->
                    deleteSelectedDrawing model

                CharZ ->
                    if isPressed Shift keyboardState && ctrlPressed then
                        redoEdit model
                            |> releaseKey CharZ
                    else if ctrlPressed then
                        undoEdit model
                            |> releaseKey CharZ
                    else
                        model

                Control ->
                    if model.operatingSystem == MacOS then
                        model
                    else if isPressed Shift keyboardState && isPressed CharZ keyboardState then
                        redoEdit model
                            |> releaseKey CharZ
                    else if isPressed CharZ keyboardState then
                        undoEdit model
                            |> releaseKey CharZ
                    else
                        model

                Super ->
                    if model.operatingSystem == Windows then
                        model
                    else if isPressed Shift keyboardState && isPressed CharZ keyboardState then
                        redoEdit model
                            |> releaseKey CharZ
                    else if isPressed CharZ keyboardState then
                        undoEdit model
                            |> releaseKey CharZ
                    else
                        model

                _ ->
                    model

        KeyUp _ ->
            model


tryToEdit : Int -> Result Dom.Error () -> Msg
tryToEdit index result =
    case result of
        Ok _ ->
            StartEditingText index

        Err _ ->
            Undo


tryToBlur : Result Dom.Error () -> Msg
tryToBlur result =
    case result of
        Ok _ ->
            ResetToReadyToDraw

        Err _ ->
            Undo


toggleAnnotationMenu : Maybe Int -> Position -> Model -> Model
toggleAnnotationMenu selectedIndex position model =
    case model.annotationMenu of
        Just menu ->
            if menu.index == selectedIndex then
                { model | showingAnyMenu = False, annotationMenu = Nothing }
            else
                { model
                    | showingAnyMenu = True
                    , annotationMenu = Just { index = selectedIndex, position = position }
                }

        Nothing ->
            { model
                | annotationMenu = Just { index = selectedIndex, position = position }
                , showingAnyMenu = True
                , annotationState =
                    case selectedIndex of
                        Just index ->
                            selectAnnotation index model
                                |> .annotationState

                        Nothing ->
                            model.annotationState
            }


bringToFront : Int -> Array Annotation -> Array Annotation
bringToFront index annotations =
    case Array.get index annotations of
        Just annotation ->
            Array.push annotation (Array.append (Array.slice 0 index annotations) (Array.slice (index + 1) (Array.length annotations) annotations))

        Nothing ->
            annotations


bringAnnotationToFront : Int -> Model -> Model
bringAnnotationToFront index model =
    { model
        | edits = UndoList.new (bringToFront index model.edits.present) model.edits
        , annotationState = ReadyToDraw
    }
        |> closeAllMenus


sendToBack : Int -> Array Annotation -> Array Annotation
sendToBack index annotations =
    case Array.get index annotations of
        Just annotation ->
            Array.append (Array.fromList [ annotation ]) (Array.append (Array.slice 0 index annotations) (Array.slice (index + 1) (Array.length annotations) annotations))

        Nothing ->
            annotations


sendAnnotationToBack : Int -> Model -> Model
sendAnnotationToBack index model =
    { model
        | edits = UndoList.new (sendToBack index model.edits.present) model.edits
        , annotationState = ReadyToDraw
    }
        |> closeAllMenus


closeAllMenus : Model -> Model
closeAllMenus model =
    { model | showingAnyMenu = False, annotationMenu = Nothing }


selectImage : Image -> Model -> Model
selectImage image model =
    case model.images of
        Just images ->
            { model
                | images =
                    images
                        |> List.Zipper.first
                        |> List.Zipper.find ((==) image.url << .url)
                , imageSelected = True
                , edits = UndoList.reset model.edits
            }

        Nothing ->
            { model
                | images = List.Zipper.fromList [ image ]
                , imageSelected = True
                , edits = UndoList.reset model.edits
            }


returnToImageSelection : Model -> Model
returnToImageSelection model =
    { model | imageSelected = False, edits = UndoList.reset model.edits }


autoExpandConfig : Int -> AutoExpand.Config Msg
autoExpandConfig index =
    AutoExpand.config
        { onInput = TextBoxInput index
        , padding = 2
        , minRows = 1
        , maxRows = 4
        , attributes =
            [ Attr.id <| "text-box-edit--" ++ toString index
            , Attr.class "text-box-textarea"
            , Attr.attribute "onfocus" "this.select();"
            ]
        }
