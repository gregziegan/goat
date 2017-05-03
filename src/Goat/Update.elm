module Goat.Update exposing (..)

import Array.Hamt as Array exposing (Array)
import AutoExpand
import Color exposing (Color)
import Dom
import Goat.Flags exposing (Image)
import Goat.Model exposing (..)
import Goat.Ports as Ports
import Goat.Utils exposing (calcLinePos, calcShapePos, currentAnnotationAttributes, drawingsAreEqual, getAnnotationAttributes, getPositions, isDrawingTooSmall, isEmptyTextBox, isSpotlightDrawing, mapAtIndex, positionMap, positionMapX, removeItem, removeItemIf, shiftPosition)
import Html.Attributes as Attr
import Keyboard.Extra as Keyboard exposing (Key(..), KeyChange, KeyChange(KeyDown, KeyUp))
import List.Zipper exposing (Zipper)
import Mouse exposing (Position, position)
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
    | WaitForDropdownToggle AttributeDropdown
    | CancelDropdownWait
    | ToggleDropdown AttributeDropdown
    | ChangeDrawing Drawing
    | CloseDropdown
    | CloseOpenDrawingDropdowns
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
update msg ({ fill, fontSize, strokeColor, strokeStyle, images, pressedKeys, drawing } as model) =
    case msg of
        StartDrawing pos ->
            model
                |> startDrawing pos
                |> closeDropdown
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
                |> closeDropdown
                => []

        PreventTextMouseDown ->
            model
                => []

        TextBoxInput index { state, textValue } ->
            model
                |> editTextBoxAnnotation index state textValue
                |> closeDropdown
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
                | imageSelected = False
            }
                => []

        ReturnToImageSelection ->
            model
                |> returnToImageSelection
                => []

        KeyboardMsg keyMsg ->
            let
                ( newPressedKeys, maybeKeyChange ) =
                    Keyboard.updateWithKeyChange keyMsg pressedKeys
            in
                { model | pressedKeys = newPressedKeys }
                    |> handleKeyboardInteractions maybeKeyChange

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

        WaitForDropdownToggle attributeDropdown ->
            model
                |> waitForDropdownToggle attributeDropdown
                |> closeDropdown
                => []

        CancelDropdownWait ->
            model
                |> cancelWaitForDropdownToggle
                => []

        ToggleDropdown editOption ->
            model
                |> toggleDropdown editOption
                |> cancelWaitForDropdownToggle
                => []

        CloseDropdown ->
            model
                |> closeDropdown
                => []

        CloseOpenDrawingDropdowns ->
            model
                |> closeOpenDrawingDropdowns
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
                |> closeDropdown
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
                |> closeDropdown
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
            { model
                | annotationState = DrawingAnnotation start pos
                , freeDrawPositions =
                    case model.drawing of
                        DrawFreeHand ->
                            case model.freeDrawPositions of
                                [] ->
                                    [ pos ]

                                lastPos :: rest ->
                                    if (abs (lastPos.x - pos.x)) < 10 && (abs (lastPos.y - pos.y)) < 10 then
                                        model.freeDrawPositions
                                    else
                                        pos :: model.freeDrawPositions

                        _ ->
                            model.freeDrawPositions
            }

        _ ->
            model


finishDrawing : Position -> Model -> ( Model, List (Cmd Msg) )
finishDrawing pos ({ fill, strokeColor, strokeStyle, fontSize, freeDrawPositions } as model) =
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

                    DrawFreeHand ->
                        finishFreeDrawing start pos freeDrawPositions model
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
        |> addAnnotation (Lines lineType (Shape start (calcLinePos (List.member Shift model.pressedKeys) start end) model.strokeColor model.strokeStyle))


finishFreeDrawing : StartPosition -> EndPosition -> List Position -> Model -> Model
finishFreeDrawing start end positions model =
    model
        |> addAnnotation (FreeDraw (Shape start end model.strokeColor model.strokeStyle) positions)
        |> clearFreeDrawPositions


finishPixelateDrawing : StartPosition -> EndPosition -> Model -> Model
finishPixelateDrawing start end model =
    model
        |> addAnnotation (Pixelate start end)


finishShapeDrawing : StartPosition -> EndPosition -> ShapeType -> Model -> Model
finishShapeDrawing start end shapeType model =
    model
        |> addAnnotation (Shapes shapeType model.fill (Shape start (calcShapePos (List.member Shift model.pressedKeys) start end) model.strokeColor model.strokeStyle))


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
        |> addAnnotation (Spotlight shapeType (Shape start (calcShapePos (List.member Shift model.pressedKeys) start end) model.strokeColor model.strokeStyle))


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


clearFreeDrawPositions : Model -> Model
clearFreeDrawPositions model =
    { model | freeDrawPositions = [] }


updateStrokeColor : Color -> Annotation -> Annotation
updateStrokeColor strokeColor annotation =
    case annotation of
        Lines lineType line ->
            Lines lineType { line | strokeColor = strokeColor }

        FreeDraw shape positions ->
            FreeDraw { shape | strokeColor = strokeColor } positions

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

        FreeDraw _ _ ->
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

        FreeDraw shape positions ->
            FreeDraw { shape | strokeStyle = strokeStyle } positions

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
                | edits = UndoList.mapPresent (mapAtIndex resizingData.index (resize (List.member Shift model.pressedKeys) { resizingData | curPos = curPos })) model.edits
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
resizeVertices constrain { curPos, vertex, originalCoords } annotation =
    let
        ( start, end ) =
            originalCoords
    in
        case vertex of
            Start ->
                { annotation | start = constrain annotation.end curPos }

            Goat.Model.End ->
                { annotation | end = constrain annotation.start curPos }

            StartPlusX ->
                { annotation | start = constrain annotation.end curPos, end = Position start.x end.y }

            StartPlusY ->
                { annotation | start = constrain annotation.end curPos, end = Position end.x start.y }


resize : Bool -> ResizingData -> Annotation -> Annotation
resize constrain resizingData annotation =
    case annotation of
        Lines lineType shape ->
            Lines lineType (resizeVertices (calcLinePos constrain) resizingData shape)

        FreeDraw _ _ ->
            annotation

        Shapes shapeType fill shape ->
            Shapes shapeType fill (resizeVertices (calcShapePos constrain) resizingData shape)

        TextBox textArea ->
            TextBox (resizeVertices (calcShapePos False) resizingData textArea)

        Spotlight shapeType shape ->
            Spotlight shapeType (resizeVertices (calcShapePos constrain) resizingData shape)

        Pixelate start end ->
            Pixelate (resizeVertices (calcShapePos constrain) resizingData { start = start, end = end }).start (resizeVertices (calcShapePos constrain) resizingData { start = start, end = end }).end


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

        FreeDraw shape positions ->
            FreeDraw (shift translate shape) (List.map (shiftPosition (Tuple.first translate) (Tuple.second translate)) positions)

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


closeOpenDrawingDropdowns : Model -> Model
closeOpenDrawingDropdowns model =
    { model
        | currentDropdown =
            if model.currentDropdown == (Just ShapesDropdown) || model.currentDropdown == (Just SpotlightsDropdown) then
                Nothing
            else
                model.currentDropdown
    }


waitForDropdownToggle : AttributeDropdown -> Model -> Model
waitForDropdownToggle attributeDropdown model =
    { model
        | waitingForDropdownToggle = Just attributeDropdown
        , drawing =
            case attributeDropdown of
                ShapesDropdown ->
                    model.shape

                SpotlightsDropdown ->
                    model.spotlight

                _ ->
                    model.drawing
    }


cancelWaitForDropdownToggle : Model -> Model
cancelWaitForDropdownToggle model =
    { model | waitingForDropdownToggle = Nothing }


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
                ShapesDropdown ->
                    model.shape

                SpotlightsDropdown ->
                    model.spotlight

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


alterToolbarWithKeyboard : Bool -> Maybe KeyChange -> Model -> Model
alterToolbarWithKeyboard ctrlPressed keyChangeMaybe model =
    case keyChangeMaybe of
        Just keyChange ->
            case keyChange of
                KeyDown key ->
                    case key of
                        Escape ->
                            closeDropdown model

                        CharA ->
                            { model | drawing = DrawLine Arrow }

                        CharH ->
                            { model | drawing = DrawFreeHand }

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
                            if ctrlPressed then
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

                KeyUp _ ->
                    model

        Nothing ->
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
    else if (List.member CharC model.pressedKeys) then
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
            Lines lineType { line | start = shiftPosition 10 10 line.start, end = shiftPosition 10 10 line.end }

        FreeDraw shape positions ->
            FreeDraw { shape | start = shiftPosition 10 10 shape.start, end = shiftPosition 10 10 shape.end } (List.map (shiftPosition 10 10) positions)

        Shapes shapeType fill shape ->
            Shapes shapeType fill { shape | start = shiftPosition 10 10 shape.start, end = shiftPosition 10 10 shape.end }

        TextBox textArea ->
            TextBox { textArea | start = shiftPosition 10 10 textArea.start, end = shiftPosition 10 10 textArea.end }

        Spotlight shapeType shape ->
            Spotlight shapeType { shape | start = shiftPosition 10 10 shape.start, end = shiftPosition 10 10 shape.end }

        Pixelate start end ->
            Pixelate (shiftPosition 10 10 start) (shiftPosition 10 10 end)


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
                    else if List.member CharV model.pressedKeys then
                        pasteAnnotation model
                            |> releaseKey CharV
                    else
                        model

                Super ->
                    if model.operatingSystem == Windows then
                        model
                    else if List.member CharV model.pressedKeys then
                        pasteAnnotation model
                            |> releaseKey CharV
                    else
                        model

                _ ->
                    model

        KeyUp key ->
            model


handleKeyboardInteractions : Maybe KeyChange -> Model -> ( Model, List (Cmd Msg) )
handleKeyboardInteractions maybeKeyChange ({ pressedKeys } as model) =
    let
        controlKeys =
            case model.operatingSystem of
                MacOS ->
                    [ Super, ContextMenu ]

                Windows ->
                    [ Control ]

        ctrlPressed =
            List.any (\key -> List.member key pressedKeys) controlKeys
    in
        case maybeKeyChange of
            Just keyChange ->
                case model.annotationState of
                    ReadyToDraw ->
                        alterDrawing ctrlPressed keyChange model
                            |> alterToolbarWithKeyboard ctrlPressed maybeKeyChange
                            |> handlePaste ctrlPressed keyChange
                            => []

                    DrawingAnnotation _ _ ->
                        alterDrawing ctrlPressed keyChange model
                            |> alterToolbarWithKeyboard ctrlPressed maybeKeyChange
                            => []

                    SelectedAnnotation index _ ->
                        alterDrawing ctrlPressed keyChange model
                            |> alterToolbarWithKeyboard ctrlPressed maybeKeyChange
                            |> handleSelectedAnnotationKeyboard index ctrlPressed keyChange
                            |> handlePaste ctrlPressed keyChange
                            => []

                    MovingAnnotation _ _ _ _ ->
                        alterDrawing ctrlPressed keyChange model
                            |> alterToolbarWithKeyboard ctrlPressed maybeKeyChange
                            => []

                    ResizingAnnotation _ _ ->
                        alterDrawing ctrlPressed keyChange model
                            |> alterToolbarWithKeyboard ctrlPressed maybeKeyChange
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


changeShapeAndSpotlightDropdowns : Drawing -> Model -> Model
changeShapeAndSpotlightDropdowns drawing model =
    case drawing of
        DrawLine lineType ->
            case lineType of
                Arrow ->
                    model

                StraightLine ->
                    { model | shape = drawing }

        DrawFreeHand ->
            model

        DrawShape _ ->
            { model | shape = drawing }

        DrawTextBox ->
            model

        DrawSpotlight _ ->
            { model | spotlight = drawing }

        DrawPixelate ->
            model


changeDrawing : Drawing -> Model -> Model
changeDrawing drawing model =
    { model | drawing = drawing }
        |> changeShapeAndSpotlightDropdowns drawing


releaseKey : Key -> Model -> Model
releaseKey key model =
    { model | pressedKeys = List.filter ((/=) key) model.pressedKeys }


undoEdit : Model -> Model
undoEdit model =
    { model | edits = UndoList.undo model.edits }


redoEdit : Model -> Model
redoEdit model =
    { model | edits = UndoList.redo model.edits }


alterDrawing : Bool -> KeyChange -> Model -> Model
alterDrawing ctrlPressed keyChange ({ pressedKeys } as model) =
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
                    if List.member Shift pressedKeys && ctrlPressed then
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
                    else if List.member Shift pressedKeys && List.member CharZ pressedKeys then
                        redoEdit model
                            |> releaseKey CharZ
                    else if List.member CharZ pressedKeys then
                        undoEdit model
                            |> releaseKey CharZ
                    else
                        model

                Super ->
                    if model.operatingSystem == Windows then
                        model
                    else if List.member Shift pressedKeys && List.member CharZ pressedKeys then
                        redoEdit model
                            |> releaseKey CharZ
                    else if List.member CharZ pressedKeys then
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
