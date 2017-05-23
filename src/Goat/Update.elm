module Goat.Update exposing (..)

import Array.Hamt as Array exposing (Array)
import AutoExpand
import Color exposing (Color)
import Dom
import Goat.Annotation as Annotation exposing (Annotation, Drawing(..), EndPosition, LineType(..), Shape, ShapeType(..), StartPosition, TextArea)
import Goat.Annotation.Shared exposing (AnnotationAttributes, DrawingInfo, EditingTextInfo, ResizingInfo, SelectingInfo, StrokeStyle, Vertex)
import Goat.EditState as EditState exposing (EditState, KeyboardConfig)
import Goat.Model exposing (Image, Model, AttributeDropdown(..))
import Goat.Ports as Ports
import Goat.Environment exposing (OperatingSystem(..))
import Goat.Utils exposing (mapAtIndex, removeItemIf, removeItem)
import Keyboard.Extra as Keyboard exposing (Key(..), KeyChange, KeyChange(KeyDown, KeyUp))
import List.Extra
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
    | SelectImage (Result String Image)
    | SetImages (Result String (List Image))
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
                => [ Ports.selectText ("text-box-edit--" ++ toString index) ]

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

        SetImages resultImages ->
            case resultImages of
                Ok images ->
                    { model | images = List.Zipper.fromList images, imageSelected = False }
                        => []

                Err _ ->
                    model => []

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

        SelectImage resultImage ->
            case resultImage of
                Ok image ->
                    model
                        |> selectImage image
                        => []

                Err _ ->
                    model => []

        ChangeDrawing drawing ->
            model
                |> changeDrawing drawing
                |> closeDropdown
                |> resetEditState
                => []

        SelectFill fill ->
            model.editState
                |> EditState.updateAnySelectedAnnotations (updateAnySelectedAnnotationsHelper (Annotation.updateFill fill) model)
                |> Maybe.withDefault model
                |> setFill fill
                |> closeDropdown
                => []

        SelectStrokeColor strokeColor ->
            model.editState
                |> EditState.updateAnySelectedAnnotations (updateAnySelectedAnnotationsHelper (Annotation.updateStrokeColor strokeColor) model)
                |> Maybe.withDefault model
                |> setStrokeColor strokeColor
                |> closeDropdown
                => []

        SelectStrokeStyle strokeStyle ->
            model.editState
                |> EditState.updateAnySelectedAnnotations (updateAnySelectedAnnotationsHelper (Annotation.updateStrokeStyle strokeStyle) model)
                |> Maybe.withDefault model
                |> setStrokeStyle strokeStyle
                |> closeDropdown
                => []

        SelectFontSize fontSize ->
            model.editState
                |> EditState.updateAnySelectedAnnotations (updateAnySelectedAnnotationsHelper (Annotation.updateFontSize fontSize) model)
                |> Maybe.withDefault model
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
                |> startMovingAnnotation start
                => []

        ResetToReadyToDraw ->
            model
                |> resetEditState
                => []

        StartMovingAnnotation index start ->
            model
                |> selectAnnotation index
                |> startMovingAnnotation start
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
                |> resetEditState
                => [ case model.images of
                        Just images ->
                            Ports.exportToImage (List.Zipper.current images).id

                        Nothing ->
                            Cmd.none
                   ]

        ShowMeTheGoats ->
            model
                => [ Ports.requestImages () ]


{-| Do not add this annotations array change to undo history
-}
skipChange : Model -> Array Annotation -> Model
skipChange model annotations =
    { model | edits = UndoList.mapPresent (\_ -> annotations) model.edits }



-- DRAWING


startDrawing : StartPosition -> Model -> Model
startDrawing start model =
    case EditState.startDrawing start model.editState of
        Ok newEditState ->
            { model | editState = newEditState }

        Err _ ->
            model


continueDrawing : Position -> Model -> Model
continueDrawing pos model =
    case EditState.continueDrawing pos (model.drawing == DrawFreeHand) model.editState of
        Ok newEditState ->
            { model | editState = newEditState }

        Err _ ->
            model


finishValidDrawing : Model -> DrawingInfo -> Model
finishValidDrawing model drawingInfo =
    case Annotation.fromDrawing (List.member Shift model.pressedKeys) model.drawing (extractAnnotationAttributes model) drawingInfo of
        Just annotation ->
            addAnnotation annotation model

        Nothing ->
            model


finishNonTextDrawing : EditState -> DrawingInfo -> Model -> Model
finishNonTextDrawing finishedEditState ({ start, curPos } as drawingInfo) model =
    if isDrawingTooSmall (isSpotlightDrawing model.drawing) start curPos then
        resetEditState model
    else
        finishValidDrawing { model | editState = finishedEditState } drawingInfo


finishTextDrawing : Position -> Model -> ( Model, List (Cmd Msg) )
finishTextDrawing pos model =
    let
        numAnnotations =
            Array.length model.edits.present

        attributes =
            AnnotationAttributes model.strokeColor model.fill model.strokeStyle model.fontSize
    in
        case EditState.finishTextDrawing pos numAnnotations attributes model.editState of
            Ok ( newEditState, drawingInfo ) ->
                { model | editState = newEditState }
                    |> addAnnotation (Annotation.newTextBox TextBoxInput numAnnotations attributes drawingInfo)
                    => [ "text-box-edit--"
                            ++ toString numAnnotations
                            |> Dom.focus
                            |> Task.attempt (tryToEdit numAnnotations)
                       ]

            Err _ ->
                model => []


finishDrawing : Position -> Model -> ( Model, List (Cmd Msg) )
finishDrawing pos model =
    case model.drawing of
        DrawTextBox ->
            finishTextDrawing pos model

        _ ->
            case EditState.finishDrawing pos model.editState of
                Ok ( newEditState, drawingInfo ) ->
                    finishNonTextDrawing newEditState drawingInfo model
                        => []

                Err _ ->
                    model => []


resetEditState : Model -> Model
resetEditState model =
    { model | editState = EditState.initialState }


finishMovingAnnotation : Model -> Model
finishMovingAnnotation model =
    case EditState.finishMoving model.editState of
        Ok ( newEditState, { id, translate } ) ->
            { model
                | edits = UndoList.new (mapAtIndex id (Annotation.move translate) model.edits.present) model.edits
                , editState = newEditState
            }

        Err _ ->
            model


updateAnySelectedAnnotationsHelper : (Annotation -> Annotation) -> Model -> Int -> Model
updateAnySelectedAnnotationsHelper fn model index =
    { model
        | edits = UndoList.new (mapAtIndex index fn model.edits.present) model.edits
    }


annotationAttributesInModel : Model -> AnnotationAttributes
annotationAttributesInModel { strokeColor, fill, strokeStyle, fontSize } =
    AnnotationAttributes strokeColor fill strokeStyle fontSize


selectAnnotation : Int -> Model -> Model
selectAnnotation index model =
    model.edits.present
        |> Array.get index
        |> Maybe.andThen (\annotation -> Result.toMaybe <| EditState.selectAnnotation index (Annotation.attributes annotation (annotationAttributesInModel model)) model.editState)
        |> Maybe.map (\newEditState -> { model | editState = newEditState })
        |> Maybe.withDefault model


addAnnotation : Annotation -> Model -> Model
addAnnotation annotation model =
    { model
        | edits = UndoList.new (Array.push annotation model.edits.present) model.edits
    }


startEditingText : Int -> Model -> Model
startEditingText index model =
    model.edits.present
        |> Array.get index
        |> Maybe.andThen (\annotation -> Result.toMaybe <| EditState.startEditingText index (Annotation.attributes annotation (annotationAttributesInModel model)) model.editState)
        |> Maybe.map (\newEditState -> { model | editState = newEditState })
        |> Maybe.withDefault model


editTextBoxAnnotation : Int -> AutoExpand.State -> String -> Model -> Model
editTextBoxAnnotation index autoExpandState autoExpandText model =
    model.edits.present
        |> mapAtIndex index (Annotation.updateTextArea autoExpandState autoExpandText)
        |> skipChange model


setFill : Maybe Color -> Model -> Model
setFill fill model =
    { model
        | editState = EditState.updateSelectedAttributes (Annotation.setFill fill) model.editState
        , fill = fill
    }


setFontSize : Int -> Model -> Model
setFontSize fontSize model =
    { model
        | editState = EditState.updateSelectedAttributes (Annotation.setFontSize fontSize) model.editState
        , fontSize = fontSize
    }


setStrokeStyle : StrokeStyle -> Model -> Model
setStrokeStyle strokeStyle model =
    { model
        | editState = EditState.updateSelectedAttributes (Annotation.setStrokeStyle strokeStyle) model.editState
        , strokeStyle = strokeStyle
    }


setStrokeColor : Color -> Model -> Model
setStrokeColor strokeColor model =
    { model
        | editState = EditState.updateSelectedAttributes (Annotation.setStrokeColor strokeColor) model.editState
        , strokeColor = strokeColor
    }


startMovingAnnotation : Position -> Model -> Model
startMovingAnnotation newPos model =
    case EditState.startMoving newPos model.editState of
        Ok newEditState ->
            { model
                | editState = newEditState
            }

        Err _ ->
            model


moveAnnotation : Position -> Model -> Model
moveAnnotation newPos model =
    case EditState.continueMoving newPos model.editState of
        Ok ( newEditState, _ ) ->
            { model | editState = newEditState }

        Err _ ->
            model


startResizingAnnotation : Int -> Vertex -> StartPosition -> Model -> Model
startResizingAnnotation index vertex start model =
    model.edits.present
        |> Array.get index
        |> Maybe.andThen (\annotation -> Result.toMaybe <| EditState.startResizing start vertex (Annotation.positions annotation) model.editState)
        |> Maybe.map (\newEditState -> { model | editState = newEditState })
        |> Maybe.withDefault model


resizeAnnotation : Position -> Model -> Model
resizeAnnotation curPos model =
    case EditState.continueResizing curPos model.editState of
        Ok ( newEditState, resizingData ) ->
            { model
                | edits = UndoList.mapPresent (mapAtIndex resizingData.id (Annotation.resize (List.member Shift model.pressedKeys) resizingData)) model.edits
                , editState = newEditState
            }

        Err _ ->
            model


finishResizingAnnotation : Model -> Model
finishResizingAnnotation model =
    case EditState.finishResizing model.editState of
        Ok ( newEditState, resizingData ) ->
            { model
                | edits = UndoList.mapPresent (mapAtIndex resizingData.id (Annotation.resize (List.member Shift model.pressedKeys) resizingData)) model.edits
                , editState = newEditState
            }

        Err _ ->
            model


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


finishEditingText : Int -> Model -> Model
finishEditingText index model =
    case EditState.finishEditingText model.editState of
        Ok ( newEditState, _ ) ->
            { model
                | editState = newEditState
                , edits = UndoList.new (removeItemIf Annotation.isEmptyTextBox index model.edits.present) model.edits
            }

        Err _ ->
            model


alterToolbarWithKeyboard : Bool -> KeyChange -> Model -> Model
alterToolbarWithKeyboard ctrlPressed keyChange model =
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
                    { model | drawing = DrawShape Rect, shape = DrawShape Rect }

                CharO ->
                    { model | drawing = DrawShape RoundedRect, shape = DrawShape RoundedRect }

                CharE ->
                    { model | drawing = DrawShape Ellipse, shape = DrawShape Ellipse }

                CharT ->
                    { model | drawing = DrawTextBox }

                CharG ->
                    { model | drawing = DrawSpotlight Rect, spotlight = DrawSpotlight Rect }

                CharC ->
                    if ctrlPressed then
                        model
                    else
                        { model | drawing = DrawSpotlight RoundedRect, spotlight = DrawSpotlight RoundedRect }

                CharI ->
                    { model | drawing = DrawSpotlight Ellipse, spotlight = DrawSpotlight Ellipse }

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
                Delete ->
                    deleteSelectedDrawing index model

                BackSpace ->
                    deleteSelectedDrawing index model

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


pasteAnnotation : Model -> Model
pasteAnnotation model =
    case model.clipboard of
        Just annotation ->
            { model
                | edits = UndoList.new (Array.push (Annotation.shiftForPaste annotation) model.edits.present) model.edits
                , clipboard = Just (Annotation.shiftForPaste annotation)
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


deleteSelectedDrawing : Int -> Model -> Model
deleteSelectedDrawing index model =
    { model
        | edits = UndoList.new (removeItem index model.edits.present) model.edits
        , editState = EditState.initialState
    }


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
                    resetEditState model

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
                , editState =
                    case selectedIndex of
                        Just index ->
                            selectAnnotation index model
                                |> .editState

                        Nothing ->
                            model.editState
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
        , editState = EditState.initialState
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
        , editState = EditState.initialState
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


controlKeys : OperatingSystem -> List Key
controlKeys os =
    case os of
        MacOS ->
            [ Super, ContextMenu ]

        Windows ->
            [ Control ]


keyboardConfig : KeyChange -> KeyboardConfig Model Model
keyboardConfig keyChange =
    { notSelecting = whenNotSelectingKeyboard keyChange
    , drawing = whenDrawingKeyboard keyChange
    , selecting = whenSelectingKeyboard keyChange
    , moving = whenMovingKeyboard keyChange
    , resizing = whenResizingKeyboard keyChange
    , editingText = whenEditingTextKeyboard keyChange
    }


handleKeyboardInteractions : Maybe KeyChange -> Model -> ( Model, List (Cmd Msg) )
handleKeyboardInteractions maybeKeyChange model =
    case maybeKeyChange of
        Just keyChange ->
            EditState.keyboard (keyboardConfig keyChange) model.editState model
                => []

        Nothing ->
            model
                => []


whenNotSelectingKeyboard : KeyChange -> Model -> Model
whenNotSelectingKeyboard keyChange model =
    let
        ctrlPressed =
            isCtrlPressed model.pressedKeys model.operatingSystem
    in
        alterDrawing ctrlPressed keyChange model
            |> alterToolbarWithKeyboard ctrlPressed keyChange
            |> handlePaste ctrlPressed keyChange


whenDrawingKeyboard : KeyChange -> DrawingInfo -> Model -> Model
whenDrawingKeyboard keyChange _ model =
    let
        ctrlPressed =
            isCtrlPressed model.pressedKeys model.operatingSystem
    in
        alterDrawing ctrlPressed keyChange model
            |> alterToolbarWithKeyboard ctrlPressed keyChange


whenSelectingKeyboard : KeyChange -> SelectingInfo -> Model -> Model
whenSelectingKeyboard keyChange { id } model =
    let
        ctrlPressed =
            isCtrlPressed model.pressedKeys model.operatingSystem
    in
        alterDrawing ctrlPressed keyChange model
            |> alterToolbarWithKeyboard ctrlPressed keyChange
            |> handleSelectedAnnotationKeyboard id ctrlPressed keyChange
            |> handlePaste ctrlPressed keyChange


whenMovingKeyboard : KeyChange -> a -> Model -> Model
whenMovingKeyboard keyChange _ model =
    let
        ctrlPressed =
            isCtrlPressed model.pressedKeys model.operatingSystem
    in
        alterDrawing ctrlPressed keyChange model
            |> alterToolbarWithKeyboard ctrlPressed keyChange


whenResizingKeyboard : KeyChange -> a -> Model -> Model
whenResizingKeyboard =
    whenMovingKeyboard


whenEditingTextKeyboard : KeyChange -> a -> Model -> Model
whenEditingTextKeyboard keyChange _ model =
    let
        ctrlPressed =
            isCtrlPressed model.pressedKeys model.operatingSystem
    in
        model


getFirstSpotlightIndex : Array Annotation -> Int
getFirstSpotlightIndex annotations =
    annotations
        |> Array.toList
        |> List.Extra.findIndex Annotation.isSpotlightShape
        |> Maybe.withDefault 0


isSpotlightDrawing : Drawing -> Bool
isSpotlightDrawing drawing =
    case drawing of
        DrawSpotlight _ ->
            True

        _ ->
            False


drawingsAreEqual : Drawing -> Drawing -> Bool
drawingsAreEqual drawing drawing2 =
    case drawing of
        DrawLine lineType ->
            case drawing2 of
                DrawLine lineType2 ->
                    lineType == lineType2

                _ ->
                    False

        DrawFreeHand ->
            drawing2 == DrawFreeHand

        DrawShape shapeType ->
            case drawing2 of
                DrawShape shapeType2 ->
                    shapeType == shapeType2

                _ ->
                    False

        DrawTextBox ->
            case drawing2 of
                DrawTextBox ->
                    True

                _ ->
                    False

        DrawSpotlight shapeType ->
            case drawing2 of
                DrawSpotlight shapeType2 ->
                    shapeType == shapeType2

                _ ->
                    False

        DrawPixelate ->
            drawing2 == DrawPixelate


extractAnnotationAttributes : Model -> AnnotationAttributes
extractAnnotationAttributes { strokeColor, fill, strokeStyle, fontSize } =
    AnnotationAttributes strokeColor fill strokeStyle fontSize


minDrawingDistance : number
minDrawingDistance =
    4


minSpotlightDrawingDistance : number
minSpotlightDrawingDistance =
    8


isDrawingTooSmall : Bool -> StartPosition -> EndPosition -> Bool
isDrawingTooSmall isSpotlight start end =
    if isSpotlight then
        abs (start.x - end.x) < minSpotlightDrawingDistance && abs (start.y - end.y) < minSpotlightDrawingDistance
    else
        abs (start.x - end.x) < minDrawingDistance && abs (start.y - end.y) < minDrawingDistance


isCtrlPressed : List Key -> OperatingSystem -> Bool
isCtrlPressed pressedKeys os =
    List.any (\key -> List.member key pressedKeys) (controlKeys os)
