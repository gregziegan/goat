module Goat.Update exposing (..)

import Array.Hamt as Array exposing (Array)
import AutoExpand
import Color exposing (Color)
import Dom
import Goat.Helpers exposing (..)
import Goat.Model exposing (..)
import Goat.Ports as Ports
import Html.Attributes as Attr
import Keyboard.Extra as Keyboard exposing (Key(..), KeyChange, KeyChange(..), isPressed)
import List.Zipper exposing (Zipper)
import Mouse exposing (Position)
import Rocket exposing ((=>))
import Task exposing (succeed)
import UndoList exposing (UndoList)


type Msg
    = StartDrawing StartPosition
    | ContinueDrawing StartPosition
    | FinishDrawing StartPosition EndPosition
      -- TextArea Updates
    | StartEditingText Int TextArea
    | SwitchToEditingText Int
    | FinishEditingText Int
    | SetText Int TextArea String
    | AutoExpandInput Int { textValue : String, state : AutoExpand.State }
      -- Annotation Attribute updates
    | SelectFill Fill
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
    | MoveAnnotation Int StartPosition Position
    | FinishMovingAnnotation Int StartPosition Position
      -- Resize updates
    | StartResizingAnnotation Int Vertex StartPosition
    | ResizeAnnotation Position
    | FinishResizingAnnotation Position
      -- Annotation menu updates
    | ToggleAnnotationMenu Int Position
    | BringAnnotationToFront Int
    | SendAnnotationToBack Int
      -- History updates
    | Undo
    | Redo
    | Save
      -- Image Selection updates
    | SelectImage Image
    | SetImages (List Image)
    | Cancel
      -- Keyboard updates
    | KeyboardMsg Keyboard.Msg
      -- Modal updates
    | CloseAllMenus
      -- !!! GOATS !!!
    | ShowMeTheGoats


update : Msg -> Model -> ( Model, List (Cmd Msg) )
update msg ({ edits, fill, fontSize, strokeColor, strokeStyle, mouse, images, keyboardState, drawing } as model) =
    case msg of
        StartDrawing pos ->
            model
                |> startDrawing pos
                => []

        FinishDrawing start end ->
            if isDrawingTooSmall (isSpotlightDrawing model.drawing) start end then
                model
                    |> cancelDrawing
                    => []
            else
                finishDrawing start end model

        StartEditingText index textArea ->
            model.edits.present
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
            model.edits.present
                |> Array.set index (TextBox { textBox | text = newText })
                |> skipChange model
                => []

        FinishEditingText index ->
            model
                |> finishEditingText index
                => []

        AutoExpandInput index { state, textValue } ->
            model.edits.present
                |> mapAtIndex index (autoExpandAnnotation state textValue)
                |> skipChange model
                => []

        ContinueDrawing pos ->
            model
                |> setMouse pos
                => []

        SetImages images ->
            { model | images = List.Zipper.fromList images }
                => []

        Cancel ->
            { model | imageSelected = False, edits = UndoList.reset model.edits }
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

        MoveAnnotation index start newPos ->
            model
                |> moveAnnotation index start newPos
                => []

        FinishMovingAnnotation index start newPos ->
            model
                |> moveAnnotation index start newPos
                |> finishMovingAnnotation index
                => []

        StartResizingAnnotation index vertex start ->
            model
                |> setMouse start
                |> selectAnnotation index
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

        ToggleAnnotationMenu index pos ->
            model
                |> toggleAnnotationMenu index pos
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


resetSelection : Model -> Model
resetSelection model =
    { model | annotationState = ReadyToDraw }


startDrawing : StartPosition -> Model -> Model
startDrawing start model =
    { model | annotationState = DrawingAnnotation start, mouse = start }


resetToReadyToDraw : Model -> Model
resetToReadyToDraw model =
    { model | annotationState = ReadyToDraw }


finishMovingAnnotation : Int -> Model -> Model
finishMovingAnnotation index model =
    case model.annotationState of
        MovingAnnotation int startPosition translate ->
            { model
                | annotationState = SelectedAnnotation index
                , edits = UndoList.new (mapAtIndex index (move translate) model.edits.present) model.edits
            }

        _ ->
            model


updateAnySelectedAnnotations : (Annotation -> Annotation) -> Model -> Model
updateAnySelectedAnnotations fn model =
    case model.annotationState of
        SelectedAnnotation index ->
            { model
                | edits = UndoList.new (mapAtIndex index fn model.edits.present) model.edits
                , annotationState = SelectedAnnotation index
            }

        EditingATextBox index ->
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
    { model | annotationState = SelectedAnnotation index }


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
            TextBox <| TextArea start end strokeColor fontSize "Text" 0 (AutoExpand.initState (config numAnnotations))
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
                    |> addAnnotation (TextBox <| TextArea start (calcShapePos start end DrawingShape) strokeColor fontSize "Text" 0 (AutoExpand.initState (config numAnnotations)))
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


updateFontSize : Int -> Annotation -> Annotation
updateFontSize fontSize annotation =
    case annotation of
        TextBox textBox ->
            TextBox { textBox | fontSize = fontSize }

        _ ->
            annotation


setFill : Fill -> Model -> Model
setFill fill model =
    { model | fill = fill }


setFontSize : Int -> Model -> Model
setFontSize fontSize model =
    { model | fontSize = fontSize }


setStrokeStyle : StrokeStyle -> Model -> Model
setStrokeStyle strokeStyle model =
    { model | strokeStyle = strokeStyle }


setStrokeColor : Color -> Model -> Model
setStrokeColor strokeColor model =
    { model | strokeColor = strokeColor }


startMovingAnnotation : Int -> Position -> Model -> Model
startMovingAnnotation index newPos model =
    case Array.get index model.edits.present of
        Just annotation ->
            { model
                | annotationState = MovingAnnotation index newPos ( 0, 0 )
            }

        Nothing ->
            model


getPositions : Annotation -> ( StartPosition, EndPosition )
getPositions annotation =
    case annotation of
        Lines lineType line ->
            line.start => line.end

        Shapes shapeType shape ->
            shape.start => shape.end

        TextBox textArea ->
            textArea.start => textArea.end

        Spotlight shapeType shape ->
            shape.start => shape.end


moveAnnotation : Int -> StartPosition -> Position -> Model -> Model
moveAnnotation index start newPos model =
    case model.annotationState of
        MovingAnnotation index start ( dx, dy ) ->
            { model | annotationState = MovingAnnotation index start ( newPos.x - start.x, newPos.y - start.y ) }

        _ ->
            model


startResizingAnnotation : Int -> Vertex -> StartPosition -> Model -> Model
startResizingAnnotation index vertex start model =
    case Array.get index model.edits.present of
        Just annotation ->
            { model | annotationState = ResizingAnnotation index start vertex (getPositions annotation) }

        Nothing ->
            model


resizeAnnotation : Position -> Model -> Model
resizeAnnotation newPos model =
    case model.annotationState of
        ResizingAnnotation index start vertex originalCords ->
            { model
                | edits = UndoList.mapPresent (mapAtIndex index (resize start newPos vertex originalCords)) model.edits
            }

        _ ->
            model


finishResizingAnnotation : Model -> Model
finishResizingAnnotation model =
    case model.annotationState of
        ResizingAnnotation index _ _ _ ->
            { model | annotationState = SelectedAnnotation index }

        _ ->
            model


resizeVertices : Position -> Vertex -> ( StartPosition, EndPosition ) -> { a | start : Position, end : Position } -> { a | start : Position, end : Position }
resizeVertices pos vertex ( start, end ) annotation =
    case vertex of
        Start ->
            { annotation | start = pos }

        Goat.Model.End ->
            { annotation | end = pos }

        StartPlusX ->
            { annotation | start = pos, end = Position start.x end.y }

        StartPlusY ->
            { annotation | start = pos, end = Position end.x start.y }


resizeEllipseVertices : Position -> Vertex -> ( StartPosition, EndPosition ) -> { a | start : Position, end : Position } -> { a | start : Position, end : Position }
resizeEllipseVertices pos vertex ( start, end ) annotation =
    let
        dX =
            start.x - pos.x

        dY =
            end.y - pos.y
    in
        case vertex of
            Start ->
                { annotation | start = Position (pos.x + ((end.x - pos.x) // 2)) (end.y - (dY // 2)) }

            Goat.Model.End ->
                { annotation | end = pos }

            StartPlusX ->
                { annotation | start = Position (pos.x + ((end.x - pos.x) // 2) - ((end.x - start.x) // 2)) (end.y - (dY // 2)), end = Position start.x end.y }

            StartPlusY ->
                { annotation | start = Position (pos.x + ((end.x - pos.x) // 2)) (start.y - ((start.y - pos.y) // 2)), end = Position end.x start.y }


resize : StartPosition -> EndPosition -> Vertex -> ( StartPosition, EndPosition ) -> Annotation -> Annotation
resize start end vertex originalCoords annotation =
    case annotation of
        Lines lineType line ->
            Lines lineType (resizeVertices end vertex originalCoords line)

        Shapes shapeType shape ->
            case shapeType of
                Ellipse ->
                    Shapes shapeType (resizeEllipseVertices end vertex originalCoords shape)

                _ ->
                    Shapes shapeType (resizeVertices end vertex originalCoords shape)

        TextBox textArea ->
            TextBox (resizeVertices end vertex originalCoords textArea)

        Spotlight shapeType shape ->
            Spotlight shapeType (resizeVertices end vertex originalCoords shape)


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

        Shapes shapeType shape ->
            Shapes shapeType (shift translate shape)

        TextBox textArea ->
            TextBox (shift translate textArea)

        Spotlight shapeType shape ->
            Spotlight shapeType (shift translate shape)


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


handleKeyboardShortcuts : KeyChange -> Model -> Model
handleKeyboardShortcuts keyChange model =
    case keyChange of
        KeyDown key ->
            case key of
                Number1 ->
                    { model | drawing = selectLine Arrow model.keyboardState }

                Number2 ->
                    { model | drawing = selectLine StraightLine model.keyboardState }

                Number3 ->
                    { model | drawing = selectShape Rect model.keyboardState }

                Number4 ->
                    { model | drawing = selectShape RoundedRect model.keyboardState }

                Number5 ->
                    { model | drawing = selectShape Ellipse model.keyboardState }

                Number6 ->
                    { model | drawing = DrawTextBox }

                Number7 ->
                    { model | drawing = selectSpotlight Rect model.keyboardState }

                Number8 ->
                    { model | drawing = selectSpotlight RoundedRect model.keyboardState }

                Number9 ->
                    { model | drawing = selectSpotlight Ellipse model.keyboardState }

                CharQ ->
                    toggleDropdown Fonts model

                CharW ->
                    toggleDropdown StrokeColors model

                CharE ->
                    toggleDropdown Fills model

                CharR ->
                    toggleDropdown Strokes model

                _ ->
                    model

        KeyUp key ->
            model


alterDrawingsWithKeyboard : Maybe KeyChange -> Model -> ( Model, List (Cmd Msg) )
alterDrawingsWithKeyboard maybeKeyChange ({ keyboardState } as model) =
    case maybeKeyChange of
        Just keyChange ->
            case model.annotationState of
                ReadyToDraw ->
                    alterDrawing keyChange model
                        |> handleKeyboardShortcuts keyChange
                        => []

                EditingATextBox index ->
                    alterTextBoxDrawing keyChange index model

                _ ->
                    alterDrawing keyChange model
                        => []

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
        SelectedAnnotation index ->
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


alterDrawing : KeyChange -> Model -> Model
alterDrawing keyChange ({ keyboardState } as model) =
    let
        controlKey =
            case model.operatingSystem of
                MacOS ->
                    Super

                Windows ->
                    Control
    in
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


toggleAnnotationMenu : Int -> Position -> Model -> Model
toggleAnnotationMenu index position model =
    if model.annotationMenu == Nothing then
        { model | annotationMenu = Just { index = index, position = position }, showingAnyMenu = True }
    else
        { model | showingAnyMenu = False }


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


config : Int -> AutoExpand.Config Msg
config index =
    AutoExpand.config
        { onInput = AutoExpandInput index
        , padding = 2
        , minRows = 1
        , maxRows = 4
        , attributes =
            [ Attr.id <| "text-box-edit--" ++ toString index
            , Attr.class "text-box-textarea"
            , Attr.attribute "onfocus" "this.select();"
            ]
        }
