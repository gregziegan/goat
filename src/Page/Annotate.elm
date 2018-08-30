module Page.Annotate exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Annotation exposing (Annotation(..), TextArea)
import Array exposing (Array)
import AutoExpand
import Browser.Dom as Dom
import Color exposing (Color)
import Controls
import Drawing exposing (AttributeDropdown(..), Drawing(..), LineType(..), Shape, ShapeType(..), calcLinePos, calcShapePos, lineAttributes, rectAttrs, shapeAttributes)
import Drawing.Options exposing (DrawingStyles, StrokeColor, StrokeStyle(..))
import DrawingArea
import DrawingArea.Definitions as Definitions
import DrawingArea.Vertices as Vertices exposing (Vertex(..))
import EditState as EditState exposing (AnnotationConfig, DrawingConfig, DrawingInfo, EditState, EndPosition, MovingInfo, SelectState(..), StartPosition, SubscriptionConfig)
import Environment exposing (Environment, OperatingSystem(..), Platform(..))
import EventUtils exposing (stopPropagationAndDefault)
import Html exposing (Html, a, button, div, li, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, style, title)
import Html.Events exposing (onClick, onMouseDown)
import Icons as Icons
import Image exposing (Image)
import Json.Decode as Decode
import Keyboard exposing (Key(..), KeyChange(..), anyKeyUpper)
import List.Extra
import Palette
import Ports
import Position exposing (Position, calcDistance)
import Route
import Session exposing (Session)
import Svg exposing (Svg, foreignObject, rect, svg)
import Svg.Attributes as Attr
import Svg.Lazy as Svg
import Task exposing (succeed)
import UndoList exposing (UndoList)
import Utils exposing (mapAtIndex, removeItem, removeItemIf, toPx)


type alias DrawingModifiers =
    { drawing : Drawing
    , constrain : Bool
    , editState : EditState
    , styles : DrawingStyles
    }


type alias AnnotationMenu =
    { index : Maybe Int
    , position : Position
    }


type alias AnnotationModifiers =
    { config : AnnotationConfig Msg
    , editState : EditState
    , styles : DrawingStyles
    }


type alias Model =
    { -- Annotation Editing State
      edits : UndoList (Array Annotation)
    , editState : EditState
    , clipboard : Maybe Annotation

    -- Control UI State
    , controls : Controls.State

    -- Image Annotator Modals
    , annotationMenu : Maybe AnnotationMenu

    -- Keys pressed
    , pressedKeys : List Key

    -- Session info
    , session : Session
    }


init : Session -> Environment -> ( Model, Cmd msg )
init session environment =
    ( initialModel session
    , case environment.platform of
        Zendesk ->
            Cmd.none

        Web ->
            Cmd.none
    )


initialModel : Session -> Model
initialModel session =
    { edits = UndoList.fresh Array.empty
    , editState = EditState.initialState
    , clipboard = Nothing
    , controls = Controls.initialState
    , annotationMenu = Nothing
    , pressedKeys = []
    , session = session
    }


type Msg
    = StartDrawing Position
    | ContinueDrawing Position
    | FinishDrawing Position
      -- TextArea Updates
    | FocusTextArea Int
    | SelectText Int
    | StartEditingText Int
    | PreventTextMouseDown
    | TextBoxInput Int { textValue : String, state : AutoExpand.State }
    | FinishEditingText Int
    | ResetToReadyToDraw
    | ControlsUpdate Controls.Msg
      -- Edit Updates
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
      -- Keyboard updates
    | KeyboardMsg Keyboard.Msg
      -- Modal updates
    | CloseAllMenus


alterControls : (Controls.State -> Controls.State) -> Model -> Model
alterControls fn model =
    { model | controls = fn model.controls }


update : Environment -> Image -> Msg -> Model -> ( Model, Cmd Msg )
update env image msg ({ pressedKeys } as model) =
    case msg of
        StartDrawing pos ->
            ( model
                |> startDrawing pos
                |> alterControls Controls.closeDropdown
            , Cmd.none
            )

        ContinueDrawing pos ->
            ( model
                |> continueDrawing pos
            , Cmd.none
            )

        FinishDrawing pos ->
            finishDrawing pos model

        FocusTextArea index ->
            ( model
                |> startEditingText index
            , EditState.textAreaDomId index
                |> Dom.focus
                |> Task.attempt (tryToEdit index)
            )

        SelectText index ->
            ( model
            , Ports.selectText (EditState.textAreaDomId index)
            )

        StartEditingText index ->
            ( model
                |> alterControls Controls.closeDropdown
            , Ports.selectText (EditState.textAreaDomId index)
            )

        PreventTextMouseDown ->
            ( model
            , Cmd.none
            )

        TextBoxInput index { state, textValue } ->
            ( model
                |> editTextBoxAnnotation index state textValue
                |> alterControls Controls.closeDropdown
            , Cmd.none
            )

        FinishEditingText index ->
            ( model
                |> finishEditingText index
            , Cmd.none
            )

        KeyboardMsg keyMsg ->
            let
                ( newPressedKeys, maybeKeyChange ) =
                    Keyboard.updateWithKeyChange anyKeyUpper keyMsg pressedKeys
            in
            { model | pressedKeys = newPressedKeys }
                |> handleKeyboardInteractions env maybeKeyChange

        CloseAllMenus ->
            ( model
                |> closeAllMenus
            , Cmd.none
            )

        SelectAndMoveAnnotation index start ->
            ( model
                |> selectAnnotation index
                |> startMovingAnnotation start
            , Cmd.none
            )

        ResetToReadyToDraw ->
            ( model
                |> resetEditState
            , Cmd.none
            )

        ControlsUpdate controlsMsg ->
            let
                ( newState, options ) =
                    Controls.update controlsMsg model.controls
            in
            ( model.editState
                |> EditState.updateAnySelectedAnnotations (updateAnySelectedAnnotationsHelper (Annotation.updateAttributes options.styles) model)
                |> Maybe.withDefault model
                |> alterControls (always newState)
            , Cmd.none
            )

        StartMovingAnnotation index start ->
            ( model
                |> selectAnnotation index
                |> startMovingAnnotation start
                |> alterControls Controls.closeDropdown
            , Cmd.none
            )

        MoveAnnotation newPos ->
            ( model
                |> moveAnnotation newPos
            , Cmd.none
            )

        FinishMovingAnnotation endPos ->
            ( model
                |> moveAnnotation endPos
                |> finishMovingAnnotation
            , Cmd.none
            )

        StartResizingAnnotation index vertex start ->
            ( model
                |> startResizingAnnotation index vertex start
                |> resizeAnnotation start
                |> alterControls Controls.closeDropdown
            , Cmd.none
            )

        ResizeAnnotation pos ->
            ( model
                |> resizeAnnotation pos
            , Cmd.none
            )

        FinishResizingAnnotation pos ->
            ( model
                |> resizeAnnotation pos
                |> finishResizingAnnotation
            , Cmd.none
            )

        BringAnnotationToFront index ->
            ( model
                |> bringAnnotationToFront index
            , Cmd.none
            )

        SendAnnotationToBack index ->
            ( model
                |> sendAnnotationToBack index
            , Cmd.none
            )

        ToggleAnnotationMenu pos ->
            ( model
                |> toggleAnnotationMenu Nothing pos
            , Cmd.none
            )

        ToggleSelectedAnnotationMenu index pos ->
            ( model
                |> toggleAnnotationMenu (Just index) pos
                |> selectAnnotation index
            , Cmd.none
            )

        Undo ->
            ( undoEdit model
            , Cmd.none
            )

        Redo ->
            ( redoEdit model
            , Cmd.none
            )

        Save ->
            ( model
                |> resetEditState
            , Ports.exportToImage image.id
            )


{-| Do not add this annotations array change to undo history
-}
skipChange : Model -> Array Annotation -> Model
skipChange model annotations =
    { model | edits = UndoList.mapPresent (\_ -> annotations) model.edits }


resetEditState : Model -> Model
resetEditState model =
    { model | editState = EditState.initialState }



-- DRAWING


shouldConstrain : Model -> Bool
shouldConstrain { pressedKeys } =
    List.member Shift pressedKeys


startDrawing : StartPosition -> Model -> Model
startDrawing start model =
    case EditState.startDrawing start model.editState of
        Ok newEditState ->
            { model | editState = newEditState }

        Err _ ->
            model


continueDrawing : Position -> Model -> Model
continueDrawing pos model =
    case EditState.continueDrawing pos (Controls.getDrawing model.controls == DrawFreeHand) model.editState of
        Ok newEditState ->
            { model | editState = newEditState }

        Err _ ->
            model


finishValidDrawing : Model -> DrawingStyles -> DrawingInfo -> Model
finishValidDrawing model styles drawingInfo =
    let
        config =
            { constrain = shouldConstrain model
            , drawing = Controls.getDrawing model.controls
            , styles = styles
            }
    in
    case Annotation.fromDrawing config drawingInfo of
        Just annotation ->
            addAnnotation annotation model

        Nothing ->
            model


sumDistance : Position -> ( Float, Position ) -> ( Float, Position )
sumDistance position ( distance, previousPosition ) =
    ( distance
        + calcDistance position previousPosition
    , position
    )


finishFreeHandDrawing : ( EditState, DrawingInfo ) -> DrawingStyles -> Model -> Model
finishFreeHandDrawing ( finishedEditState, { start, curPos, positions } as drawingInfo ) styles model =
    let
        ( totalDistance, _ ) =
            List.foldl sumDistance ( 0.0, start ) (positions ++ [ curPos ])
    in
    if totalDistance < minDrawingDistance then
        resetEditState model

    else
        finishValidDrawing { model | editState = finishedEditState } styles drawingInfo


finishNonTextDrawing : ( EditState, DrawingInfo ) -> DrawingStyles -> Model -> Model
finishNonTextDrawing ( finishedEditState, { start, curPos } as drawingInfo ) styles model =
    if isDrawingTooSmall (Drawing.isSpotlight (Controls.getDrawing model.controls)) start curPos then
        resetEditState model

    else
        finishValidDrawing { model | editState = finishedEditState } styles drawingInfo


finishTextDrawing : Position -> Model -> ( Model, Cmd Msg )
finishTextDrawing pos model =
    let
        numAnnotations =
            Array.length model.edits.present

        styles =
            Controls.styles model.controls
    in
    case EditState.finishTextDrawing pos numAnnotations styles model.editState of
        Ok ( newEditState, drawingInfo ) ->
            ( { model | editState = newEditState }
                |> addAnnotation (Annotation.newTextBox TextBoxInput numAnnotations styles drawingInfo)
            , EditState.textAreaDomId numAnnotations
                |> Dom.focus
                |> Task.attempt (selectText numAnnotations)
            )

        Err _ ->
            ( model, Cmd.none )


selectText : Int -> Result Dom.Error () -> Msg
selectText index result =
    case result of
        Ok _ ->
            SelectText index

        Err _ ->
            Undo


finishDrawing : Position -> Model -> ( Model, Cmd Msg )
finishDrawing pos model =
    let
        styles =
            Controls.styles model.controls
    in
    case Controls.getDrawing model.controls of
        DrawFreeHand ->
            case EditState.finishDrawing pos model.editState of
                Ok newState ->
                    ( finishFreeHandDrawing newState styles model
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        DrawTextBox ->
            finishTextDrawing pos model

        _ ->
            case EditState.finishDrawing pos model.editState of
                Ok newState ->
                    ( finishNonTextDrawing newState styles model
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


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


selectAnnotation : Int -> Model -> Model
selectAnnotation index model =
    model.edits.present
        |> Array.get index
        |> Maybe.andThen (\annotation -> Result.toMaybe <| EditState.selectAnnotation index (Annotation.styles annotation) model.editState)
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
        |> Maybe.andThen (\annotation -> Result.toMaybe <| EditState.startEditingText index (Annotation.styles annotation) model.editState)
        |> Maybe.map (\newEditState -> { model | editState = newEditState })
        |> Maybe.withDefault model


editTextBoxAnnotation : Int -> AutoExpand.State -> String -> Model -> Model
editTextBoxAnnotation index autoExpandState autoExpandText model =
    model.edits.present
        |> mapAtIndex index (Annotation.updateTextArea autoExpandState autoExpandText)
        |> skipChange model


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
        |> Maybe.andThen (\annotation -> Result.toMaybe <| EditState.startResizing start vertex (Annotation.startAndEnd annotation) model.editState)
        |> Maybe.map (\newEditState -> { model | editState = newEditState })
        |> Maybe.withDefault model


resizeAnnotation : Position -> Model -> Model
resizeAnnotation curPos model =
    case EditState.continueResizing curPos model.editState of
        Ok ( newEditState, resizingData ) ->
            { model
                | edits = UndoList.mapPresent (mapAtIndex resizingData.id (Annotation.resize (shouldConstrain model) resizingData)) model.edits
                , editState = newEditState
            }

        Err _ ->
            model


finishResizingAnnotation : Model -> Model
finishResizingAnnotation model =
    case EditState.finishResizing model.editState of
        Ok ( newEditState, resizingData ) ->
            { model
                | edits = UndoList.mapPresent (mapAtIndex resizingData.id (Annotation.resize (shouldConstrain model) resizingData)) model.edits
                , editState = newEditState
            }

        Err _ ->
            model


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


copySelectedAnnotation : Model -> Model
copySelectedAnnotation model =
    EditState.mapSelected
        (\maybeIndex ->
            case maybeIndex of
                Just index ->
                    case Array.get index model.edits.present of
                        Just annotation ->
                            { model | clipboard = Just annotation }

                        Nothing ->
                            model

                Nothing ->
                    model
        )
        model.editState


cutSelectedAnnotation : Model -> Model
cutSelectedAnnotation model =
    EditState.mapSelected
        (\maybeIndex ->
            case maybeIndex of
                Just index ->
                    case Array.get index model.edits.present of
                        Just annotation ->
                            { model
                                | clipboard = Just annotation
                                , edits = UndoList.new (removeItem index model.edits.present) model.edits
                            }

                        Nothing ->
                            model

                Nothing ->
                    model
        )
        model.editState


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


alterTextBoxDrawing : Key -> Model -> ( Model, Cmd Msg )
alterTextBoxDrawing key model =
    EditState.whenTyping
        (\maybeIndex ->
            case maybeIndex of
                Just index ->
                    case key of
                        Escape ->
                            ( finishEditingText index model
                            , "text-box-edit--"
                                ++ String.fromInt index
                                |> Dom.blur
                                |> Task.attempt tryToBlur
                            )

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )
        )
        model.editState


deleteSelectedDrawing : Model -> Model
deleteSelectedDrawing model =
    let
        edits =
            EditState.mapSelected
                (\maybeIndex ->
                    case maybeIndex of
                        Just index ->
                            UndoList.new (removeItem index model.edits.present) model.edits

                        Nothing ->
                            model.edits
                )
                model.editState
    in
    { model
        | edits = edits
        , editState = EditState.initialState
    }


releaseKey : Key -> Model -> Model
releaseKey key model =
    { model | pressedKeys = List.filter ((/=) key) model.pressedKeys }


undoEdit : Model -> Model
undoEdit model =
    { model | edits = UndoList.undo model.edits }


redoEdit : Model -> Model
redoEdit model =
    { model | edits = UndoList.redo model.edits }


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
                { model | annotationMenu = Nothing }

            else
                { model
                    | annotationMenu = Just { index = selectedIndex, position = position }
                }

        Nothing ->
            { model
                | annotationMenu = Just { index = selectedIndex, position = position }
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
    { model | annotationMenu = Nothing, controls = Controls.closeDropdown model.controls }


controlKeys : OperatingSystem -> List Key
controlKeys os =
    case os of
        MacOS ->
            [ Super, ContextMenu ]

        Windows ->
            [ Control ]


handleKeyboardInteractions : Environment -> Maybe KeyChange -> Model -> ( Model, Cmd Msg )
handleKeyboardInteractions env maybeKeyChange model =
    case maybeKeyChange of
        Just keyChange ->
            case keyChange of
                KeyDown key ->
                    withKeyDown env key model
                        |> alterTextBoxDrawing key

                KeyUp _ ->
                    ( model, Cmd.none )

        Nothing ->
            ( model
            , Cmd.none
            )


withKeyDown : Environment -> Key -> Model -> Model
withKeyDown env key model =
    let
        pressedKeys =
            model.pressedKeys

        ctrlPressed =
            isCtrlPressed pressedKeys env.operatingSystem

        newModel =
            alterControls (Controls.onKeyDown key) model
    in
    case key of
        Escape ->
            newModel
                |> alterControls Controls.closeDropdown
                |> resetEditState

        Character "V" ->
            if ctrlPressed then
                pasteAnnotation newModel
                    |> releaseKey (Character "V")

            else
                newModel

        Character "Z" ->
            if List.member Shift pressedKeys && ctrlPressed then
                redoEdit newModel
                    |> releaseKey (Character "Z")

            else if ctrlPressed then
                undoEdit newModel
                    |> releaseKey (Character "Z")

            else
                newModel

        Control ->
            if env.operatingSystem == MacOS then
                newModel

            else if List.member (Character "V") pressedKeys then
                pasteAnnotation newModel
                    |> releaseKey (Character "V")

            else if List.member Shift pressedKeys && List.member (Character "Z") pressedKeys then
                redoEdit newModel
                    |> releaseKey (Character "Z")

            else if List.member (Character "Z") pressedKeys then
                undoEdit newModel
                    |> releaseKey (Character "Z")

            else
                newModel

        Super ->
            if env.operatingSystem == Windows then
                newModel

            else if List.member (Character "V") newModel.pressedKeys then
                pasteAnnotation newModel
                    |> releaseKey (Character "V")

            else if List.member Shift pressedKeys && List.member (Character "Z") pressedKeys then
                redoEdit newModel
                    |> releaseKey (Character "Z")

            else if List.member (Character "Z") pressedKeys then
                undoEdit newModel
                    |> releaseKey (Character "Z")

            else
                newModel

        Delete ->
            deleteSelectedDrawing newModel

        Backspace ->
            deleteSelectedDrawing newModel

        Character "C" ->
            if ctrlPressed then
                copySelectedAnnotation newModel

            else
                newModel

        Character "X" ->
            if ctrlPressed then
                cutSelectedAnnotation newModel

            else
                newModel

        _ ->
            newModel


getFirstSpotlightIndex : Array Annotation -> Int
getFirstSpotlightIndex annotations =
    annotations
        |> Array.toList
        |> List.Extra.findIndex Annotation.isSpotlightShape
        |> Maybe.withDefault 0


minDrawingDistance : number
minDrawingDistance =
    4


minSpotlightDrawingDistance : number
minSpotlightDrawingDistance =
    8


isDrawingTooSmall : Bool -> StartPosition -> EndPosition -> Bool
isDrawingTooSmall isSpotlightDrawing start end =
    if isSpotlightDrawing then
        abs (start.x - end.x) < minSpotlightDrawingDistance && abs (start.y - end.y) < minSpotlightDrawingDistance

    else
        abs (start.x - end.x) < minDrawingDistance && abs (start.y - end.y) < minDrawingDistance


isCtrlPressed : List Key -> OperatingSystem -> Bool
isCtrlPressed pressedKeys os =
    List.any (\key -> List.member key pressedKeys) (controlKeys os)


toDrawingModifiers : Model -> DrawingModifiers
toDrawingModifiers model =
    DrawingModifiers (Controls.getDrawing model.controls) (shouldConstrain model) model.editState (Controls.styles model.controls)


translateArrowHead : Int -> StartPosition -> EndPosition -> MovingInfo -> List (Svg.Attribute Msg)
translateArrowHead index start end { translate, id } =
    let
        theta =
            (2 * pi)
                - Position.angle start end

        ( dx, dy ) =
            translate
    in
    if index == id then
        [ Attr.transform ("translate(" ++ String.fromInt dx ++ "," ++ String.fromInt dy ++ ") rotate(" ++ String.fromFloat (-theta * (180 / pi)) ++ " " ++ String.fromInt end.x ++ " " ++ String.fromInt end.y ++ ")") ]

    else
        []


viewDrawing : DrawingModifiers -> Bool -> Svg Msg
viewDrawing drawingModifiers isInMask =
    drawingModifiers.editState
        |> EditState.viewDrawing (viewDrawingHelper drawingModifiers isInMask)
        |> Maybe.withDefault (Svg.text "")


solidLine : StartPosition -> EndPosition -> StrokeColor -> Shape
solidLine start end strokeColor =
    Shape start end strokeColor SolidMedium


viewDrawingHelper : DrawingModifiers -> Bool -> DrawingInfo -> Svg Msg
viewDrawingHelper { drawing, constrain, styles } isInMask { start, curPos, positions } =
    let
        { strokeColor, fill, strokeStyle } =
            styles

        lineAttrs lineType =
            lineAttributes lineType (Shape start (calcLinePos constrain start curPos) strokeColor strokeStyle)

        shapeAttrs shapeType =
            shapeAttributes shapeType (Shape start (calcShapePos constrain start curPos) strokeColor strokeStyle) fill

        spotlightAttrs shapeType =
            if isInMask then
                shapeAttributes shapeType (Shape start (calcShapePos constrain start curPos) strokeColor strokeStyle) (Just Palette.black)

            else
                shapeAttributes shapeType (Shape start (calcShapePos constrain start curPos) strokeColor strokeStyle) Nothing
    in
    case drawing of
        DrawLine lineType ->
            case lineType of
                Arrow ->
                    Drawing.viewArrow
                        (arrowConfig
                            (solidLine start (calcLinePos constrain start curPos) strokeColor)
                            (lineAttrs Arrow)
                            []
                        )

                StraightLine ->
                    Svg.path (lineAttrs lineType) []

        DrawFreeHand ->
            Drawing.viewFreeDraw (Shape start curPos strokeColor strokeStyle) positions []

        DrawShape shapeType ->
            case shapeType of
                Rect ->
                    Svg.rect (shapeAttrs shapeType) []

                RoundedRect ->
                    Svg.rect (shapeAttrs shapeType) []

                Ellipse ->
                    Svg.ellipse (shapeAttrs shapeType) []

        DrawTextBox ->
            Svg.rect ((shapeAttributes Rect <| Shape start curPos Palette.gray Drawing.Options.defaults.strokeStyle) Nothing ++ [ Attr.strokeWidth "1" ]) []

        DrawSpotlight shapeType ->
            case shapeType of
                Rect ->
                    Svg.rect (spotlightAttrs shapeType) []

                RoundedRect ->
                    Svg.rect (spotlightAttrs shapeType) []

                Ellipse ->
                    Svg.ellipse (spotlightAttrs shapeType) []

        DrawPixelate ->
            Svg.text ""


arrowConfig : Shape -> List (Svg.Attribute Msg) -> List (Svg.Attribute Msg) -> Drawing.ArrowAttributes Msg
arrowConfig shape attrs head =
    { headAttributes = head
    , bodyAttributes = lineAttributes Arrow shape ++ attrs
    , start = shape.start
    , end = shape.end
    , strokeColor = shape.strokeColor
    }


viewLine : Int -> EditState -> List (Svg.Attribute Msg) -> LineType -> Shape -> Svg Msg
viewLine index editState attrs lineType shape =
    case lineType of
        StraightLine ->
            Svg.path (lineAttributes lineType shape ++ attrs) []

        Arrow ->
            Drawing.viewArrow
                (arrowConfig shape
                    attrs
                    (List.append attrs
                        (editState
                            |> EditState.ifMoving
                            |> Maybe.map (translateArrowHead index shape.start shape.end)
                            |> Maybe.withDefault []
                        )
                    )
                )


viewFreeDraw : SelectState -> List (Svg.Attribute Msg) -> Shape -> List Position -> Svg Msg
viewFreeDraw selectState attrs shape positions =
    let
        leftMostX =
            List.Extra.minimumBy .x positions
                |> Maybe.map .x
                |> Maybe.withDefault 0

        rightMostX =
            List.Extra.maximumBy .x positions
                |> Maybe.map .x
                |> Maybe.withDefault 0

        topMostY =
            List.Extra.minimumBy .y positions
                |> Maybe.map .y
                |> Maybe.withDefault 0

        bottomMostY =
            List.Extra.maximumBy .y positions
                |> Maybe.map .y
                |> Maybe.withDefault 0
    in
    Svg.g attrs
        (Svg.path (Drawing.freeDrawAttributes shape positions) []
            :: (if selectState == Selected then
                    [ Svg.rect
                        [ Attr.x (String.fromInt (leftMostX - 5))
                        , Attr.y (String.fromInt (topMostY - 5))
                        , Attr.width (String.fromInt (10 + rightMostX - leftMostX))
                        , Attr.height (String.fromInt (10 + bottomMostY - topMostY))
                        , Attr.stroke "#555"
                        , Attr.strokeWidth "0.5"
                        , Attr.strokeDasharray "10, 5"
                        , Attr.fill "none"
                        , Attr.strokeLinejoin "round"
                        , Attr.pointerEvents "none"
                        ]
                        []
                    ]

                else
                    []
               )
        )


viewShape : List (Svg.Attribute Msg) -> ShapeType -> Maybe Color -> Shape -> Svg Msg
viewShape attrs shapeType fill shape =
    case shapeType of
        Rect ->
            Svg.rect (shapeAttributes shapeType shape fill ++ attrs) []

        RoundedRect ->
            Svg.rect (shapeAttributes shapeType shape fill ++ attrs) []

        Ellipse ->
            Svg.ellipse (shapeAttributes shapeType shape fill ++ attrs) []


viewTextArea : Int -> TextArea -> Html Msg
viewTextArea index ({ start, end, fill, fontSize, autoexpand } as textArea) =
    foreignObject []
        [ div
            [ class "text-box-container"
            , style "top" (toPx (-10 + Basics.min start.y end.y))
            , style "left" (toPx (-20 + Basics.min start.x end.x))
            , style "width" (toPx (abs (end.x - start.x)))
            , style "font-size" (toPx fontSize)
            , style "color" (Color.toHexString fill)
            , stopPropagationAndDefault "mousedown" (Decode.succeed PreventTextMouseDown)
            ]
            [ AutoExpand.view (Annotation.autoExpandConfig TextBoxInput index fontSize) autoexpand textArea.text
            ]
        ]


svgTextOffsetX : Int
svgTextOffsetX =
    Annotation.textareaPadding - 20


svgTextOffsetY : Int
svgTextOffsetY =
    -20 + Annotation.textareaPadding + 6


toTSpan : TextArea -> String -> Svg msg
toTSpan { fontSize, start, end, fill } spanText =
    Svg.tspan
        [ Attr.dy <| String.fromFloat <| Annotation.fontSizeToLineHeight fontSize
        , Attr.x <| String.fromInt <| (svgTextOffsetX + Basics.min start.x end.x)
        , Attr.fill (Color.toHexString fill)
        , Attr.fontSize <| String.fromInt fontSize
        ]
        [ Svg.text spanText ]


viewTextBoxWithVertices : Msg -> List (Svg.Attribute Msg) -> TextArea -> Svg Msg
viewTextBoxWithVertices onDoubleClick attrs ({ start, end, fill, fontSize } as textArea) =
    textArea.text
        |> String.split "\n"
        |> List.map (toTSpan textArea)
        |> Svg.text_
            ([ Attr.y <| String.fromInt <| (svgTextOffsetY + Basics.min start.y end.y)
             , Html.Events.onDoubleClick <| onDoubleClick
             , Attr.stroke <|
                if textArea.fill == Palette.black then
                    "white"

                else
                    "black"
             , Attr.strokeWidth "0.5px"
             , Attr.fontSize <| String.fromInt fontSize
             , Attr.fontFamily "sans-serif"
             ]
                ++ attrs
            )


viewTextAnnotation : List (Svg.Attribute Msg) -> TextArea -> Svg Msg
viewTextAnnotation attrs textArea =
    textArea.text
        |> String.split "\n"
        |> List.map (toTSpan textArea)
        |> Svg.text_ ([ Attr.y <| String.fromInt <| (svgTextOffsetY + Basics.min textArea.start.y textArea.end.y), Attr.fontFamily "sans-serif" ] ++ attrs)


viewTextBox : List (Svg.Attribute Msg) -> SelectState -> Int -> TextArea -> Svg Msg
viewTextBox attrs selectState index textArea =
    case selectState of
        Selected ->
            viewTextArea index textArea

        NotSelected ->
            viewTextAnnotation attrs textArea

        SelectedWithVertices ->
            viewTextBoxWithVertices (FocusTextArea index) attrs textArea


annotationConfig : DrawingStyles -> AnnotationConfig Msg
annotationConfig defaultAttributes =
    { selectAndMove = SelectAndMoveAnnotation
    , contextMenu = ToggleSelectedAnnotationMenu
    , startMoving = StartMovingAnnotation
    , finishMoving = FinishMovingAnnotation
    , defaultAttributes = defaultAttributes
    }


viewPixelate : AnnotationModifiers -> Int -> Annotation -> Maybe (List (Svg Msg))
viewPixelate modifiers index annotation =
    case annotation of
        Pixelate start end ->
            Just [ Svg.rect (EditState.annotationEvents modifiers.config index modifiers.editState ++ rectAttrs start end ++ [ Attr.fill (Color.toHexString Palette.black) ]) [] ]

        _ ->
            Nothing


type alias AnnotationView =
    Svg Msg


type alias VerticesView =
    Svg Msg


viewAnnotation : AnnotationModifiers -> Int -> Annotation -> ( AnnotationView, Maybe VerticesView )
viewAnnotation modifiers index annotation =
    let
        selectState =
            EditState.selectState index (not (Annotation.isFreeHand annotation)) modifiers.editState

        editStateAttrs =
            EditState.annotationEvents modifiers.config index modifiers.editState

        eventsForVertex =
            EditState.vertexEvents (StartResizingAnnotation index) modifiers.editState

        vertices verticesType { start, end } =
            case selectState of
                SelectedWithVertices ->
                    Just
                        (Vertices.view
                            { kind = verticesType
                            , start = start
                            , end = end
                            , eventsForVertex = eventsForVertex
                            }
                        )

                _ ->
                    Nothing
    in
    case annotation of
        Lines lineType shape ->
            ( viewLine index modifiers.editState editStateAttrs lineType shape
            , vertices Vertices.Linear shape
            )

        FreeDraw shape positions ->
            ( viewFreeDraw selectState editStateAttrs shape positions
            , Nothing
            )

        Shapes shapeType fill shape ->
            ( viewShape editStateAttrs shapeType fill shape
            , vertices Vertices.Rectangular shape
            )

        TextBox textBox ->
            ( viewTextBox editStateAttrs selectState index textBox
            , Nothing
            )

        Spotlight shapeType shape ->
            ( viewShape editStateAttrs shapeType Nothing shape
            , vertices Vertices.Rectangular shape
            )

        Pixelate start end ->
            ( Svg.rect (rectAttrs start end ++ [ Attr.fill "none", Attr.style "pointer-events: all;" ] ++ editStateAttrs) []
            , vertices Vertices.Rectangular { start = start, end = end }
            )


viewSpotlightInMask : AnnotationModifiers -> ( Int, Annotation ) -> Maybe (Svg Msg)
viewSpotlightInMask modifiers annotationById =
    spotlightToMaskCutout annotationById
        |> Maybe.map (viewSpotlightInMaskHelper modifiers)


viewSpotlightInMaskHelper : AnnotationModifiers -> ( Int, ShapeType, Shape ) -> Svg Msg
viewSpotlightInMaskHelper modifiers ( index, shapeType, shape ) =
    viewShape (EditState.annotationEvents modifiers.config index modifiers.editState) shapeType (Just Palette.black) shape


spotlightToMaskCutout : ( Int, Annotation ) -> Maybe ( Int, ShapeType, Shape )
spotlightToMaskCutout ( index, annotation ) =
    case annotation of
        Spotlight shapeType shape ->
            Just ( index, shapeType, shape )

        _ ->
            Nothing


viewModals : Model -> Html Msg
viewModals model =
    case model.annotationMenu of
        Just { index, position } ->
            viewAnnotationMenu position index

        Nothing ->
            text ""


viewModalMask : Maybe modal -> Html Msg
viewModalMask modal =
    let
        showingAnyMenu =
            case modal of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    div
        [ classList [ ( "modal-mask", True ), ( "hidden", not showingAnyMenu ) ]
        , onClick CloseAllMenus
        ]
        []


viewPixelatedImage : Image -> Svg Msg
viewPixelatedImage { width, height, url } =
    Svg.image
        [ Attr.width (String.fromInt (round width))
        , Attr.height (String.fromInt (round height))
        , Attr.xlinkHref url
        , Attr.filter "url(#pixelate)"
        ]
        []


viewImage : Image -> Svg Msg
viewImage { url, width, height } =
    Svg.image
        [ Attr.class "image-to-annotate"
        , Attr.width (String.fromInt (round width))
        , Attr.height (String.fromInt (round height))
        , Attr.xlinkHref url
        , Attr.mask "url(#pixelateMask)"
        ]
        []


drawingConfig : DrawingConfig Msg
drawingConfig =
    { startDrawing = StartDrawing
    , continueDrawing = ContinueDrawing
    , finishDrawing = FinishDrawing
    , continueMoving = MoveAnnotation
    , finishMoving = FinishMovingAnnotation
    , continueResizing = ResizeAnnotation
    , finishResizing = FinishResizingAnnotation
    , finishEditingText = FinishEditingText
    , contextMenu = ToggleAnnotationMenu
    }


canvasAttributes : EditState -> List (Svg.Attribute Msg)
canvasAttributes editState =
    [ id "canvas"
    , class "image-edit"
    , Html.Events.onMouseDown CloseAllMenus
    , Html.Attributes.contextmenu "annotation-menu"
    ]
        ++ EditState.drawingEvents drawingConfig editState


viewAnnotations : Array Annotation -> List (Svg Msg) -> List (Svg Msg) -> Bool -> List (Svg Msg)
viewAnnotations annotations spotlights nonSpotlights isDrawingSpotlight =
    let
        firstSpotlightIndex =
            getFirstSpotlightIndex annotations
    in
    if isDrawingSpotlight && List.isEmpty spotlights then
        nonSpotlights ++ [ DrawingArea.viewMask ]

    else if List.isEmpty spotlights then
        nonSpotlights

    else
        List.take firstSpotlightIndex nonSpotlights
            ++ (DrawingArea.viewMask :: List.drop firstSpotlightIndex nonSpotlights)


type alias IsInMask =
    Bool


type alias MaskInsertsAndAnnotations =
    { spotlights : List (Svg Msg)
    , pixelates : List (Svg Msg)
    , imagesAndAnnotations : List (Svg Msg)
    }


viewDrawingAndAnnotations : MaskInsertsAndAnnotations -> Bool -> (IsInMask -> Svg Msg) -> List (Svg Msg)
viewDrawingAndAnnotations { spotlights, pixelates, imagesAndAnnotations } isSpotlightDrawing toDrawing =
    if isSpotlightDrawing then
        List.concat
            [ List.singleton (Definitions.view (spotlights ++ [ toDrawing True ]) pixelates)
            , imagesAndAnnotations
            , List.singleton (toDrawing False)
            ]

    else
        List.concat
            [ List.singleton (Definitions.view spotlights pixelates)
            , imagesAndAnnotations
            , List.singleton (toDrawing False)
            ]


insertIfPixelate : Array Annotation -> List (Svg Msg) -> List (Svg Msg) -> Drawing -> DrawingInfo -> ( Array Annotation, List (Svg Msg) )
insertIfPixelate annotations spotlights nonSpotlights drawing { start, curPos } =
    case drawing of
        DrawPixelate ->
            ( Array.push (Pixelate start curPos) annotations
            , viewAnnotations annotations spotlights nonSpotlights (Drawing.isSpotlight drawing)
            )

        _ ->
            ( annotations, viewAnnotations annotations spotlights nonSpotlights (Drawing.isSpotlight drawing) )


viewSpotlights : AnnotationModifiers -> Array Annotation -> List (Svg Msg)
viewSpotlights modifiers annotations =
    annotations
        |> Array.toIndexedList
        |> List.filterMap (viewSpotlightInMask modifiers)


viewNonSpotlightAnnotations : AnnotationModifiers -> Array Annotation -> List (Svg Msg)
viewNonSpotlightAnnotations modifiers annotations =
    let
        annotationsAndVertices =
            annotations
                |> Array.toList
                |> List.indexedMap (viewAnnotation modifiers)
    in
    List.map Tuple.first annotationsAndVertices
        ++ List.filterMap Tuple.second annotationsAndVertices


viewPixelates : AnnotationModifiers -> Array Annotation -> List (Svg Msg)
viewPixelates modifiers annotations =
    annotations
        |> Array.toIndexedList
        |> List.filterMap (\( index, annotation ) -> viewPixelate modifiers index annotation)
        |> List.concat


maskInsertsAndAnnotations : Image -> Drawing -> AnnotationModifiers -> Array Annotation -> MaskInsertsAndAnnotations
maskInsertsAndAnnotations image drawing modifiers annotations =
    let
        spotlights =
            viewSpotlights modifiers annotations

        ( pixelates, svgAnnotations ) =
            modifiers.editState
                |> EditState.viewDrawing (insertIfPixelate annotations spotlights nonSpotlights drawing)
                |> Maybe.withDefault ( annotations, viewAnnotations annotations spotlights nonSpotlights False )
                |> Tuple.mapFirst (viewPixelates modifiers)

        nonSpotlights =
            viewNonSpotlightAnnotations modifiers annotations

        imagesAndAnnotations =
            Svg.lazy viewPixelatedImage image :: Svg.lazy viewImage image :: svgAnnotations
    in
    MaskInsertsAndAnnotations spotlights pixelates imagesAndAnnotations


viewDrawingArea : DrawingModifiers -> AnnotationModifiers -> Array Annotation -> Image -> Html Msg
viewDrawingArea ({ drawing, editState } as drawingModifiers) annotationModifiers annotations image =
    div
        (canvasAttributes editState)
        [ svg
            [ Attr.id "drawing-area"
            , Attr.class "drawing-area"
            , Attr.width (String.fromInt (round image.width))
            , Attr.height (String.fromInt (round image.height))
            , attribute "xmlns" "http://www.w3.org/2000/svg"
            ]
            (viewDrawingAndAnnotations
                (maskInsertsAndAnnotations image drawing annotationModifiers annotations)
                (Drawing.isSpotlight drawing)
                (viewDrawing drawingModifiers)
            )
        ]


viewAnnotationMenu : Position -> Maybe Int -> Html Msg
viewAnnotationMenu pos selectedIndex =
    div
        [ id "annotation-menu"
        , class "annotation-menu"
        , style "top" (toPx pos.y)
        , style "left" (toPx pos.x)
        ]
        [ ul [ class "annotation-menu__list" ]
            (case selectedIndex of
                Just index ->
                    [ viewAnnotationMenuItem (BringAnnotationToFront index) "Bring to Front"
                    , viewAnnotationMenuItem (SendAnnotationToBack index) "Send to Back"
                    ]

                Nothing ->
                    [ viewDisabledAnnotationMenuItem "Bring to Front"
                    , viewDisabledAnnotationMenuItem "Send to Back"
                    ]
            )
        ]


viewDisabledAnnotationMenuItem : String -> Html msg
viewDisabledAnnotationMenuItem buttonText =
    li [ class "annotation-menu__item" ]
        [ button
            [ class "annotation-menu__button"
            , disabled True
            ]
            [ text buttonText ]
        ]


viewAnnotationMenuItem : Msg -> String -> Html Msg
viewAnnotationMenuItem msg buttonText =
    li [ class "annotation-menu__item" ]
        [ button
            [ class "annotation-menu__button"
            , onClick msg
            ]
            [ text buttonText ]
        ]


view : Environment -> Image -> Model -> { title : String, content : Html Msg }
view environment image model =
    { title = "Annotate images"
    , content = content environment image model
    }


content : Environment -> Image -> Model -> Html Msg
content env image model =
    let
        styles =
            Controls.styles model.controls

        annotationModifiers =
            { config = annotationConfig styles
            , editState = model.editState
            , styles = styles
            }
    in
    div
        [ class "annotation-app" ]
        [ viewModals model
        , viewModalMask model.annotationMenu
        , viewControls env model annotationModifiers.styles
        , viewDrawingArea (toDrawingModifiers model) annotationModifiers model.edits.present image
        ]


viewNavigationControls : Html Msg
viewNavigationControls =
    div [ class "navigation-controls" ]
        [ a [ Route.href Route.Gallery, class "cancel-button" ] [ text "Back" ]
        , button [ onClick Save, class "save-button" ] [ text "Save" ]
        ]


viewHistoryControls : OperatingSystem -> UndoList a -> Html Msg
viewHistoryControls os edits =
    div [ class "history-controls" ]
        [ button
            [ onClick Undo
            , class "history-button"
            , disabled ((not << UndoList.hasPast) edits)
            , title <|
                case os of
                    MacOS ->
                        "Undo (Cmd-Z)"

                    Windows ->
                        "Undo (Ctrl + Z)"
            ]
            [ Icons.viewUndoArrow ]
        , button
            [ onClick Redo
            , class "history-button flip"
            , disabled ((not << UndoList.hasFuture) edits)
            , title <|
                case os of
                    MacOS ->
                        "Redo (Cmd-Shift-Z)"

                    Windows ->
                        "Redo (Ctrl + Shift + Z)"
            ]
            [ Icons.viewUndoArrow ]
        ]


viewControls : Environment -> Model -> DrawingStyles -> Html Msg
viewControls env model annotationAttrs =
    div
        [ class "controls" ]
        [ viewNavigationControls
        , viewHistoryControls env.operatingSystem model.edits
        , Html.map ControlsUpdate (Controls.view (controlsConfig annotationAttrs env.operatingSystem) model.controls)
        ]


controlsConfig : DrawingStyles -> OperatingSystem -> Controls.Config
controlsConfig styles os =
    Controls.Config styles os


editStateConfig : SubscriptionConfig Msg
editStateConfig =
    { drawToMsg = ContinueDrawing
    , resizeToMsg = ResizeAnnotation
    , moveToMsg = MoveAnnotation
    , keyboardToMsg = KeyboardMsg
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ EditState.subscriptions editStateConfig model.editState
        , Sub.map KeyboardMsg Keyboard.subscriptions
        , Sub.map ControlsUpdate (Controls.subscriptions model.controls)
        ]


toSession : Model -> Session
toSession model =
    model.session
