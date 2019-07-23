module Page.Annotate exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Annotation exposing (Annotation, Choice(..))
import Annotation.Options exposing (StrokeStyle(..))
import Annotation.Vertices exposing (Vertex(..))
import Array exposing (Array)
import AutoExpand
import Browser.Dom as Dom
import Controls
import DrawingArea.Definitions as Definitions
import EditState as EditState exposing (AnnotationConfig, DrawingConfig, EditState, EndPosition, StartPosition, SubscriptionConfig)
import Environment exposing (Environment, OperatingSystem(..), Platform(..))
import Html exposing (Html, a, button, div, li, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, style, title)
import Html.Events exposing (onClick, onMouseDown)
import Icons as Icons
import Image exposing (Image)
import Keyboard exposing (Key(..), KeyChange(..), anyKeyUpper)
import List.Extra
import Log
import Ports
import Position exposing (Position)
import Route
import Session exposing (Session)
import Svg exposing (Svg, svg)
import Svg.Attributes as Attr
import Svg.Lazy as Svg
import Task
import UndoList exposing (UndoList)
import Utils exposing (mapAtIndex, removeItem, toPx)


type alias AnnotationMenu =
    { index : Maybe Int
    , position : Position
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


type SelectedMsg
    = FocusTextArea
    | SelectText
    | StartEditingText
    | PreventTextMouseDown
    | TextBoxInput { textValue : String, state : AutoExpand.State }
    | FinishEditingText
      -- Edit Updates
    | SelectAndMoveAnnotation StartPosition
      -- Move updates
    | StartMovingAnnotation StartPosition
    | FinishMovingAnnotation EndPosition
      -- Resize updates
    | StartResizingAnnotation Vertex StartPosition
    | FinishResizingAnnotation Position
    | FinishedEdit
    | ToggleSelectedAnnotationMenu Position
    | BringAnnotationToFront
    | SendAnnotationToBack


type Msg
    = StartDrawing Position
    | ContinueDrawing Position
    | FinishDrawing Position
    | MoveAnnotation Position
    | ResizeAnnotation Position
    | ResetToReadyToDraw
    | ControlsUpdate Controls.Msg
    | GotSelectedMsg Int SelectedMsg
    | ToggleAnnotationMenu Position
      -- History updates
    | Undo
    | Redo
    | Save
      -- Keyboard updates
    | KeyboardMsg Keyboard.Msg
      -- Modal updates
    | CloseAllMenus
    | LogError String


alterControls : (Controls.State -> Controls.State) -> Model -> Model
alterControls fn model =
    { model | controls = fn model.controls }


updateSelected : Annotation -> SelectedMsg -> Model -> ( Model, Cmd Msg )
updateSelected annotation msg model =
    case msg of
        FocusTextArea ->
            ( startEditingText model
            , textAreaDomId annotation.id
                |> Dom.focus
                |> Task.attempt (tryToEdit annotation.id)
            )

        SelectText ->
            ( model
            , Ports.selectText (textAreaDomId annotation.id)
            )

        StartEditingText ->
            ( model
                |> alterControls Controls.closeDropdown
            , Ports.selectText (textAreaDomId annotation.id)
            )

        PreventTextMouseDown ->
            ( model
            , Cmd.none
            )

        TextBoxInput { state, textValue } ->
            ( model
                |> editTextBoxAnnotation annotation state textValue
                |> alterControls Controls.closeDropdown
            , Cmd.none
            )

        FinishEditingText ->
            ( finishEditingText annotation model
            , Cmd.none
            )

        SelectAndMoveAnnotation start ->
            ( model
                |> selectAnnotation annotation.id
                |> startMovingAnnotation start
            , Cmd.none
            )

        StartMovingAnnotation start ->
            ( model
                |> selectAnnotation annotation.id
                |> startMovingAnnotation start
                |> alterControls Controls.closeDropdown
            , Cmd.none
            )

        FinishMovingAnnotation endPos ->
            ( model
                |> moveAnnotation endPos annotation
                |> finishMovingAnnotation annotation
            , Cmd.none
            )

        StartResizingAnnotation vertex start ->
            ( model
                |> startResizingAnnotation annotation vertex start
                |> resizeAnnotation annotation start
                |> alterControls Controls.closeDropdown
            , Cmd.none
            )

        FinishResizingAnnotation pos ->
            ( model
                |> resizeAnnotation annotation pos
                |> finishResizingAnnotation annotation
            , Cmd.none
            )

        FinishedEdit ->
            finishEdit annotation model

        BringAnnotationToFront ->
            ( bringAnnotationToFront annotation model
            , Cmd.none
            )

        SendAnnotationToBack ->
            ( sendAnnotationToBack annotation model
            , Cmd.none
            )

        ToggleSelectedAnnotationMenu pos ->
            ( model
                |> toggleAnnotationMenu (Just annotation.id) pos
                |> selectAnnotation annotation.id
            , Cmd.none
            )


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
                |> findAnnotation
                |> Maybe.map (\annotation -> continueDrawing pos annotation model)
                |> Maybe.withDefault model
            , Cmd.none
            )

        FinishDrawing pos ->
            case Array.get (Array.length model.edits.present - 1) model.edits.present of
                Just annotation ->
                    finishDrawing annotation pos model

                Nothing ->
                    ( model, Cmd.none )

        MoveAnnotation pos ->
            ( model
                |> findAnnotation
                |> Maybe.map (\annotation -> moveAnnotation pos annotation model)
                |> Maybe.withDefault model
            , Cmd.none
            )

        ResizeAnnotation pos ->
            ( model
                |> findAnnotation
                |> Maybe.map (\annotation -> resizeAnnotation annotation pos model)
                |> Maybe.withDefault model
            , Cmd.none
            )

        GotSelectedMsg id selectedMsg ->
            case Array.get id model.edits.present of
                Just annotation ->
                    updateSelected annotation selectedMsg model

                Nothing ->
                    ( model, Cmd.none )

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

        ResetToReadyToDraw ->
            ( model
                |> resetEditState
            , Cmd.none
            )

        ControlsUpdate controlsMsg ->
            let
                newState =
                    Controls.update controlsMsg model.controls
            in
            ( model.editState
                |> EditState.updateAnySelectedAnnotations (updateAnySelectedAnnotationsHelper (Annotation.setStyles newState.annotationStyles) model)
                |> Maybe.withDefault model
                |> alterControls (always newState)
            , Cmd.none
            )

        ToggleAnnotationMenu pos ->
            ( model
                |> toggleAnnotationMenu Nothing pos
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

        LogError error ->
            ( model, Log.error error )


{-| Do not add this annotations array change to undo history
-}
skipChange : Model -> Array Annotation -> Model
skipChange model annotations =
    { model | edits = UndoList.mapPresent (always annotations) model.edits }


resetEditState : Model -> Model
resetEditState model =
    { model | editState = EditState.initialState }


finishEdit : Annotation -> Model -> ( Model, Cmd Msg )
finishEdit annotation model =
    case EditState.finish (annoConfig model annotation.id) annotation model.editState of
        Ok ( newState, updatedAnnotation ) ->
            ( { model
                | editState = newState
                , edits = UndoList.new (Array.set annotation.id updatedAnnotation model.edits.present) model.edits
              }
            , Cmd.none
            )

        Err _ ->
            ( model, Cmd.none )



-- DRAWING


shouldSnap : Model -> Bool
shouldSnap { pressedKeys } =
    List.member Shift pressedKeys


startDrawing : StartPosition -> Model -> Model
startDrawing start model =
    let
        numAnnotations =
            Array.length model.edits.present

        styles =
            model.controls.annotationStyles

        annotation =
            Annotation.init
                { id = numAnnotations
                , choice = model.controls.annotation
                , start = start
                , end = start
                , positions = []
                , onInput = GotSelectedMsg numAnnotations << TextBoxInput
                , onFocus = GotSelectedMsg numAnnotations FocusTextArea
                , styles = styles
                }
    in
    case EditState.startDrawing annotation model.editState of
        Ok newEditState ->
            { model
                | editState = newEditState
            }
                |> addAnnotation annotation

        Err _ ->
            model


continueDrawing : Position -> Annotation -> Model -> Model
continueDrawing pos annotation model =
    case EditState.continueDrawing pos annotation model.editState of
        Ok ( newEditState, updatedAnnotation ) ->
            { model
                | editState = newEditState
                , edits =
                    UndoList.mapPresent (Array.set annotation.id updatedAnnotation) model.edits
            }

        Err _ ->
            model


textAreaDomId : Int -> String
textAreaDomId id =
    "text-box-edit--" ++ String.fromInt id


annoConfig : Model -> Int -> Annotation.Config Msg
annoConfig model index =
    Annotation.configure
        { onInput = GotSelectedMsg index << TextBoxInput
        , onFocus = GotSelectedMsg index FocusTextArea
        , snap = shouldSnap model
        , eventsForVertex = Nothing
        }


selectText : Int -> Result Dom.Error () -> Msg
selectText index result =
    case result of
        Ok _ ->
            GotSelectedMsg index SelectText

        Err error ->
            LogError (domErrToString error)


finishValidDrawing : Annotation -> Model -> ( Model, Cmd Msg )
finishValidDrawing annotation model =
    let
        numAnnotations =
            Array.length model.edits.present
    in
    ( { model | edits = UndoList.mapPresent (Array.set annotation.id annotation) model.edits }
    , case annotation.choice of
        TextBox ->
            textAreaDomId (numAnnotations - 1)
                |> Dom.focus
                |> Task.attempt (selectText (numAnnotations - 1))

        _ ->
            Cmd.none
    )


finishDrawing : Annotation -> Position -> Model -> ( Model, Cmd Msg )
finishDrawing annotation pos model =
    case EditState.finishDrawing pos annotation model.editState of
        Ok ( newState, Just updatedAnnotation ) ->
            finishValidDrawing updatedAnnotation { model | editState = newState }

        Ok ( newState, Nothing ) ->
            ( { model | editState = newState, edits = UndoList.mapPresent (removeItem annotation.id) model.edits }
            , Cmd.none
            )

        Err _ ->
            ( model, Cmd.none )



-- MOVING


finishMovingAnnotation : Annotation -> Model -> Model
finishMovingAnnotation annotation model =
    case EditState.finishMoving annotation model.editState of
        Ok ( newEditState, updatedAnnotation ) ->
            { model
                | edits = UndoList.new (Array.set annotation.id updatedAnnotation model.edits.present) model.edits
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
        |> Maybe.andThen (\annotation -> Result.toMaybe <| EditState.selectAnnotation annotation model.editState)
        |> Maybe.map (\newEditState -> { model | editState = newEditState })
        |> Maybe.withDefault model


addAnnotation : Annotation -> Model -> Model
addAnnotation annotation model =
    { model
        | edits = UndoList.new (Array.push annotation model.edits.present) model.edits
    }


startEditingText : Model -> Model
startEditingText model =
    EditState.startEditingText model.editState
        |> Result.map (\newState -> { model | editState = newState })
        |> Result.withDefault model


editTextBoxAnnotation : Annotation -> AutoExpand.State -> String -> Model -> Model
editTextBoxAnnotation annotation autoExpandState autoExpandText model =
    model.edits.present
        |> mapAtIndex annotation.id (Annotation.updateTextArea autoExpandState autoExpandText)
        |> skipChange model


startMovingAnnotation : Position -> Model -> Model
startMovingAnnotation newPos model =
    case EditState.startMoving newPos model.editState of
        Ok newEditState ->
            { model | editState = newEditState }

        Err _ ->
            model


moveAnnotation : Position -> Annotation -> Model -> Model
moveAnnotation newPos annotation model =
    case EditState.continueMoving newPos annotation model.editState of
        Ok ( newEditState, updatedAnnotation ) ->
            { model | editState = newEditState, edits = UndoList.mapPresent (Array.set annotation.id updatedAnnotation) model.edits }

        Err _ ->
            model


startResizingAnnotation : Annotation -> Vertex -> StartPosition -> Model -> Model
startResizingAnnotation annotation vertex start model =
    EditState.startResizing start vertex (Annotation.startAndEnd annotation) model.editState
        |> Result.map (\newEditState -> { model | editState = newEditState })
        |> Result.withDefault model


resizeAnnotation : Annotation -> Position -> Model -> Model
resizeAnnotation annotation curPos model =
    case EditState.continueResizing curPos annotation model.editState of
        Ok ( newEditState, updatedAnnotation ) ->
            { model
                | edits = UndoList.mapPresent (Array.set annotation.id updatedAnnotation) model.edits
                , editState = newEditState
            }

        Err _ ->
            model


finishResizingAnnotation : Annotation -> Model -> Model
finishResizingAnnotation annotation model =
    case EditState.finishResizing (annoConfig model annotation.id) annotation model.editState of
        Ok ( newEditState, updatedAnnotation ) ->
            { model
                | edits = UndoList.mapPresent (Array.set annotation.id updatedAnnotation) model.edits
                , editState = newEditState
            }

        Err _ ->
            model


finishEditingText : Annotation -> Model -> Model
finishEditingText annotation model =
    case EditState.finishEditingText annotation model.editState of
        Ok ( newEditState, updatedAnnotation ) ->
            { model
                | editState = newEditState
                , edits = UndoList.mapPresent (Array.set annotation.id updatedAnnotation) model.edits
            }

        Err _ ->
            model


copySelectedAnnotation : Annotation -> Model -> Model
copySelectedAnnotation annotation model =
    { model | clipboard = Just annotation }


cutSelectedAnnotation : Annotation -> Model -> Model
cutSelectedAnnotation annotation model =
    { model
        | clipboard = Just annotation
        , edits = UndoList.new (Array.filter ((/=) annotation.id << .id) model.edits.present) model.edits
    }


pasteAnnotation : Model -> Model
pasteAnnotation model =
    case model.clipboard of
        Just annotation ->
            { model
                | edits = UndoList.new (Array.push (Annotation.move ( 10, 10 ) annotation) model.edits.present) model.edits
                , clipboard = Just (Annotation.move ( 10, 10 ) annotation)
            }
                |> selectAnnotation (Array.length model.edits.present)

        Nothing ->
            model


deleteSelectedAnnotation : Annotation -> Model -> Model
deleteSelectedAnnotation annotation model =
    { model
        | edits = UndoList.new (Array.filter ((/=) annotation.id << .id) model.edits.present) model.edits
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


domErrToString : Dom.Error -> String
domErrToString err =
    case err of
        Dom.NotFound str ->
            str


tryToEdit : Int -> Result Dom.Error () -> Msg
tryToEdit index result =
    case result of
        Ok _ ->
            GotSelectedMsg index StartEditingText

        Err err ->
            LogError (domErrToString err)


tryToBlur : Result Dom.Error () -> Msg
tryToBlur result =
    case result of
        Ok _ ->
            ResetToReadyToDraw

        Err err ->
            LogError (domErrToString err)


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


bringAnnotationToFront : Annotation -> Model -> Model
bringAnnotationToFront annotation model =
    { model
        | edits = UndoList.new (bringToFront annotation.id model.edits.present) model.edits
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


sendAnnotationToBack : Annotation -> Model -> Model
sendAnnotationToBack annotation model =
    { model
        | edits = UndoList.new (sendToBack annotation.id model.edits.present) model.edits
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


findAnnotation : Model -> Maybe Annotation
findAnnotation model =
    model.editState
        |> EditState.selected
        |> Maybe.andThen (\id -> Array.get id model.edits.present)


handleKeyboardInteractions : Environment -> Maybe KeyChange -> Model -> ( Model, Cmd Msg )
handleKeyboardInteractions env maybeKeyChange model =
    case maybeKeyChange of
        Just keyChange ->
            case keyChange of
                KeyDown key ->
                    ( withKeyDown env key (findAnnotation model) model, Cmd.none )

                KeyUp _ ->
                    ( model, Cmd.none )

        Nothing ->
            ( model
            , Cmd.none
            )


withKeyDown : Environment -> Key -> Maybe Annotation -> Model -> Model
withKeyDown env key selected model =
    let
        pressedKeys =
            model.pressedKeys

        ctrlPressed =
            isCtrlPressed pressedKeys env.operatingSystem

        newModel =
            alterControls (Controls.onKeyDown key) model
    in
    case ( key, selected ) of
        ( Escape, _ ) ->
            newModel
                |> alterControls Controls.closeDropdown
                |> resetEditState

        ( Character "V", _ ) ->
            if ctrlPressed then
                pasteAnnotation newModel
                    |> releaseKey (Character "V")

            else
                newModel

        ( Character "Z", _ ) ->
            if List.member Shift pressedKeys && ctrlPressed then
                redoEdit newModel
                    |> releaseKey (Character "Z")

            else if ctrlPressed then
                undoEdit newModel
                    |> releaseKey (Character "Z")

            else
                newModel

        ( Control, _ ) ->
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

        ( Super, _ ) ->
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

        ( Delete, Just annotation ) ->
            deleteSelectedAnnotation annotation newModel

        ( Backspace, Just annotation ) ->
            deleteSelectedAnnotation annotation newModel

        ( Character "C", Just annotation ) ->
            if ctrlPressed then
                copySelectedAnnotation annotation newModel

            else
                newModel

        ( Character "X", Just annotation ) ->
            if ctrlPressed then
                cutSelectedAnnotation annotation newModel

            else
                newModel

        _ ->
            newModel


isCtrlPressed : List Key -> OperatingSystem -> Bool
isCtrlPressed pressedKeys os =
    List.any (\key -> List.member key pressedKeys) (controlKeys os)


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
    , finishDrawing = FinishDrawing
    , finishMoving = \index -> GotSelectedMsg index << FinishMovingAnnotation
    , finishResizing = \index -> GotSelectedMsg index << FinishResizingAnnotation
    , finishEditingText = \index -> GotSelectedMsg index FinishEditingText
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


viewDrawingArea : Model -> Image -> Html Msg
viewDrawingArea model image =
    div
        (canvasAttributes model.editState)
        [ viewSvgArea model (Array.toList model.edits.present) image ]


withMask : List Annotation -> List (Svg Msg) -> List (Svg Msg)
withMask annotations svgs =
    case List.Extra.findIndex (Annotation.isSpotlight << .choice) annotations of
        Just index ->
            List.Extra.intercalate [ Definitions.viewMask ] [ List.take index svgs, List.drop index svgs ]

        Nothing ->
            svgs


viewSvgArea : Model -> List Annotation -> Image -> Svg Msg
viewSvgArea model annotations image =
    let
        svgAnnotations =
            annotations
                |> List.indexedMap (viewAnnotation model)
                |> withMask annotations

        svgs =
            Svg.lazy viewPixelatedImage image :: Svg.lazy viewImage image :: svgAnnotations
    in
    svg
        [ Attr.id "drawing-area"
        , Attr.class "drawing-area"
        , Attr.width (String.fromInt (round image.width))
        , Attr.height (String.fromInt (round image.height))
        , attribute "xmlns" "http://www.w3.org/2000/svg"
        ]
        (Definitions.view (List.indexedMap (viewDef model) annotations) :: svgs)


viewDef : Model -> Int -> Annotation -> Annotation.Def Msg
viewDef model index annotation =
    EditState.viewDef (annotationConfig index (annoConfig model index)) annotation model.editState


viewAnnotation : Model -> Int -> Annotation -> Svg Msg
viewAnnotation model index annotation =
    EditState.view (annotationConfig index (annoConfig model index)) annotation model.editState


annotationConfig : Int -> Annotation.Config Msg -> AnnotationConfig Msg
annotationConfig index config =
    { selectAndMove = GotSelectedMsg index << SelectAndMoveAnnotation
    , contextMenu = GotSelectedMsg index << ToggleSelectedAnnotationMenu
    , startMoving = GotSelectedMsg index << StartMovingAnnotation
    , finishMoving = GotSelectedMsg index << FinishMovingAnnotation
    , resize = \pos vertex -> GotSelectedMsg index (StartResizingAnnotation pos vertex)
    , finishResizing = GotSelectedMsg index << FinishResizingAnnotation
    , annotation = config
    }


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
                    [ viewAnnotationMenuItem (GotSelectedMsg index BringAnnotationToFront) "Bring to Front"
                    , viewAnnotationMenuItem (GotSelectedMsg index SendAnnotationToBack) "Send to Back"
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
    div
        [ class "annotation-app" ]
        [ viewModals model
        , viewModalMask model.annotationMenu
        , viewControls env model
        , viewDrawingArea model image
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


viewControls : Environment -> Model -> Html Msg
viewControls env model =
    div
        [ class "controls" ]
        [ viewNavigationControls
        , viewHistoryControls env.operatingSystem model.edits
        , Html.map ControlsUpdate (Controls.view (controlsConfig model.controls.annotationStyles env.operatingSystem) model.controls)
        ]


controlsConfig : Annotation.Styles -> OperatingSystem -> Controls.Config
controlsConfig styles os =
    Controls.Config styles os


editStateConfig : SubscriptionConfig Msg
editStateConfig =
    { drew = ContinueDrawing
    , resized = ResizeAnnotation
    , moved = MoveAnnotation
    , changedKey = KeyboardMsg
    , clicked = \index -> GotSelectedMsg index FinishEditingText
    , finished = \index -> GotSelectedMsg index FinishedEdit
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ EditState.subscriptions editStateConfig model.editState
        , Sub.map ControlsUpdate (Controls.subscriptions model.controls)
        ]


toSession : Model -> Session
toSession model =
    model.session
