port module Annotator exposing (..)

import Array exposing (Array)
import AutoExpand
import Color exposing (Color)
import Color.Convert
import Dom
import Html exposing (Attribute, Html, button, div, p, text)
import Html.Attributes as Html exposing (class, classList, disabled, height, id, readonly, src, start, style, type_, width)
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput, onMouseEnter, onMouseLeave, onWithOptions)
import Json.Decode as Json
import Keyboard.Extra as Keyboard exposing (Key(..), KeyChange(..), isPressed)
import List.Extra
import List.Zipper exposing (Zipper)
import Mouse exposing (Position)
import Rocket exposing ((=>))
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import Svg.Events as SE
import Task exposing (succeed)
import UndoList exposing (UndoList)


-- MODEL


type alias Flags =
    { isMac : Bool }


type alias StartPosition =
    Position


type alias EndPosition =
    Position


type ArrowMode
    = DrawingArrow StartPosition
    | DrawingDiscreteArrow StartPosition


type RectMode
    = DrawingRect StartPosition
    | DrawingSquare StartPosition


type RoundedRectMode
    = DrawingRoundedRect StartPosition
    | DrawingRoundedSquare StartPosition


type EllipseMode
    = DrawingOval StartPosition
    | DrawingCircle StartPosition


type LineMode
    = DrawingLine StartPosition
    | DrawingDiscreteLine StartPosition


type Drawing
    = DrawArrow ArrowMode
    | DrawRect RectMode
    | DrawRoundedRect RoundedRectMode
    | DrawEllipse EllipseMode
    | DrawTextBox StartPosition
    | DrawLine LineMode
    | DrawSpotlightRect RoundedRectMode


type EditMode
    = EditRect
    | EditRoundedRect
    | EditEllipse
    | EditArrow
    | EditLine
    | EditTextBox
    | EditSpotlightRect


type alias Rect =
    { start : Position
    , end : Position
    , fill : Fill
    , strokeColor : Color
    , stroke : LineStroke
    , strokeStyle : StrokeStyle
    , rounded : Bool
    }


type Fill
    = SolidFill Color
    | EmptyFill
    | MaskFill
    | SpotlightFill


type alias Ellipse =
    { start : Position
    , end : Position
    , fill : Fill
    , strokeColor : Color
    , stroke : LineStroke
    , strokeStyle : StrokeStyle
    }


type alias TextBox =
    { start : Position
    , end : Position
    , text : String
    , fill : Color
    , stroke : LineStroke
    , fontSize : Float
    , angle : Float
    , autoexpand : AutoExpand.State
    }


type alias Line =
    { start : Position
    , end : Position
    , fill : Color
    , stroke : LineStroke
    , strokeStyle : StrokeStyle
    }


type alias Image =
    { url : String
    , width : Float
    , height : Float
    , originalWidth : Float
    , originalHeight : Float
    }


type LineStroke
    = VeryThin
    | Thin
    | Medium
    | Thick
    | VeryThick


type StrokeStyle
    = Solid
    | Dotted
    | Dashed


type EditOption
    = Fonts
    | Fills
    | StrokeColors
    | Strokes


type Annotation
    = Arrow_ Line
    | Rect_ Rect
    | Ellipse_ Ellipse
    | TextBox_ TextBox
    | Line_ Line


type alias EditState =
    { annotations : Array ( Annotation, SelectState )
    , drawing : Maybe Drawing
    }


type SelectState
    = Selected
    | NotSelected
    | SelectedWithVertices


type Vertex
    = Start
    | End
    | StartPlusX
    | StartPlusY


type OperatingSystem
    = MacOS
    | Windows


type MovementState
    = ReadyToDraw
    | DrawingAnnotation
    | HoveringOverAnnotation
    | HoveringOverSelectedAnnotation
    | HoveringOverVertex
    | OutsideSelectedAnnotation
    | MovingAnnotation Int Annotation StartPosition
    | ResizingAnnotation Int Annotation StartPosition Vertex
    | EditingATextBox Int


type alias Model =
    { edits : UndoList EditState
    , fill : Fill
    , strokeColor : Color
    , stroke : LineStroke
    , strokeStyle : StrokeStyle
    , fontSize : Float
    , editMode : EditMode
    , mouse : Mouse.Position
    , keyboardState : Keyboard.State
    , images : Maybe (Zipper Image)
    , imageSelected : Bool
    , currentDropdown : Maybe EditOption
    , movementState : MovementState
    , operatingSystem : OperatingSystem
    }


strokeColorOptions : List Color
strokeColorOptions =
    [ Color.red
    , Color.orange
    , Color.yellow
    , Color.green
    , Color.blue
    , Color.purple
    , Color.brown
    , Color.black
    , Color.white
    ]


fillOptions : List Fill
fillOptions =
    [ EmptyFill
    , SolidFill Color.red
    , SolidFill Color.orange
    , SolidFill Color.yellow
    , SolidFill Color.green
    , SolidFill Color.blue
    , SolidFill Color.purple
    , SolidFill Color.brown
    , SolidFill Color.black
    , SolidFill Color.white
    ]


lineStrokeOptions : List LineStroke
lineStrokeOptions =
    [ VeryThin
    , Thin
    , Medium
    , Thick
    , VeryThick
    ]


strokeStyles : List StrokeStyle
strokeStyles =
    [ Solid
    , Dotted
    , Dashed
    ]


drawingOptions : List EditMode
drawingOptions =
    [ EditArrow
    , EditLine
    , EditRect
    , EditRoundedRect
    , EditEllipse
    , EditTextBox
    , EditSpotlightRect
    ]


fontSizes : List Float
fontSizes =
    [ 10
    , 12
    , 14
    , 16
    , 18
    , 20
    ]


initialEditState : EditState
initialEditState =
    { annotations = Array.empty
    , drawing = Nothing
    }


init : Flags -> ( Model, List (Cmd Msg) )
init flags =
    { edits = UndoList.fresh initialEditState
    , fill = EmptyFill
    , strokeColor = Color.red
    , stroke = Medium
    , strokeStyle = Solid
    , fontSize = 14
    , editMode = EditArrow
    , mouse = Mouse.Position 0 0
    , keyboardState = Keyboard.initialState
    , images = List.Zipper.fromList []
    , imageSelected = False
    , currentDropdown = Nothing
    , movementState = ReadyToDraw
    , operatingSystem =
        if flags.isMac then
            MacOS
        else
            Windows
    }
        => []



-- UPDATE


type Msg
    = -- Line Updates
      StartLine StartPosition
    | AddLine StartPosition EndPosition
    | StartArrow StartPosition
    | AddArrow StartPosition EndPosition
      -- Shape Updates
    | StartRect StartPosition
    | AddRect StartPosition EndPosition
    | StartRoundedRect StartPosition
    | AddRoundedRect StartPosition EndPosition
    | StartEllipse StartPosition
    | AddEllipse StartPosition EndPosition
      -- TextBox Updates
    | StartTextBox StartPosition
    | AddTextBox StartPosition EndPosition
    | StartEditingText Int TextBox
    | SwitchToEditingText Int
    | FinishEditingText Int
    | SetText Int TextBox String
    | AutoExpandInput Int { textValue : String, state : AutoExpand.State }
      -- Spotlight Updates
    | StartSpotlightRect StartPosition
    | AddSpotlightRect StartPosition EndPosition
    | ChangeEditMode EditMode
      -- Annotation Attribute updates
    | SelectFill Fill
    | SelectStrokeColor Color
    | SelectLineStroke LineStroke
    | SelectStrokeStyle StrokeStyle
    | SelectFontSize Float
      -- Control UI updates
    | ToggleDropdown EditOption
    | CloseDropdown
      -- Selection Updates
    | HoverOverAnnotation
    | LeaveAnnotation
    | ShowResizeIcon
    | ResetToReadyToDraw
    | SelectAnnotation Int Annotation StartPosition
      -- Move updates
    | StartMovingAnnotation Int Annotation StartPosition
    | MoveAnnotation Int Annotation StartPosition EndPosition
      -- Resize updates
    | ResizeDrawing Image Mouse.Position
    | FinishMovingAnnotation Int Annotation StartPosition EndPosition
    | StartResizingAnnotation Int Annotation Vertex StartPosition
    | ResizeAnnotation Int Annotation Vertex StartPosition EndPosition
    | FinishResizingAnnotation Int Annotation Vertex StartPosition EndPosition
      -- History updates
    | Undo
    | Redo
    | Export
      -- Image Selection updates
    | SelectImage Image
    | SetImages (List Image)
      -- Keyboard updates
    | KeyboardMsg Keyboard.Msg


update : Msg -> Model -> ( Model, List (Cmd Msg) )
update msg ({ edits, fill, fontSize, stroke, strokeColor, strokeStyle, mouse, images, keyboardState, editMode } as model) =
    let
        editState =
            edits.present

        ( width, height ) =
            case images of
                Just imageZipper ->
                    (List.Zipper.current imageZipper).width => (List.Zipper.current imageZipper).height

                Nothing ->
                    ( 0, 0 )
    in
        case msg of
            StartRect pos ->
                editState
                    |> startAnnotation pos editMode images model
                    => []

            AddRect start end ->
                editState
                    |> addAnnotation ( (Rect_ <| Rect start end fill strokeColor stroke strokeStyle False), NotSelected ) model
                    => []

            StartRoundedRect pos ->
                editState
                    |> startAnnotation pos editMode images model
                    => []

            AddRoundedRect start end ->
                editState
                    |> addAnnotation ( (Rect_ <| Rect start end fill strokeColor stroke strokeStyle True), NotSelected ) model
                    => []

            StartArrow pos ->
                editState
                    |> startAnnotation pos editMode images model
                    => []

            AddArrow startPos endPos ->
                editState
                    |> addAnnotation ( (Arrow_ <| Line startPos endPos strokeColor stroke strokeStyle), NotSelected ) model
                    => []

            StartEllipse pos ->
                editState
                    |> startAnnotation pos editMode images model
                    => []

            AddEllipse startPos endPos ->
                editState
                    |> addAnnotation ( (Ellipse_ <| Ellipse startPos endPos fill strokeColor stroke strokeStyle), NotSelected ) model
                    => []

            StartTextBox pos ->
                editState
                    |> startAnnotation pos editMode images model
                    => []

            AddTextBox start end ->
                let
                    numAnnotations =
                        Array.length editState.annotations

                    initialTextBox =
                        TextBox_ <| TextBox start end "Text" strokeColor stroke fontSize 0 (AutoExpand.initState (config numAnnotations fontSize Selected))
                in
                    editState
                        |> addAnnotation ( initialTextBox, Selected ) model
                        => [ "text-box-edit--"
                                ++ toString numAnnotations
                                |> Dom.focus
                                |> Task.attempt (tryToEdit numAnnotations)
                           ]

            StartEditingText index textBox ->
                { editState | annotations = Array.set index ( (TextBox_ textBox), Selected ) editState.annotations }
                    |> logChange model
                    => [ "text-box-edit--"
                            ++ toString index
                            |> Dom.focus
                            |> Task.attempt (tryToEdit index)
                       ]

            SwitchToEditingText index ->
                editState
                    |> startEditingText index model
                    => []

            SetText index textBox newText ->
                { editState | annotations = Array.set index ( TextBox_ { textBox | text = newText }, Selected ) editState.annotations }
                    |> skipChange model
                    => []

            FinishEditingText index ->
                editState
                    |> finishEditingText index model
                    => [ "text-box-edit--"
                            ++ toString index
                            |> Dom.blur
                            |> Task.attempt tryToBlur
                       ]

            AutoExpandInput index { state, textValue } ->
                { editState
                    | annotations =
                        editState.annotations
                            |> Array.get index
                            |> Maybe.map (\ann -> Array.set index (autoExpandAnnotation state textValue ann) editState.annotations)
                            |> Maybe.withDefault editState.annotations
                }
                    |> skipChange model
                    => []

            StartLine pos ->
                editState
                    |> startAnnotation pos editMode images model
                    => []

            AddLine startPos endPos ->
                editState
                    |> addAnnotation ( (Line_ <| Line startPos endPos strokeColor stroke strokeStyle), NotSelected ) model
                    => []

            StartSpotlightRect pos ->
                editState
                    |> startAnnotation pos editMode images model
                    => []

            AddSpotlightRect startPos endPos ->
                editState
                    |> addAnnotation ( (Rect_ <| Rect startPos endPos SpotlightFill strokeColor stroke strokeStyle True), NotSelected ) model
                    => []

            ResizeDrawing { width, height } pos ->
                editState
                    |> skipChange model
                    |> setMouse pos
                    => []

            SetImages images ->
                { model | images = List.Zipper.fromList images }
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
                        { model | images = List.Zipper.find ((==) image.url << .url) images, imageSelected = True }
                            => []

                    Nothing ->
                        model => []

            ChangeEditMode editMode ->
                { model | editMode = editMode }
                    |> closeDropdown
                    => []

            SelectFill fill ->
                editState
                    |> updateAnySelectedAnnotations (updateFill fill)
                    |> logChange model
                    |> setFill fill
                    |> closeDropdown
                    => []

            SelectStrokeColor strokeColor ->
                editState
                    |> updateAnySelectedAnnotations (updateStrokeColor strokeColor)
                    |> logChange model
                    |> setStrokeColor strokeColor
                    |> closeDropdown
                    => []

            SelectLineStroke lineStroke ->
                editState
                    |> updateAnySelectedAnnotations (updateLineStroke lineStroke)
                    |> logChange model
                    |> setLineStroke lineStroke
                    |> closeDropdown
                    => []

            SelectStrokeStyle strokeStyle ->
                editState
                    |> updateAnySelectedAnnotations (updateStrokeStyle strokeStyle)
                    |> logChange model
                    |> setStrokeStyle strokeStyle
                    |> closeDropdown
                    => []

            SelectFontSize fontSize ->
                editState
                    |> updateSelectedAnnotations (updateFontSize fontSize)
                    |> logChange model
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

            HoverOverAnnotation ->
                { model
                    | movementState =
                        case model.movementState of
                            ReadyToDraw ->
                                HoveringOverAnnotation

                            OutsideSelectedAnnotation ->
                                HoveringOverSelectedAnnotation

                            _ ->
                                model.movementState
                }
                    => []

            LeaveAnnotation ->
                { model
                    | movementState =
                        case model.movementState of
                            HoveringOverAnnotation ->
                                ReadyToDraw

                            HoveringOverSelectedAnnotation ->
                                OutsideSelectedAnnotation

                            HoveringOverVertex ->
                                OutsideSelectedAnnotation

                            _ ->
                                model.movementState
                }
                    => []

            ShowResizeIcon ->
                { model | movementState = HoveringOverVertex }
                    => []

            SelectAnnotation index annotation startPos ->
                editState
                    |> showVertices index annotation
                    |> skipChange model
                    |> startMovingAnnotation index annotation startPos
                    => []

            ResetToReadyToDraw ->
                model
                    |> resetToReadyToDraw
                    => []

            StartMovingAnnotation index annotation startPos ->
                editState
                    |> removeAllVertices
                    |> showVertices index annotation
                    |> logChange model
                    |> startMovingAnnotation index annotation startPos
                    => []

            MoveAnnotation index annotation oldPos newPos ->
                editState
                    |> moveAnnotation index annotation oldPos newPos
                    |> skipChange model
                    => []

            FinishMovingAnnotation index annotation startPos endPos ->
                editState
                    |> moveAnnotation index annotation startPos endPos
                    |> skipChange model
                    |> hoverOverSelectedAnnotation
                    => []

            StartResizingAnnotation index annotation vertex startPos ->
                editState
                    |> showVertices index annotation
                    |> logChange model
                    |> startResizingAnnotation index annotation vertex startPos
                    => []

            ResizeAnnotation index annotation vertex startPos endPos ->
                editState
                    |> resizeAnnotation index annotation vertex startPos endPos
                    |> skipChange model
                    => []

            FinishResizingAnnotation index annotation vertex startPos endPos ->
                editState
                    |> resizeAnnotation index annotation vertex startPos endPos
                    |> showVertices index (resize startPos endPos vertex annotation)
                    |> skipChange model
                    |> hoverOverSelectedAnnotation
                    => []

            Undo ->
                { model | edits = UndoList.undo model.edits }
                    => []

            Redo ->
                { model | edits = UndoList.redo model.edits }
                    => []

            Export ->
                model
                    => [ case model.images of
                            Just images ->
                                exportToImage <| List.Zipper.current images

                            Nothing ->
                                Cmd.none
                       ]


{-| Add this editState change to app history
-}
logChange : Model -> EditState -> Model
logChange model editState =
    { model | edits = UndoList.new editState model.edits }


{-| Do not add this editState change to app history
-}
skipChange : Model -> EditState -> Model
skipChange model editState =
    { model | edits = UndoList.mapPresent (always editState) model.edits }


startAnnotation : Position -> EditMode -> Maybe (Zipper Image) -> Model -> EditState -> Model
startAnnotation startPos editMode images model editState =
    { editState | drawing = Just <| drawingFromEditMode startPos editMode model.keyboardState }
        |> removeAllVertices
        |> logChange model
        |> startDrawing
        |> updateMouse images startPos


startDrawing : Model -> Model
startDrawing model =
    { model | movementState = DrawingAnnotation }


changeDrawing : Drawing -> EditState -> EditState
changeDrawing drawing editState =
    { editState | drawing = Just drawing }


autoExpandAnnotation : AutoExpand.State -> String -> ( Annotation, SelectState ) -> ( Annotation, SelectState )
autoExpandAnnotation state textValue ( annotation, selection ) =
    case annotation of
        TextBox_ textBox ->
            ( TextBox_ { textBox | autoexpand = state, text = textValue }, Selected )

        _ ->
            ( annotation, selection )


roundedRectDrawing : Bool -> StartPosition -> RoundedRectMode
roundedRectDrawing shiftPressed startPos =
    if shiftPressed then
        DrawingRoundedSquare startPos
    else
        DrawingRoundedRect startPos


drawingFromEditMode : Position -> EditMode -> Keyboard.State -> Drawing
drawingFromEditMode startPos editMode keyboardState =
    let
        shiftPressed =
            isPressed Shift keyboardState
    in
        case editMode of
            EditRect ->
                DrawRect <|
                    if shiftPressed then
                        DrawingSquare startPos
                    else
                        DrawingRect startPos

            EditRoundedRect ->
                DrawRoundedRect <| roundedRectDrawing shiftPressed startPos

            EditEllipse ->
                DrawEllipse <|
                    if shiftPressed then
                        DrawingCircle startPos
                    else
                        DrawingOval startPos

            EditArrow ->
                DrawArrow <|
                    if shiftPressed then
                        DrawingDiscreteArrow startPos
                    else
                        DrawingArrow startPos

            EditLine ->
                DrawLine <|
                    if shiftPressed then
                        DrawingDiscreteLine startPos
                    else
                        DrawingLine startPos

            EditTextBox ->
                DrawTextBox startPos

            EditSpotlightRect ->
                DrawSpotlightRect <| roundedRectDrawing shiftPressed startPos


addAnnotation : ( Annotation, SelectState ) -> Model -> EditState -> Model
addAnnotation annotation model editState =
    { editState
        | annotations = Array.push annotation editState.annotations
        , drawing = Nothing
    }
        |> skipChange model
        |> hoverOverAnnotation


startEditingTextBox : Int -> EditState -> EditState
startEditingTextBox index editState =
    { editState
        | annotations =
            case Array.get index editState.annotations of
                Just ( textbox, _ ) ->
                    Array.set index ( textbox, Selected ) editState.annotations

                Nothing ->
                    editState.annotations
    }


startEditingText : Int -> Model -> EditState -> Model
startEditingText index model editState =
    { model
        | edits = UndoList.new (startEditingTextBox index editState) model.edits
        , movementState = EditingATextBox index
    }


updateSelectedAnnotations : (Annotation -> Annotation) -> EditState -> EditState
updateSelectedAnnotations fn editState =
    { editState | annotations = Array.map (updateSelectedAnnotation fn) editState.annotations }


updateAnySelectedAnnotations : (Annotation -> Annotation) -> EditState -> EditState
updateAnySelectedAnnotations fn editState =
    { editState | annotations = Array.map (updateAnySelectedAnnotation fn) editState.annotations }


updateAnySelectedAnnotation : (Annotation -> Annotation) -> ( Annotation, SelectState ) -> ( Annotation, SelectState )
updateAnySelectedAnnotation fn ( annotation, selectState ) =
    if isSelected selectState then
        ( fn annotation, selectState )
    else
        ( annotation, selectState )


updateSelectedAnnotation : (Annotation -> Annotation) -> ( Annotation, SelectState ) -> ( Annotation, SelectState )
updateSelectedAnnotation updateAnno ( annotation, selectState ) =
    case selectState of
        Selected ->
            ( updateAnno annotation, selectState )

        _ ->
            ( annotation, selectState )


updateStrokeColor : Color -> Annotation -> Annotation
updateStrokeColor strokeColor annotation =
    case annotation of
        Arrow_ line ->
            Arrow_ { line | fill = strokeColor }

        Rect_ rect ->
            Rect_ { rect | strokeColor = strokeColor }

        Ellipse_ ellipse ->
            Ellipse_ { ellipse | strokeColor = strokeColor }

        TextBox_ textBox ->
            TextBox_ { textBox | fill = strokeColor }

        Line_ line ->
            Line_ { line | fill = strokeColor }


updateFill : Fill -> Annotation -> Annotation
updateFill fill annotation =
    case annotation of
        Arrow_ _ ->
            annotation

        Rect_ rect ->
            Rect_ { rect | fill = fill }

        Ellipse_ ellipse ->
            Ellipse_ { ellipse | fill = fill }

        TextBox_ textBox ->
            annotation

        Line_ _ ->
            annotation


updateLineStroke : LineStroke -> Annotation -> Annotation
updateLineStroke lineStroke annotation =
    case annotation of
        Arrow_ line ->
            Arrow_ { line | stroke = lineStroke }

        Rect_ rect ->
            Rect_ { rect | stroke = lineStroke }

        Ellipse_ ellipse ->
            Ellipse_ { ellipse | stroke = lineStroke }

        TextBox_ textBox ->
            annotation

        Line_ line ->
            Line_ { line | stroke = lineStroke }


updateStrokeStyle : StrokeStyle -> Annotation -> Annotation
updateStrokeStyle strokeStyle annotation =
    case annotation of
        Arrow_ line ->
            Arrow_ { line | strokeStyle = strokeStyle }

        Rect_ rect ->
            Rect_ { rect | strokeStyle = strokeStyle }

        Ellipse_ ellipse ->
            Ellipse_ { ellipse | strokeStyle = strokeStyle }

        TextBox_ textBox ->
            annotation

        Line_ line ->
            Line_ { line | strokeStyle = strokeStyle }


updateFontSize : Float -> Annotation -> Annotation
updateFontSize fontSize annotation =
    case annotation of
        TextBox_ textBox ->
            TextBox_ { textBox | fontSize = fontSize }

        _ ->
            annotation


verticesAreShown : Model -> Model
verticesAreShown model =
    { model | movementState = HoveringOverSelectedAnnotation }


setFill : Fill -> Model -> Model
setFill fill model =
    { model | fill = fill }


setFontSize : Float -> Model -> Model
setFontSize fontSize model =
    { model | fontSize = fontSize }


setLineStroke : LineStroke -> Model -> Model
setLineStroke lineStroke model =
    { model | stroke = lineStroke }


setStrokeStyle : StrokeStyle -> Model -> Model
setStrokeStyle strokeStyle model =
    { model | strokeStyle = strokeStyle }


setStrokeColor : Color -> Model -> Model
setStrokeColor strokeColor model =
    { model | strokeColor = strokeColor }


showVertices : Int -> Annotation -> EditState -> EditState
showVertices index annotation editState =
    { editState | annotations = Array.set index ( annotation, SelectedWithVertices ) editState.annotations }


removeAllVertices : EditState -> EditState
removeAllVertices editState =
    { editState | annotations = Array.map (Tuple.mapSecond (always NotSelected)) editState.annotations }


removeVertices : ( Annotation, SelectState ) -> ( Annotation, SelectState )
removeVertices ( annotation, selectState ) =
    ( annotation, NotSelected )


startMovingAnnotation : Int -> Annotation -> StartPosition -> Model -> Model
startMovingAnnotation index annotation startPos model =
    { model | movementState = MovingAnnotation index annotation startPos }


moveAnnotation : Int -> Annotation -> StartPosition -> EndPosition -> EditState -> EditState
moveAnnotation index annotation oldPos newPos editState =
    { editState | annotations = Array.set index ( move oldPos newPos annotation, SelectedWithVertices ) editState.annotations }


startResizingAnnotation : Int -> Annotation -> Vertex -> StartPosition -> Model -> Model
startResizingAnnotation index annotation vertex startPos model =
    { model | movementState = ResizingAnnotation index annotation startPos vertex }


resizeAnnotation : Int -> Annotation -> Vertex -> StartPosition -> EndPosition -> EditState -> EditState
resizeAnnotation index annotation vertex oldPos newPos editState =
    { editState | annotations = Array.set index ( resize oldPos newPos vertex annotation, SelectedWithVertices ) editState.annotations }


resizeVertices : Position -> Vertex -> { a | start : Position, end : Position } -> { a | start : Position, end : Position }
resizeVertices pos vertex annotation =
    case vertex of
        Start ->
            { annotation | start = pos }

        End ->
            { annotation | end = pos }

        StartPlusX ->
            { annotation | start = pos, end = Position annotation.start.x annotation.end.y }

        StartPlusY ->
            { annotation | start = pos, end = Position annotation.end.x annotation.start.y }


resize : StartPosition -> EndPosition -> Vertex -> Annotation -> Annotation
resize start end vertex annotation =
    case annotation of
        Arrow_ arrow ->
            Arrow_ <| resizeVertices end vertex arrow

        Rect_ rect ->
            Rect_ <| resizeVertices end vertex rect

        Ellipse_ ellipse ->
            Ellipse_ <| resizeVertices end vertex ellipse

        TextBox_ textBox ->
            TextBox_ <| resizeVertices end vertex textBox

        Line_ line ->
            Line_ <| resizeVertices end vertex line


move : StartPosition -> EndPosition -> Annotation -> Annotation
move oldPos newPos annotation =
    let
        dX =
            newPos.x - oldPos.x

        dY =
            newPos.y - oldPos.y
    in
        case annotation of
            Arrow_ arrow ->
                Arrow_ { arrow | start = shiftPosition dX dY arrow.start, end = shiftPosition dX dY arrow.end }

            Rect_ rect ->
                Rect_ { rect | start = shiftPosition dX dY rect.start, end = shiftPosition dX dY rect.end }

            Ellipse_ ellipse ->
                Ellipse_ { ellipse | start = shiftPosition dX dY ellipse.start, end = shiftPosition dX dY ellipse.end }

            TextBox_ textBox ->
                TextBox_ { textBox | start = shiftPosition dX dY textBox.start, end = shiftPosition dX dY textBox.end }

            Line_ line ->
                Line_ { line | start = shiftPosition dX dY line.start, end = shiftPosition dX dY line.end }


shiftPosition : Int -> Int -> Mouse.Position -> Mouse.Position
shiftPosition dx dy pos =
    { pos | x = pos.x + dx, y = pos.y + dy }


hoverOverAnnotation : Model -> Model
hoverOverAnnotation model =
    { model | movementState = HoveringOverAnnotation }


hoverOverSelectedAnnotation : Model -> Model
hoverOverSelectedAnnotation model =
    { model | movementState = HoveringOverSelectedAnnotation }


closeDropdown : Model -> Model
closeDropdown model =
    { model | currentDropdown = Nothing }


toggleDropdown : EditOption -> Model -> Model
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
        , editMode =
            case editOption of
                Fonts ->
                    EditTextBox

                _ ->
                    model.editMode
    }


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


updateDrawing : EditState -> Drawing -> EditState
updateDrawing editState drawing =
    { editState | drawing = Just drawing }


transitionOnShift : Drawing -> Drawing
transitionOnShift drawing =
    case drawing of
        DrawRect rectMode ->
            DrawRect <|
                case rectMode of
                    DrawingRect start ->
                        DrawingSquare start

                    DrawingSquare start ->
                        DrawingRect start

        DrawRoundedRect roundedRectMode ->
            DrawRoundedRect <| toggleRoundedSquare roundedRectMode

        DrawArrow arrowMode ->
            DrawArrow <|
                case arrowMode of
                    DrawingArrow startPos ->
                        DrawingDiscreteArrow startPos

                    DrawingDiscreteArrow startPos ->
                        DrawingArrow startPos

        DrawEllipse ellipseMode ->
            DrawEllipse <|
                case ellipseMode of
                    DrawingOval startPos ->
                        DrawingCircle startPos

                    DrawingCircle startPos ->
                        DrawingOval startPos

        DrawLine lineMode ->
            DrawLine <|
                case lineMode of
                    DrawingLine startPos ->
                        DrawingDiscreteLine startPos

                    DrawingDiscreteLine startPos ->
                        DrawingLine startPos

        DrawTextBox _ ->
            drawing

        DrawSpotlightRect roundedRectMode ->
            DrawSpotlightRect <| toggleRoundedSquare roundedRectMode


toggleRoundedSquare : RoundedRectMode -> RoundedRectMode
toggleRoundedSquare roundedRectMode =
    case roundedRectMode of
        DrawingRoundedRect start ->
            DrawingRoundedSquare start

        DrawingRoundedSquare start ->
            DrawingRoundedRect start


updateDrawingsOnShift : EditState -> EditState
updateDrawingsOnShift editState =
    case editState.drawing of
        Just drawing ->
            transitionOnShift drawing
                |> updateDrawing editState

        Nothing ->
            editState


cancelDrawing : EditState -> EditState
cancelDrawing editState =
    { editState | drawing = Nothing }


deleteSelectedDrawings : EditState -> EditState
deleteSelectedDrawings editState =
    { editState | annotations = Array.filter (not << isSelected << Tuple.second) editState.annotations }


isSelected : SelectState -> Bool
isSelected selectState =
    case selectState of
        Selected ->
            True

        NotSelected ->
            False

        SelectedWithVertices ->
            True


finishEditingTextHelper : Int -> EditState -> EditState
finishEditingTextHelper index editState =
    { editState
        | annotations =
            case Array.get index editState.annotations of
                Just ( annotation, _ ) ->
                    Array.set index ( annotation, NotSelected ) editState.annotations

                Nothing ->
                    editState.annotations
    }


finishEditingText : Int -> Model -> EditState -> Model
finishEditingText index model editState =
    { model
        | edits = UndoList.new (finishEditingTextHelper index editState) model.edits
        , movementState = ReadyToDraw
    }


resetToReadyToDraw : Model -> Model
resetToReadyToDraw model =
    { model
        | movementState =
            case model.movementState of
                OutsideSelectedAnnotation ->
                    OutsideSelectedAnnotation

                MovingAnnotation _ _ _ ->
                    OutsideSelectedAnnotation

                ResizingAnnotation _ _ _ _ ->
                    OutsideSelectedAnnotation

                _ ->
                    ReadyToDraw
    }


alterDrawingsWithKeyboard : Maybe KeyChange -> Model -> ( Model, List (Cmd Msg) )
alterDrawingsWithKeyboard maybeKeyChange ({ keyboardState } as model) =
    case model.movementState of
        EditingATextBox index ->
            alterTextBoxDrawing maybeKeyChange index model

        _ ->
            alterDrawing maybeKeyChange model
                => []


alterTextBoxDrawing maybeKeyChange index model =
    case maybeKeyChange of
        Just keyChange ->
            case keyChange of
                KeyDown key ->
                    case key of
                        Escape ->
                            model.edits.present
                                |> finishEditingText index model
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
                                { model | edits = UndoList.mapPresent updateDrawingsOnShift model.edits }

                            Escape ->
                                { model | edits = UndoList.mapPresent cancelDrawing model.edits }

                            Delete ->
                                { model
                                    | edits = UndoList.new (deleteSelectedDrawings model.edits.present) model.edits
                                    , movementState = ReadyToDraw
                                }

                            BackSpace ->
                                { model
                                    | edits = UndoList.new (deleteSelectedDrawings model.edits.present) model.edits
                                    , movementState = ReadyToDraw
                                }

                            CharZ ->
                                if isPressed Shift keyboardState && isPressed controlKey keyboardState then
                                    { model | edits = UndoList.redo model.edits }
                                else if isPressed controlKey keyboardState then
                                    { model | edits = UndoList.undo model.edits }
                                else
                                    model

                            Control ->
                                if model.operatingSystem == MacOS then
                                    model
                                else if isPressed Shift keyboardState && isPressed CharZ keyboardState then
                                    { model | edits = UndoList.redo model.edits }
                                else if isPressed CharZ keyboardState then
                                    { model | edits = UndoList.undo model.edits }
                                else
                                    model

                            Super ->
                                if model.operatingSystem == Windows then
                                    model
                                else if isPressed Shift keyboardState && isPressed CharZ keyboardState then
                                    { model | edits = UndoList.redo model.edits }
                                else if isPressed CharZ keyboardState then
                                    { model | edits = UndoList.undo model.edits }
                                else
                                    model

                            _ ->
                                model

                    KeyUp key ->
                        case key of
                            Shift ->
                                { model | edits = UndoList.mapPresent updateDrawingsOnShift model.edits }

                            _ ->
                                model

            Nothing ->
                model



-- VIEW


view : Model -> Html Msg
view model =
    case model.images of
        Nothing ->
            viewInfoScreen

        Just images ->
            if model.imageSelected then
                viewImageAnnotator model <| List.Zipper.current images
            else
                viewImageSelector model images


viewImageSelector : Model -> Zipper Image -> Html Msg
viewImageSelector model images =
    images
        |> List.Zipper.toList
        |> List.map (viewImageOption images (List.Zipper.current images))
        |> div [ Html.class "image-selector" ]


viewImageOption : Zipper Image -> Image -> Image -> Html Msg
viewImageOption zipper highlightedImage image =
    div
        [ Html.class "image-option"
        , Html.width <| round image.width
        , Html.height <| round image.height
        , onClick <| SelectImage image
        ]
        [ Html.img [ src image.url, Html.height <| round image.height, Html.width <| round image.width ] []
        ]


viewInfoScreen : Html Msg
viewInfoScreen =
    div []
        [ Html.text "please upload an image!" ]


viewImageAnnotator : Model -> Image -> Html Msg
viewImageAnnotator ({ edits, fill, strokeColor, mouse, keyboardState, currentDropdown, editMode } as model) selectedImage =
    let
        toDropdownMenu =
            viewDropdownMenu currentDropdown editMode model
    in
        div
            [ Html.class "annotation-app" ]
            [ div [ Html.class "controls" ]
                [ button [ onClick Export, Html.class "export-button" ] [ Html.text "Save" ]
                , viewHistoryControls edits
                , div [ Html.class "columns" ]
                    (List.map (viewDrawingButton editMode toDropdownMenu) drawingOptions
                        ++ [ viewFillDropdown toDropdownMenu fill
                           , viewStrokeColorDropdown toDropdownMenu strokeColor
                           , viewLineStrokeDropdown toDropdownMenu
                           ]
                    )
                ]
            , viewCanvas model selectedImage
            ]


viewDrawingButton : EditMode -> (EditOption -> Html Msg) -> EditMode -> Html Msg
viewDrawingButton selectedEditMode toDropdownMenu editMode =
    case editMode of
        EditTextBox ->
            viewTextSizeDropdown selectedEditMode toDropdownMenu

        _ ->
            button
                [ Html.classList [ "drawing-button" => True, "drawing-button--selected" => selectedEditMode == editMode ]
                , onClick <| ChangeEditMode editMode
                ]
                [ viewShapeSvg editMode ]


viewHistoryControls : UndoList EditState -> Html Msg
viewHistoryControls edits =
    div [ Html.class "history-controls" ]
        [ button [ onClick Undo, Html.class "history-button", disabled <| not <| UndoList.hasPast edits ] [ viewUndoArrow ]
        , button [ onClick Redo, Html.class "history-button flip", disabled <| not <| UndoList.hasFuture edits ] [ viewUndoArrow ]
        ]


viewTextSizeDropdown : EditMode -> (EditOption -> Html Msg) -> Html Msg
viewTextSizeDropdown editMode toDropdownMenu =
    div [ Html.class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown Fonts
            , Html.classList [ "dropdown-button" => True, "dropdown-button--selected" => editMode == EditTextBox ]
            ]
            [ viewTextIcon ]
        , toDropdownMenu Fonts
        ]


viewFontSizeOptions : Float -> Html Msg
viewFontSizeOptions fontSize =
    fontSizes
        |> List.map (viewFontSizeOption fontSize)
        |> div [ Html.class "dropdown-options" ]


viewFillOptions : Fill -> Html Msg
viewFillOptions fill =
    fillOptions
        |> List.map (viewFillOption fill)
        |> div [ Html.class "dropdown-options" ]


viewStrokeColorOptions : Color -> Html Msg
viewStrokeColorOptions strokeColor =
    strokeColorOptions
        |> List.map (viewStrokeColorOption strokeColor)
        |> div [ Html.class "dropdown-options" ]


viewFillOption : Fill -> Fill -> Html Msg
viewFillOption selectedFill fill =
    button
        [ Html.classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedFill == fill
            ]
        , onClick (SelectFill fill)
        ]
        [ viewFillIcon fill ]


viewStrokeColorOption : Color -> Color -> Html Msg
viewStrokeColorOption selectedColor color =
    button
        [ Html.classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedColor == color
            ]
        , onClick (SelectStrokeColor color)
        ]
        [ viewStrokeColorIcon color ]


viewFontSizeOption : Float -> Float -> Html Msg
viewFontSizeOption selectedFontSize fontSize =
    button
        [ Html.classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedFontSize == fontSize
            ]
        , onClick (SelectFontSize fontSize)
        ]
        [ Html.text <| toString <| fontSize ]


viewLineStrokeDropdown : (EditOption -> Html Msg) -> Html Msg
viewLineStrokeDropdown toDropdownMenu =
    div
        [ Html.class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown Strokes
            , Html.class "dropdown-button"
            ]
            [ viewLineStrokeDropdownIcon Color.grey
            ]
        , toDropdownMenu Strokes
        ]


viewFillDropdown : (EditOption -> Html Msg) -> Fill -> Html Msg
viewFillDropdown toDropdownMenu fill =
    div
        [ Html.class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown Fills
            , Html.class "dropdown-button"
            ]
            [ viewFillIcon fill
            ]
        , toDropdownMenu Fills
        ]


viewStrokeColorDropdown : (EditOption -> Html Msg) -> Color -> Html Msg
viewStrokeColorDropdown toDropdownMenu strokeColor =
    div
        [ Html.class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown StrokeColors
            , Html.class "dropdown-button"
            ]
            [ viewStrokeColorIcon strokeColor
            ]
        , toDropdownMenu StrokeColors
        ]


viewDropdownMenu : Maybe EditOption -> EditMode -> Model -> EditOption -> Html Msg
viewDropdownMenu maybeDropdown curEditMode model selectedOption =
    Maybe.map (viewDropdownOptions curEditMode model selectedOption) maybeDropdown
        |> Maybe.withDefault (Html.text "")


viewDropdownOptions : EditMode -> Model -> EditOption -> EditOption -> Html Msg
viewDropdownOptions curEditMode model selectedOption editOption =
    if selectedOption /= editOption then
        Html.text ""
    else
        case editOption of
            Fonts ->
                viewFontSizeOptions model.fontSize

            Fills ->
                viewFillOptions model.fill

            StrokeColors ->
                viewStrokeColorOptions model.strokeColor

            Strokes ->
                viewLineStrokeOptions model.stroke model.strokeStyle


viewShapeSvg : EditMode -> Html Msg
viewShapeSvg editMode =
    case editMode of
        EditRect ->
            viewRectangleIcon

        EditRoundedRect ->
            viewRoundedRectangleIcon

        EditEllipse ->
            viewEllipseIcon

        EditArrow ->
            viewArrowIcon

        EditLine ->
            viewLineIcon Color.grey 2

        EditTextBox ->
            viewTextIcon

        EditSpotlightRect ->
            viewRoundedRectangleIcon


viewLineStrokeOptions : LineStroke -> StrokeStyle -> Html Msg
viewLineStrokeOptions strokeWidth strokeStyle =
    [ List.map (viewLineStrokeOption strokeWidth) lineStrokeOptions
    , List.map (viewStrokeStyleOption strokeStyle) strokeStyles
    ]
        |> List.concat
        |> div [ Html.class "dropdown-options" ]


viewStrokeStyleOption : StrokeStyle -> StrokeStyle -> Html Msg
viewStrokeStyleOption selectedStrokeStyle strokeStyle =
    button
        [ Html.classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedStrokeStyle == strokeStyle
            ]
        , onClick (SelectStrokeStyle strokeStyle)
        ]
        [ case strokeStyle of
            Solid ->
                viewSolidIcon

            Dotted ->
                viewDottedIcon

            Dashed ->
                viewDashedIcon
        ]


viewLineStrokeOption : LineStroke -> LineStroke -> Html Msg
viewLineStrokeOption selectedStroke stroke =
    button
        [ Html.classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedStroke == stroke
            ]
        , onClick <| SelectLineStroke stroke
        ]
        [ viewLineStroke (strokeToWidth stroke) [] ]


drawingStateEvents : EditMode -> Maybe Drawing -> Position -> List (Html.Attribute Msg)
drawingStateEvents editMode maybeDrawing mouse =
    case maybeDrawing of
        Just drawing ->
            drawingEvents drawing mouse

        Nothing ->
            case editMode of
                EditArrow ->
                    [ onMouseDown <| Json.map (StartArrow << toDrawingPosition) Mouse.position ]

                EditLine ->
                    [ onMouseDown <| Json.map (StartLine << toDrawingPosition) Mouse.position ]

                EditRect ->
                    [ onMouseDown <| Json.map (StartRect << toDrawingPosition) Mouse.position ]

                EditRoundedRect ->
                    [ onMouseDown <| Json.map (StartRoundedRect << toDrawingPosition) Mouse.position ]

                EditEllipse ->
                    [ onMouseDown <| Json.map (StartEllipse << toDrawingPosition) Mouse.position ]

                EditTextBox ->
                    [ onMouseDown <| Json.map (StartTextBox << toDrawingPosition) Mouse.position ]

                EditSpotlightRect ->
                    [ onMouseDown <| Json.map (StartSpotlightRect << toDrawingPosition) Mouse.position ]


drawingEvents : Drawing -> Mouse.Position -> List (Html.Attribute Msg)
drawingEvents drawing curMouse =
    case drawing of
        DrawRect rectMode ->
            case rectMode of
                DrawingRect startPos ->
                    onMouseUpOrLeave <| Json.map (AddRect startPos << toDrawingPosition) Mouse.position

                DrawingSquare startPos ->
                    onMouseUpOrLeave <| Json.map (AddRect startPos << equalXandY startPos << toDrawingPosition) Mouse.position

        DrawRoundedRect rectMode ->
            case rectMode of
                DrawingRoundedRect startPos ->
                    onMouseUpOrLeave <| Json.map (AddRoundedRect startPos << toDrawingPosition) Mouse.position

                DrawingRoundedSquare startPos ->
                    onMouseUpOrLeave <| Json.map (AddRoundedRect startPos << equalXandY startPos << toDrawingPosition) Mouse.position

        DrawArrow arrowMode ->
            case arrowMode of
                DrawingArrow startPos ->
                    onMouseUpOrLeave <| Json.map (AddArrow startPos << toDrawingPosition) Mouse.position

                DrawingDiscreteArrow startPos ->
                    onMouseUpOrLeave <| Json.map (AddArrow startPos << stepMouse startPos << toDrawingPosition) Mouse.position

        DrawEllipse ellipseDrawing ->
            case ellipseDrawing of
                DrawingOval startPos ->
                    onMouseUpOrLeave <| Json.map (AddEllipse startPos << toDrawingPosition) Mouse.position

                DrawingCircle startPos ->
                    onMouseUpOrLeave <| Json.map (AddEllipse startPos << equalXandY startPos << toDrawingPosition) Mouse.position

        DrawTextBox start ->
            onMouseUpOrLeave (Json.map (AddTextBox start << toDrawingPosition) Mouse.position)

        DrawLine lineDrawing ->
            case lineDrawing of
                DrawingLine startPos ->
                    onMouseUpOrLeave (Json.map (AddLine startPos << toDrawingPosition) Mouse.position)

                DrawingDiscreteLine startPos ->
                    onMouseUpOrLeave <| Json.map (AddLine startPos << stepMouse startPos << toDrawingPosition) Mouse.position

        DrawSpotlightRect roundedRectMode ->
            case roundedRectMode of
                DrawingRoundedRect startPos ->
                    onMouseUpOrLeave <| Json.map (AddSpotlightRect startPos << toDrawingPosition) Mouse.position

                DrawingRoundedSquare startPos ->
                    onMouseUpOrLeave <| Json.map (AddSpotlightRect startPos << equalXandY startPos << toDrawingPosition) Mouse.position


viewCanvas : Model -> Image -> Html Msg
viewCanvas model image =
    let
        editState =
            model.edits.present

        attrs =
            [ Html.id "canvas"
            , Html.class "image-edit"
            , Html.style
                [ "width" => toString (round image.width) ++ "px"
                , "height" => toString (round image.height) ++ "px"
                , "cursor" => movementStateToCursor model.movementState
                ]
            ]
                ++ case model.movementState of
                    ReadyToDraw ->
                        drawingStateEvents model.editMode editState.drawing model.mouse

                    DrawingAnnotation ->
                        drawingStateEvents model.editMode editState.drawing model.mouse

                    HoveringOverAnnotation ->
                        [ Html.Events.onMouseLeave ResetToReadyToDraw ]

                    HoveringOverSelectedAnnotation ->
                        [ Html.Events.onMouseLeave ResetToReadyToDraw ]

                    HoveringOverVertex ->
                        [ Html.Events.onMouseLeave ResetToReadyToDraw ]

                    OutsideSelectedAnnotation ->
                        drawingStateEvents model.editMode editState.drawing model.mouse

                    MovingAnnotation index annotation startPos ->
                        [ Html.Events.onMouseLeave ResetToReadyToDraw
                        , SE.on "mouseup" <| Json.map (FinishMovingAnnotation index annotation startPos << toDrawingPosition) Mouse.position
                        ]

                    ResizingAnnotation index annotation startPos vertex ->
                        [ Html.Events.onMouseLeave ResetToReadyToDraw
                        , SE.on "mouseup" <| Json.map (FinishResizingAnnotation index annotation vertex startPos << toDrawingPosition) Mouse.position
                        ]

                    EditingATextBox index ->
                        drawingStateEvents model.editMode editState.drawing model.mouse ++ [ SE.onClick <| FinishEditingText index ]

        toDrawing =
            case editState.drawing of
                Just drawing ->
                    viewDrawing image.width image.height drawing model

                Nothing ->
                    always (Svg.text "")

        annotations =
            editState.annotations
                |> Array.toList
                |> List.indexedMap (viewAnnotation image.width image.height model.movementState)
                |> List.concat

        spotlights =
            editState.annotations
                |> Array.filter (isSpotlightShape << Tuple.first)
                |> Array.map (Tuple.mapFirst spotlightFillToMaskFill)
                |> Array.toList
                |> List.indexedMap (viewAnnotation image.width image.height model.movementState)
                |> List.concat

        cutOuts =
            if isDrawingSpotlight editState.drawing then
                spotlights ++ [ toDrawing True ]
            else
                spotlights

        definitions =
            List.map viewArrowHeadDefinition strokeColorOptions
                |> (::) (viewMask model.movementState image.width image.height cutOuts)
                |> (::) viewSvgFilters
                |> defs []
                |> List.singleton

        mask =
            [ rect [ x "0", y "0", Attr.height <| toString image.height, Attr.width <| toString image.width, Attr.mask "url(#Mask)", Attr.style "pointer-events: none;" ] [] ]

        firstSpotlightIndex =
            editState.annotations
                |> Array.toList
                |> List.Extra.findIndex (isSpotlightShape << Tuple.first)
                |> Maybe.withDefault 0

        svgChildren =
            if List.isEmpty spotlights && not (isDrawingSpotlight editState.drawing) then
                annotations ++ definitions ++ [ toDrawing False ]
            else if isDrawingSpotlight editState.drawing then
                definitions ++ (List.take firstSpotlightIndex annotations) ++ mask ++ (List.drop firstSpotlightIndex annotations) ++ [ toDrawing False ]
            else
                definitions ++ (List.take firstSpotlightIndex annotations) ++ mask ++ (List.drop firstSpotlightIndex annotations) ++ [ toDrawing False ]

        svgs =
            svg
                [ Attr.id "drawing"
                , Attr.class "drawing"
                , Attr.width <| toString <| image.width
                , Attr.height <| toString <| image.height
                , Html.attribute "xmlns" "http://www.w3.org/2000/svg"
                ]
                svgChildren
    in
        div attrs [ svgs, viewImage model.movementState image ]


viewSvgFilters : Svg Msg
viewSvgFilters =
    Svg.filter [ Attr.id "dropShadow" ]
        [ Svg.feGaussianBlur [ Attr.in_ "SourceAlpha", Attr.stdDeviation "3" ] []
        , Svg.feOffset [ Attr.dx "2", Attr.dy "4" ] []
        , Svg.feMerge []
            [ Svg.feMergeNode [] []
            , Svg.feMergeNode [ Attr.in_ "SourceGraphic" ] []
            ]
        ]


viewArrowHeadDefinition : Color -> Svg Msg
viewArrowHeadDefinition color =
    marker [ Attr.id <| "arrow-head--" ++ Color.Convert.colorToHex color, orient "auto", markerWidth "2", markerHeight "4", refX "0.1", refY "2" ]
        [ Svg.path [ d "M0,0 V4 L2,2 Z", Attr.fill <| Color.Convert.colorToHex color ] []
        ]


isDrawingSpotlight : Maybe Drawing -> Bool
isDrawingSpotlight maybeDrawing =
    case maybeDrawing of
        Just drawing ->
            case drawing of
                DrawSpotlightRect roundedRectMode ->
                    True

                _ ->
                    False

        Nothing ->
            False


isSpotlightShape : Annotation -> Bool
isSpotlightShape annotation =
    case annotation of
        Rect_ rect ->
            case rect.fill of
                SpotlightFill ->
                    True

                _ ->
                    False

        _ ->
            False


spotlightFillToMaskFill : Annotation -> Annotation
spotlightFillToMaskFill annotation =
    case annotation of
        Rect_ rect ->
            case rect.fill of
                SpotlightFill ->
                    Rect_ { rect | fill = MaskFill }

                _ ->
                    annotation

        _ ->
            annotation


movementStateEvents : Int -> Annotation -> MovementState -> List (Svg.Attribute Msg)
movementStateEvents index annotation movementState =
    case movementState of
        MovingAnnotation index annotation startPos ->
            [ SE.on "mouseup" <| Json.map (FinishMovingAnnotation index annotation startPos << toDrawingPosition) Mouse.position
            ]

        ReadyToDraw ->
            [ SE.onMouseOver HoverOverAnnotation ]

        DrawingAnnotation ->
            []

        HoveringOverAnnotation ->
            [ SE.onMouseOver HoverOverAnnotation
            , SE.onMouseOut LeaveAnnotation
            , onMouseDown <| Json.map (SelectAnnotation index annotation << toDrawingPosition) Mouse.position
            ]

        OutsideSelectedAnnotation ->
            [ SE.onMouseOver HoverOverAnnotation ]

        HoveringOverSelectedAnnotation ->
            [ SE.on "mousedown" <| Json.map (StartMovingAnnotation index annotation << toDrawingPosition) Mouse.position
            , SE.onMouseOut LeaveAnnotation
            ]

        HoveringOverVertex ->
            []

        ResizingAnnotation _ _ _ _ ->
            []

        EditingATextBox index ->
            [ SE.onMouseOver HoverOverAnnotation
            , SE.onMouseOut LeaveAnnotation
            ]


viewAnnotation : Float -> Float -> MovementState -> Int -> ( Annotation, SelectState ) -> List (Svg Msg)
viewAnnotation width height movementState index ( annotation, selectState ) =
    let
        movementEvents =
            movementStateEvents index annotation movementState

        toVertexEvents =
            movementStateVertexEvents index annotation movementState
    in
        case annotation of
            Arrow_ arrow ->
                viewArrow movementEvents toVertexEvents arrow selectState

            Rect_ rect ->
                viewRect movementEvents toVertexEvents rect selectState

            Ellipse_ ellipse ->
                viewEllipse movementEvents toVertexEvents ellipse selectState

            Line_ line ->
                viewLine movementEvents toVertexEvents line selectState

            TextBox_ textBox ->
                viewTextBox movementEvents toVertexEvents movementState index textBox selectState


movementStateVertexEvents : Int -> Annotation -> MovementState -> Vertex -> List (Svg.Attribute Msg)
movementStateVertexEvents index annotation movementState vertex =
    case movementState of
        HoveringOverSelectedAnnotation ->
            [ SE.onMouseOver ShowResizeIcon
            , SE.onMouseOut LeaveAnnotation
            ]

        HoveringOverVertex ->
            [ SE.on "mousedown" <| Json.map (StartResizingAnnotation index annotation vertex << toDrawingPosition) Mouse.position
            , SE.onMouseOut LeaveAnnotation
            ]

        ResizingAnnotation int annotation startPos vertex ->
            [ SE.on "mouseup" <| Json.map (FinishResizingAnnotation index annotation vertex startPos << toDrawingPosition) Mouse.position
            ]

        OutsideSelectedAnnotation ->
            [ SE.onMouseOver ShowResizeIcon ]

        _ ->
            []


viewMask : MovementState -> Float -> Float -> List (Svg Msg) -> Svg Msg
viewMask movementState width height shapes =
    rect
        ([ x "0"
         , y "0"
         , Attr.width <| toString width
         , Attr.height <| toString height
         , opacity "0.5"
         , fill "white"
         ]
         -- ++ case movementState of
         --     HoveringOverAnnotation ->
         --         [ onMouseEnter LeaveAnnotation ]
         --
         --     _ ->
         --         []
        )
        []
        :: shapes
        |> Svg.mask [ Attr.id "Mask" ]


viewDrawing : Float -> Float -> Drawing -> Model -> Bool -> Svg Msg
viewDrawing width height drawing model isInMask =
    let
        modelAccountingForMask =
            if model.editMode == EditSpotlightRect && isInMask then
                { model | fill = MaskFill, strokeColor = Color.white }
            else if model.editMode == EditSpotlightRect then
                { model | fill = EmptyFill }
            else
                model
    in
        viewDrawingHelper width height drawing modelAccountingForMask


viewDrawingHelper : Float -> Float -> Drawing -> Model -> Svg Msg
viewDrawingHelper width height drawing { fill, strokeColor, stroke, strokeStyle, fontSize, mouse, keyboardState } =
    case drawing of
        DrawRect rectMode ->
            case rectMode of
                DrawingRect startPos ->
                    Rect startPos mouse fill strokeColor stroke strokeStyle False
                        |> viewRectDrawing

                DrawingSquare startPos ->
                    Rect startPos (equalXandY startPos mouse) fill strokeColor stroke strokeStyle False
                        |> viewRectDrawing

        DrawRoundedRect rectMode ->
            case rectMode of
                DrawingRoundedRect startPos ->
                    Rect startPos mouse fill strokeColor stroke strokeStyle True
                        |> viewRectDrawing

                DrawingRoundedSquare startPos ->
                    Rect startPos (equalXandY startPos mouse) fill strokeColor stroke strokeStyle True
                        |> viewRectDrawing

        DrawArrow arrowDrawing ->
            case arrowDrawing of
                DrawingArrow pos ->
                    Line pos mouse strokeColor stroke strokeStyle
                        |> viewArrowDrawing

                DrawingDiscreteArrow pos ->
                    Line pos (stepMouse pos mouse) strokeColor stroke strokeStyle
                        |> viewArrowDrawing

        DrawEllipse ellipseDrawing ->
            case ellipseDrawing of
                DrawingOval pos ->
                    Ellipse pos mouse fill strokeColor stroke strokeStyle
                        |> viewEllipseDrawing

                DrawingCircle pos ->
                    Ellipse pos (equalXandY pos mouse) fill strokeColor stroke strokeStyle
                        |> viewEllipseDrawing

        DrawTextBox startPos ->
            TextBox startPos mouse "" strokeColor stroke fontSize 0 (AutoExpand.initState (config 0 fontSize NotSelected))
                |> viewTextBoxWithBorder

        DrawLine lineMode ->
            case lineMode of
                DrawingLine pos ->
                    Line pos mouse strokeColor stroke strokeStyle
                        |> viewLineDrawing

                DrawingDiscreteLine pos ->
                    Line pos (stepMouse pos mouse) strokeColor stroke strokeStyle
                        |> viewLineDrawing

        DrawSpotlightRect rectMode ->
            case rectMode of
                DrawingRoundedRect startPos ->
                    Rect startPos mouse fill strokeColor stroke strokeStyle True
                        |> viewRectDrawing

                DrawingRoundedSquare startPos ->
                    Rect startPos (equalXandY startPos mouse) fill strokeColor stroke strokeStyle True
                        |> viewRectDrawing


fillStyle : Fill -> List (Svg.Attribute Msg)
fillStyle fill =
    case fill of
        SolidFill color ->
            [ Attr.fill <| Color.Convert.colorToHex color ]

        SpotlightFill ->
            [ Attr.fill "white", fillOpacity "0" ]

        MaskFill ->
            [ Attr.fill "black" ]

        EmptyFill ->
            [ Attr.fill "white", fillOpacity "0" ]


pointerEvents : Fill -> String
pointerEvents fill =
    case fill of
        EmptyFill ->
            "pointer-events: visibleStroke;"

        MaskFill ->
            "pointer-events: none;"

        SolidFill _ ->
            "pointer-events: auto;"

        SpotlightFill ->
            "pointer-events: visibleStroke;"


viewRect : List (Svg.Attribute Msg) -> (Vertex -> List (Svg.Attribute Msg)) -> Rect -> SelectState -> List (Svg Msg)
viewRect attrs toVertexEvents rect selectState =
    [ Svg.rect
        (rectAttributes rect ++ attrs)
        []
    ]
        ++ if selectState == SelectedWithVertices then
            rectVertices toVertexEvents rect.start rect.end
           else
            []


viewRectDrawing : Rect -> Svg Msg
viewRectDrawing rect =
    Svg.rect (rectAttributes rect) []


rectAttributes : Rect -> List (Svg.Attribute Msg)
rectAttributes { start, end, fill, strokeColor, stroke, strokeStyle, rounded } =
    let
        strokeStyles =
            [ toLineStyle strokeStyle ]
                ++ if rounded then
                    [ rx "15", ry "15" ]
                   else
                    []
    in
        [ Attr.width <| toString <| abs <| end.x - start.x
        , Attr.height <| toString <| abs <| end.y - start.y
        , x <| toString <| Basics.min start.x end.x
        , y <| toString <| Basics.min start.y end.y
        , strokeWidth <| toString <| strokeToWidth stroke
        , Attr.stroke <| Color.Convert.colorToHex strokeColor
        , Attr.style <| pointerEvents fill
          -- , Attr.filter "url(#dropShadow)"
        ]
            ++ strokeStyles
            ++ fillStyle fill


rectVertices : (Vertex -> List (Svg.Attribute Msg)) -> StartPosition -> EndPosition -> List (Svg Msg)
rectVertices toVertexEvents start end =
    [ viewVertex (toVertexEvents Start) start.x start.y
    , viewVertex (toVertexEvents StartPlusX) end.x start.y
    , viewVertex (toVertexEvents StartPlusY) start.x end.y
    , viewVertex (toVertexEvents End) end.x end.y
    ]


viewVertex : List (Svg.Attribute Msg) -> Int -> Int -> Svg Msg
viewVertex vertexEvents x y =
    circle
        ([ cx <| toString x
         , cy <| toString y
         , r "7"
         , fill <| Color.Convert.colorToHex Color.blue
         ]
            ++ vertexEvents
        )
        []


viewArrow : List (Svg.Attribute Msg) -> (Vertex -> List (Svg.Attribute Msg)) -> Line -> SelectState -> List (Svg Msg)
viewArrow attrs toVertexEvents line selectState =
    [ Svg.path
        (arrowAttributes line
            ++ lineAttributes line
            ++ attrs
        )
        []
    ]
        ++ if selectState == SelectedWithVertices then
            arrowVertices toVertexEvents line.start line.end
           else
            []


viewArrowDrawing : Line -> Svg Msg
viewArrowDrawing line =
    Svg.path
        (arrowAttributes line ++ lineAttributes line)
        []


arrowAttributes : Line -> List (Svg.Attribute Msg)
arrowAttributes line =
    [ markerEnd <| "url(#arrow-head--" ++ Color.Convert.colorToHex line.fill ++ ")" ]


arrowVertices : (Vertex -> List (Svg.Attribute Msg)) -> StartPosition -> EndPosition -> List (Svg Msg)
arrowVertices toVertexEvents start end =
    [ viewVertex (toVertexEvents Start) start.x start.y
    , viewVertex (toVertexEvents End) end.x end.y
    ]


ellipseVertices : (Vertex -> List (Svg.Attribute Msg)) -> StartPosition -> EndPosition -> List (Svg Msg)
ellipseVertices toVertexEvents start end =
    let
        dX =
            end.x - start.x

        dY =
            end.y - start.y

        rectStart =
            Position (start.x - dX) (end.y - 2 * dY)
    in
        rectVertices toVertexEvents rectStart end


viewEllipse : List (Svg.Attribute Msg) -> (Vertex -> List (Svg.Attribute Msg)) -> Ellipse -> SelectState -> List (Svg Msg)
viewEllipse attrs toVertexEvents ellipse selectState =
    [ Svg.ellipse (ellipseAttributes ellipse ++ attrs) [] ]
        ++ if selectState == SelectedWithVertices then
            ellipseVertices toVertexEvents ellipse.start ellipse.end
           else
            []


viewEllipseDrawing : Ellipse -> Svg Msg
viewEllipseDrawing ellipse =
    Svg.ellipse (ellipseAttributes ellipse) []


ellipseAttributes : Ellipse -> List (Svg.Attribute Msg)
ellipseAttributes { start, end, fill, strokeColor, stroke, strokeStyle } =
    [ rx <| toString <| abs <| end.x - start.x
    , ry <| toString <| abs <| end.y - start.y
    , cx <| toString <| start.x
    , cy <| toString <| start.y
    , Attr.strokeWidth <| toString <| strokeToWidth stroke
    , Attr.stroke <| Color.Convert.colorToHex strokeColor
    , toLineStyle strokeStyle
    , Attr.style <| pointerEvents fill
      -- , Attr.filter "url(#dropShadow)"
    ]
        ++ fillStyle fill


viewTextArea : Int -> SelectState -> TextBox -> Svg Msg
viewTextArea index selectState { start, end, text, fill, fontSize, angle, autoexpand } =
    foreignObject
        []
        [ div
            [ Html.class "text-box-container"
            , Html.style
                [ "top" => toPx (Basics.min start.y end.y)
                , "left" => toPx (Basics.min start.x end.x)
                , "width" => toPx (abs (end.x - start.x))
                , "font-size" => toPx fontSize
                , "color" => Color.Convert.colorToHex fill
                ]
            , Html.attribute "onclick" "event.stopPropagation();"
            , Html.Events.onMouseOver HoverOverAnnotation
            , Html.Events.onMouseOut LeaveAnnotation
            ]
            [ AutoExpand.view (config index fontSize selectState) autoexpand text
            ]
        ]


viewTextBox : List (Svg.Attribute Msg) -> (Vertex -> List (Svg.Attribute Msg)) -> MovementState -> Int -> TextBox -> SelectState -> List (Svg Msg)
viewTextBox attrs toVertexEvents movementState index ({ start, end, text, fill, fontSize, angle, autoexpand } as textBox) selectState =
    case selectState of
        Selected ->
            (viewTextArea index selectState textBox)
                |> List.singleton
                |> flip List.append (viewRect ([ Attr.style "opacity: 0;" ] ++ attrs) toVertexEvents (Rect start end EmptyFill Color.black Thin Solid False) selectState)

        NotSelected ->
            textBox.text
                |> String.split "\n"
                |> List.map (Svg.tspan [ dy <| toString <| fontSize, x <| toString <| Basics.min start.x end.x ] << List.singleton << Svg.text)
                |> Svg.text_ [ y <| toString <| Basics.min start.y end.y, Attr.style "pointer-events: none; user-select: none;" ]
                |> List.singleton
                |> flip List.append (viewRect ([ Attr.style "stroke: transparent; pointer-events: auto; cursor: pointer;" ] ++ attrs) toVertexEvents (Rect start end EmptyFill Color.black Thin Solid False) selectState)

        SelectedWithVertices ->
            textBox.text
                |> String.split "\n"
                |> List.map (Svg.tspan [ dy <| toString <| fontSize, x <| toString <| Basics.min start.x end.x ] << List.singleton << Svg.text)
                |> Svg.text_ [ y <| toString <| Basics.min start.y end.y, Attr.style "pointer-events: none; user-select: none;" ]
                |> List.singleton
                |> flip List.append
                    (viewRect attrs toVertexEvents (Rect start end EmptyFill Color.black Thin Solid False) NotSelected
                        ++ viewRect (attrs ++ [ SE.onMouseDown <| StartEditingText index textBox, Attr.style "pointer-events: fill; cursor: pointer;" ]) toVertexEvents (Rect start end EmptyFill Color.black Thin Solid False) selectState
                    )


viewTextBoxWithBorder : TextBox -> Svg Msg
viewTextBoxWithBorder ({ start, end, text, fill, fontSize, angle } as textBox) =
    Svg.g []
        [ viewRectDrawing (Rect start end EmptyFill Color.black Thin Solid False)
        ]


viewLine : List (Svg.Attribute Msg) -> (Vertex -> List (Svg.Attribute Msg)) -> Line -> SelectState -> List (Svg Msg)
viewLine attrs toVertexEvents line selectState =
    [ Svg.path (lineAttributes line ++ attrs) [] ]
        ++ if selectState == SelectedWithVertices then
            arrowVertices toVertexEvents line.start line.end
           else
            []


viewLineDrawing : Line -> Svg Msg
viewLineDrawing line =
    Svg.path (lineAttributes line) []


lineAttributes : Line -> List (Svg.Attribute Msg)
lineAttributes { start, end, fill, stroke, strokeStyle } =
    [ strokeWidth <| toString <| strokeToWidth stroke
    , Attr.fill "none"
    , Attr.stroke <| Color.Convert.colorToHex fill
    , d <| "M" ++ toString start.x ++ "," ++ toString start.y ++ " l" ++ toString (end.x - start.x) ++ "," ++ toString (end.y - start.y)
    , toLineStyle strokeStyle
      -- , Attr.filter "url(#dropShadow)"
    ]


viewImage : MovementState -> Image -> Html Msg
viewImage movementState { width, height, url } =
    Html.img
        ([ Html.class "image-to-annotate"
         , Html.width (round width)
         , Html.height (round height)
         , src url
         ]
            ++ case movementState of
                HoveringOverAnnotation ->
                    [ onMouseEnter LeaveAnnotation ]

                _ ->
                    []
        )
        []


viewSolidIcon : Html msg
viewSolidIcon =
    viewLineStroke 4 []


viewDottedIcon : Html msg
viewDottedIcon =
    viewLineStroke 4 [ strokeDasharray "5, 5" ]


viewDashedIcon : Html msg
viewDashedIcon =
    viewLineStroke 4 [ strokeDasharray "10, 5" ]


viewResetArrow : Html msg
viewResetArrow =
    svg [ Attr.width "20", Attr.height "20", viewBox "0 0 14.155 14.155" ]
        [ g [ fill "grey" ]
            [ Svg.path [ d "M12.083,1.887c-0.795-0.794-1.73-1.359-2.727-1.697v2.135c0.48,0.239,0.935,0.55,1.334,0.95c1.993,1.994,1.993,5.236,0,7.229c-1.993,1.99-5.233,1.99-7.229,0c-1.991-1.995-1.991-5.235,0-7.229C3.466,3.269,3.482,3.259,3.489,3.25h0.002l1.181,1.179L4.665,0.685L0.923,0.68l1.176,1.176C2.092,1.868,2.081,1.88,2.072,1.887c-2.763,2.762-2.763,7.243,0,10.005c2.767,2.765,7.245,2.765,10.011,0C14.844,9.13,14.847,4.649,12.083,1.887z" ] [] ]
        ]


viewUndoArrow : Html msg
viewUndoArrow =
    svg [ Attr.width "20", Attr.height "20", viewBox "0 0 26.676 26.676" ]
        [ Svg.path [ d "M26.105,21.891c-0.229,0-0.439-0.131-0.529-0.346l0,0c-0.066-0.156-1.716-3.857-7.885-4.59c-1.285-0.156-2.824-0.236-4.693-0.25v4.613c0,0.213-0.115,0.406-0.304,0.508c-0.188,0.098-0.413,0.084-0.588-0.033L0.254,13.815C0.094,13.708,0,13.528,0,13.339c0-0.191,0.094-0.365,0.254-0.477l11.857-7.979c0.175-0.121,0.398-0.129,0.588-0.029c0.19,0.102,0.303,0.295,0.303,0.502v4.293c2.578,0.336,13.674,2.33,13.674,11.674c0,0.271-0.191,0.508-0.459,0.562C26.18,21.891,26.141,21.891,26.105,21.891z", fill "currentColor" ] []
        ]


viewArrowIcon : Html msg
viewArrowIcon =
    svg [ Attr.width "20", Attr.height "20", viewBox "0 0 347.341 347.341" ]
        [ polygon [ points "347.341,107.783 347.339,0 239.559,0.002 282.843,43.285 0,326.128 21.213,347.341 304.056,64.498", fill "grey" ] []
        ]


viewRectangleIcon : Html msg
viewRectangleIcon =
    svg [ Attr.width "20", Attr.height "20", viewBox "0 0 40 40" ]
        [ rect [ x "10", y "10", Attr.width "20", Attr.height "20", fill "white", stroke "grey" ] []
        ]


viewRoundedRectangleIcon : Html msg
viewRoundedRectangleIcon =
    svg [ Attr.width "20", Attr.height "20", viewBox "0 0 40 40" ]
        [ rect [ x "10", y "10", Attr.width "20", Attr.height "20", rx "2", ry "2", fill "white", stroke "grey" ] []
        ]


viewEllipseIcon : Html msg
viewEllipseIcon =
    svg [ Attr.width "20", Attr.height "20", viewBox "0 0 20 20" ]
        [ ellipse [ cx "10", cy "10", rx "8", ry "5", stroke "grey", strokeWidth "2", fill "white" ] []
        ]


viewFillIcon : Fill -> Html msg
viewFillIcon fill =
    let
        ( fillAttr, showRedLine ) =
            case fill of
                SolidFill color ->
                    if color == Color.white then
                        [ Attr.fill <| Color.Convert.colorToHex Color.white
                        , Attr.stroke <| Color.Convert.colorToHex Color.black
                        , r "9"
                        ]
                            => False
                    else
                        [ Attr.fill <| Color.Convert.colorToHex color ]
                            => False

                SpotlightFill ->
                    [ Attr.fill "white" ]
                        => False

                MaskFill ->
                    [ Attr.fill "white" ]
                        => False

                EmptyFill ->
                    [ Attr.fill "white", fillOpacity "0", stroke <| Color.Convert.colorToHex Color.red, r "9" ]
                        => True
    in
        svg [ Attr.width "20", Attr.height "20", viewBox "0 0 20 20" ]
            ([ circle
                ([ cx "10", cy "10", r "10" ]
                    ++ fillAttr
                )
                []
             ]
                ++ if showRedLine then
                    [ viewLineIcon Color.red 2 ]
                   else
                    []
            )


viewStrokeColorIcon : Color -> Html msg
viewStrokeColorIcon strokeColor =
    svg [ Attr.width "20", Attr.height "20", viewBox "0 0 20 20" ]
        [ circle
            ([ cx "10", cy "10", r "9", stroke <| Color.Convert.colorToHex strokeColor, fill "white" ]
                ++ if strokeColor == Color.white then
                    [ Attr.fill <| Color.Convert.colorToHex Color.black ]
                   else
                    []
            )
            []
        ]


viewLineStrokeDropdownIcon : Color -> Html msg
viewLineStrokeDropdownIcon strokeColor =
    svg [ Attr.width "20", Attr.height "20", viewBox "0 0 20 20" ]
        [ g [ stroke <| Color.Convert.colorToHex strokeColor ]
            [ line [ x1 "0", x2 "20", y1 "10", y2 "10", strokeWidth "6" ] []
            ]
        ]


viewLineIcon : Color -> Int -> Html msg
viewLineIcon strokeColor strokeWidth =
    svg [ Attr.width "20", Attr.height "20", viewBox "0 0 20 20" ]
        [ g [ stroke <| Color.Convert.colorToHex strokeColor ]
            [ line [ x1 "0", x2 "20", y1 "20", y2 "0", Attr.strokeWidth <| toString <| strokeWidth ] []
            ]
        ]


viewLineStroke : number -> List (Svg.Attribute msg) -> Html msg
viewLineStroke strokeWidth attrs =
    svg [ Attr.width "20", Attr.height "20", viewBox "0 0 20 20" ]
        [ g [ stroke "grey" ]
            [ line
                ([ x1 "0"
                 , x2 "20"
                 , y1 "10"
                 , y2 "10"
                 , Attr.strokeWidth <| toString <| strokeWidth
                 ]
                    ++ attrs
                )
                []
            ]
        ]


viewDownArrow : Html msg
viewDownArrow =
    svg [ Attr.height "20", Attr.width "20", viewBox "0 0 48 48" ]
        [ Svg.path [ d "M14 20l10 10 10-10z", fill "grey" ] []
        , Svg.path [ d "M0 0h48v48h-48z", fill "none" ] []
        ]


viewTextIcon : Html msg
viewTextIcon =
    svg [ viewBox "0 0 405 405", Attr.height "20", Attr.width "20" ]
        [ Svg.path [ fill "grey", d "M34.784,0v132.809h43.61v-33.98c0-7.906,3.012-15.808,9.041-21.837c6.037-6.029,13.943-9.041,21.845-9.041h46.323v262.891c0,7.906-3.012,15.804-9.041,21.841c-6.033,6.029-13.943,9.041-21.841,9.041h-32.99v43.61h221.858v-43.61h-32.981c-7.91,0-15.808-3.012-21.833-9.041c-6.042-6.037-9.05-13.939-9.05-21.841V67.951h46.323c7.91,0,15.808,3.012,21.841,9.041c6.025,6.029,9.041,13.935,9.041,21.837v33.98h43.618V0H34.784z" ] []
        ]



-- HELPERS


onMouseDown : Json.Decoder msg -> Html.Attribute msg
onMouseDown decodeToMsg =
    on "mousedown" decodeToMsg


onMouseUp : Json.Decoder msg -> Html.Attribute msg
onMouseUp decodeToMsg =
    on "mouseup" decodeToMsg


onMouseUpOrLeave : Json.Decoder msg -> List (Html.Attribute msg)
onMouseUpOrLeave decodeToMsg =
    [ on "mouseleave" decodeToMsg, onMouseUp decodeToMsg ]


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


toPx : number -> String
toPx number =
    toString number ++ "px"


arrowAngle : StartPosition -> EndPosition -> Float
arrowAngle a b =
    let
        theta =
            atan2 (toFloat (b.y - a.y)) (toFloat (b.x - a.x))

        radians =
            if theta < 0.0 then
                (2 * pi) + theta
            else
                theta
    in
        radians


toDeltas : Float -> Float -> Position
toDeltas h theta =
    Position (round (cos theta * h)) (round (sin theta * h))


calcDistance : Position -> Position -> Float
calcDistance a b =
    sqrt <| toFloat <| (b.x - a.x) ^ 2 + (b.y - a.y) ^ 2


stepMouse : StartPosition -> EndPosition -> EndPosition
stepMouse startPos curPos =
    arrowAngle startPos curPos
        / (pi / 4)
        |> round
        |> toFloat
        |> (*) (pi / 4)
        |> toDeltas (calcDistance startPos curPos)
        |> positionMapX ((+) startPos.x)
        |> positionMapY ((+) startPos.y)


positionMapX : (Int -> Int) -> Position -> Position
positionMapX fn pos =
    { pos | x = fn pos.x }


positionMapY : (Int -> Int) -> Position -> Position
positionMapY fn pos =
    { pos | y = fn pos.y }


equalXandY : StartPosition -> EndPosition -> EndPosition
equalXandY a b =
    if b.y < a.y then
        Position b.x (a.y - abs b.x - a.x)
    else
        Position b.x (a.y + abs b.x - a.x)


strokeToWidth : LineStroke -> Int
strokeToWidth stroke =
    case stroke of
        VeryThin ->
            2

        Thin ->
            4

        Medium ->
            6

        Thick ->
            8

        VeryThick ->
            10


toLineStyle : StrokeStyle -> Svg.Attribute Msg
toLineStyle strokeStyle =
    case strokeStyle of
        Solid ->
            strokeDasharray ""

        Dotted ->
            strokeDasharray "1, 5"

        Dashed ->
            strokeDasharray "10, 5"


toDrawingPosition : Mouse.Position -> Mouse.Position
toDrawingPosition mouse =
    { mouse | x = mouse.x - 65, y = mouse.y }


movementStateToCursor : MovementState -> String
movementStateToCursor movementState =
    case movementState of
        ReadyToDraw ->
            "crosshair"

        DrawingAnnotation ->
            "crosshair"

        HoveringOverAnnotation ->
            "move"

        HoveringOverSelectedAnnotation ->
            "move"

        HoveringOverVertex ->
            "nesw-resize"

        OutsideSelectedAnnotation ->
            "crosshair"

        MovingAnnotation _ _ _ ->
            "move"

        ResizingAnnotation _ _ _ vertex ->
            "nesw-resize"

        EditingATextBox _ ->
            "default"



-- Configuration


config : Int -> Float -> SelectState -> AutoExpand.Config Msg
config index fontSize selectState =
    AutoExpand.config
        { onInput = AutoExpandInput index
        , padding = 2
        , lineHeight = fontSize
        , minRows = 1
        , maxRows = 4
        , attributes =
            [ Html.id <| "text-box-edit--" ++ toString index
            , Html.classList [ "text-box-textarea" => True, "text-box-textarea--unselected" => selectState /= Selected ]
            , Html.style [ "font-size" => toPx fontSize ]
            , Html.readonly <| selectState /= Selected
            ]
        }



-- PORTS


port exportToImage : Image -> Cmd msg


port setImages : (List Image -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.images of
            Nothing ->
                Sub.none

            Just images ->
                if model.edits.present.drawing /= Nothing then
                    Mouse.moves (ResizeDrawing (List.Zipper.current images) << toDrawingPosition)
                else
                    case model.movementState of
                        ResizingAnnotation index annotation startPos vertex ->
                            Mouse.moves (ResizeAnnotation index annotation vertex startPos << toDrawingPosition)

                        MovingAnnotation index annotation startPos ->
                            Mouse.moves (MoveAnnotation index annotation startPos << toDrawingPosition)

                        _ ->
                            Sub.map KeyboardMsg Keyboard.subscriptions
        , setImages SetImages
        ]



-- MAIN


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init >> Rocket.batchInit
        , update = update >> Rocket.batchUpdate
        , view = view
        , subscriptions = subscriptions
        }
