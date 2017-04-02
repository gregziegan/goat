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
import Keyboard.Extra as Keyboard exposing (Key(..), KeyChange, KeyChange(..), isPressed)
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


type ShapeMode
    = DrawingShape
    | DrawingEqualizedShape


type LineMode
    = DrawingLine
    | DrawingDiscreteLine


type Drawing
    = DrawLine LineType LineMode
    | DrawShape ShapeType ShapeMode


type Fill
    = SolidFill Color
    | EmptyFill
    | MaskFill
    | SpotlightFill


type StrokeWidth
    = Thin
    | Medium
    | Thick


type StrokeStyle
    = Solid
    | Dotted
    | Dashed


type alias Line =
    { start : Position
    , end : Position
    , strokeColor : Color
    , strokeWidth : StrokeWidth
    , strokeStyle : StrokeStyle
    }


type alias Shape =
    { start : Position
    , end : Position
    , fill : Fill
    , strokeColor : Color
    , strokeWidth : StrokeWidth
    , strokeStyle : StrokeStyle
    }


type alias TextArea =
    { start : Position
    , end : Position
    , fill : Color
    , fontSize : Float
    , text : String
    , angle : Float
    , autoexpand : AutoExpand.State
    }


type alias Image =
    { url : String
    , width : Float
    , height : Float
    , originalWidth : Float
    , originalHeight : Float
    }


type AttributeDropdown
    = Fonts
    | Fills
    | StrokeColors
    | Strokes


type LineType
    = Arrow
    | StraightLine


type ShapeType
    = Rect
    | RoundedRect
    | Ellipse
    | SpotlightRect
    | TextBorder


type Annotation
    = Lines LineType Line
    | Shapes ShapeType Shape
    | TextBox TextArea


type Vertex
    = Start
    | End
    | StartPlusX
    | StartPlusY


type OperatingSystem
    = MacOS
    | Windows


type AnnotationState
    = ReadyToDraw
    | DrawingAnnotation StartPosition
    | SelectedAnnotation Int Annotation
    | MovingAnnotation Int Annotation StartPosition
    | ResizingAnnotation Int Annotation StartPosition Vertex
    | EditingATextBox Int


type SelectState
    = Selected
    | SelectedWithVertices
    | NotSelected


type alias Model =
    { edits : UndoList (Array Annotation)
    , annotationState :
        AnnotationState
        -- annotation attributes
    , fill : Fill
    , strokeColor : Color
    , strokeWidth : StrokeWidth
    , strokeStyle : StrokeStyle
    , fontSize :
        Float
        -- Important I/O device info
    , mouse : Mouse.Position
    , keyboardState :
        Keyboard.State
        -- Image selection fields
    , images : Maybe (Zipper Image)
    , imageSelected :
        Bool
        -- Control Interface
    , currentDropdown : Maybe AttributeDropdown
    , drawing : Drawing
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


lineStrokeOptions : List StrokeWidth
lineStrokeOptions =
    [ Thin
    , Medium
    , Thick
    ]


strokeStyles : List StrokeStyle
strokeStyles =
    [ Solid
    , Dotted
    , Dashed
    ]


drawingOptions : Bool -> List Drawing
drawingOptions shiftPressed =
    if shiftPressed then
        [ DrawLine Arrow DrawingDiscreteLine
        , DrawLine StraightLine DrawingDiscreteLine
        , DrawShape Rect DrawingEqualizedShape
        , DrawShape RoundedRect DrawingEqualizedShape
        , DrawShape Ellipse DrawingEqualizedShape
        , DrawShape TextBorder DrawingEqualizedShape
        , DrawShape SpotlightRect DrawingEqualizedShape
        ]
    else
        [ DrawLine Arrow DrawingLine
        , DrawLine StraightLine DrawingLine
        , DrawShape Rect DrawingShape
        , DrawShape RoundedRect DrawingShape
        , DrawShape Ellipse DrawingShape
        , DrawShape TextBorder DrawingShape
        , DrawShape SpotlightRect DrawingShape
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


init : Flags -> ( Model, List (Cmd Msg) )
init flags =
    { edits = UndoList.fresh Array.empty
    , fill = EmptyFill
    , strokeColor = Color.red
    , strokeWidth = Medium
    , strokeStyle = Solid
    , fontSize = 14
    , mouse = Mouse.Position 0 0
    , keyboardState = Keyboard.initialState
    , images = List.Zipper.fromList []
    , imageSelected = False
    , currentDropdown = Nothing
    , drawing = DrawLine Arrow DrawingLine
    , annotationState = ReadyToDraw
    , operatingSystem =
        if flags.isMac then
            MacOS
        else
            Windows
    }
        => []



-- UPDATE


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
    | SelectLineStroke StrokeWidth
    | SelectStrokeStyle StrokeStyle
    | SelectFontSize Float
      -- Control UI updates
    | ToggleDropdown AttributeDropdown
    | ChangeDrawing Drawing
    | CloseDropdown
      -- Selection Updates
    | ResetToReadyToDraw
    | SelectAnnotation Int Annotation StartPosition
      -- Move updates
    | StartMovingAnnotation Int Annotation StartPosition
    | MoveAnnotation Int Annotation StartPosition EndPosition
    | FinishMovingAnnotation Int Annotation StartPosition EndPosition
      -- Resize updates
    | StartResizingAnnotation Int Annotation Vertex StartPosition
    | ResizeAnnotation Int Annotation Vertex StartPosition EndPosition
    | FinishResizingAnnotation Int Annotation Vertex StartPosition EndPosition
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


update : Msg -> Model -> ( Model, List (Cmd Msg) )
update msg ({ edits, fill, fontSize, strokeWidth, strokeColor, strokeStyle, mouse, images, keyboardState, drawing } as model) =
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
                let
                    numAnnotations =
                        Array.length model.edits.present

                    initialTextBox =
                        TextBox <| TextArea start end strokeColor fontSize "Text" 0 (AutoExpand.initState (config numAnnotations fontSize))
                in
                    case model.drawing of
                        DrawLine lineType lineMode ->
                            model
                                |> addAnnotation (Lines lineType (Line start (calcLinePos start end lineMode) strokeColor strokeWidth strokeStyle))
                                => []

                        DrawShape shapeType shapeMode ->
                            model
                                |> addAnnotation (Shapes shapeType (Shape start (calcShapePos start end shapeMode) fill strokeColor strokeWidth strokeStyle))
                                => case shapeType of
                                    TextBorder ->
                                        [ "text-box-edit--"
                                            ++ toString numAnnotations
                                            |> Dom.focus
                                            |> Task.attempt (tryToEdit numAnnotations)
                                        ]

                                    _ ->
                                        []

            StartEditingText index textArea ->
                annotations
                    |> Array.set index (TextBox textArea)
                    |> logChange model
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
                    => [ "text-box-edit--"
                            ++ toString index
                            |> Dom.blur
                            |> Task.attempt tryToBlur
                       ]

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

            SelectLineStroke lineStroke ->
                model
                    |> updateAnySelectedAnnotations (updateLineStroke lineStroke)
                    |> setStrokeWidth lineStroke
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

            SelectAnnotation index annotation startPos ->
                model
                    |> selectAnnotation index annotation
                    |> startMovingAnnotation index annotation startPos
                    => []

            ResetToReadyToDraw ->
                model
                    |> resetToReadyToDraw
                    => []

            StartMovingAnnotation index annotation startPos ->
                model
                    |> selectAnnotation index annotation
                    |> startMovingAnnotation index annotation startPos
                    => []

            MoveAnnotation index annotation oldPos newPos ->
                model
                    |> moveAnnotation index annotation oldPos newPos
                    => []

            FinishMovingAnnotation index annotation startPos endPos ->
                model
                    |> moveAnnotation index annotation startPos endPos
                    |> finishMovingAnnotation index annotation
                    => []

            StartResizingAnnotation index annotation vertex startPos ->
                model
                    |> selectAnnotation index annotation
                    |> startResizingAnnotation index annotation vertex startPos
                    => []

            ResizeAnnotation index annotation vertex startPos endPos ->
                model
                    |> resizeAnnotation index annotation vertex startPos endPos
                    => []

            FinishResizingAnnotation index annotation vertex startPos endPos ->
                model
                    |> resizeAnnotation index annotation vertex startPos endPos
                    |> selectAnnotation index annotation
                    => []

            Undo ->
                { model | edits = UndoList.undo model.edits }
                    => []

            Redo ->
                { model | edits = UndoList.redo model.edits }
                    => []

            Save ->
                model
                    => [ case model.images of
                            Just images ->
                                exportToImage <| List.Zipper.current images

                            Nothing ->
                                Cmd.none
                       ]


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
startAnnotation startPos images model =
    model
        |> startDrawing startPos
        |> updateMouse images startPos


resetSelection : Model -> Model
resetSelection model =
    { model | annotationState = ReadyToDraw }


startDrawing : StartPosition -> Model -> Model
startDrawing startPos model =
    { model | annotationState = DrawingAnnotation startPos }


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
            { model | edits = UndoList.new (Array.set index (fn annotation) model.edits.present) model.edits }

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
    { model | edits = UndoList.new (Array.push annotation model.edits.present) model.edits }
        |> finishDrawing


finishDrawing : Model -> Model
finishDrawing model =
    { model | annotationState = ReadyToDraw }


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


updateFill : Fill -> Annotation -> Annotation
updateFill fill annotation =
    case annotation of
        Lines _ _ ->
            annotation

        Shapes shapeType shape ->
            Shapes shapeType { shape | fill = fill }

        TextBox textBox ->
            annotation


updateLineStroke : StrokeWidth -> Annotation -> Annotation
updateLineStroke strokeWidth annotation =
    case annotation of
        Lines lineType line ->
            Lines lineType { line | strokeWidth = strokeWidth }

        Shapes shapeType shape ->
            Shapes shapeType { shape | strokeWidth = strokeWidth }

        TextBox textBox ->
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


setStrokeWidth : StrokeWidth -> Model -> Model
setStrokeWidth strokeWidth model =
    { model | strokeWidth = strokeWidth }


setStrokeStyle : StrokeStyle -> Model -> Model
setStrokeStyle strokeStyle model =
    { model | strokeStyle = strokeStyle }


setStrokeColor : Color -> Model -> Model
setStrokeColor strokeColor model =
    { model | strokeColor = strokeColor }


startMovingAnnotation : Int -> Annotation -> StartPosition -> Model -> Model
startMovingAnnotation index annotation startPos model =
    { model | annotationState = MovingAnnotation index annotation startPos }


moveAnnotation : Int -> Annotation -> StartPosition -> EndPosition -> Model -> Model
moveAnnotation index annotation oldPos newPos model =
    { model | edits = UndoList.mapPresent (Array.set index (move oldPos newPos annotation)) model.edits }


startResizingAnnotation : Int -> Annotation -> Vertex -> StartPosition -> Model -> Model
startResizingAnnotation index annotation vertex startPos model =
    { model | annotationState = ResizingAnnotation index annotation startPos vertex }


resizeAnnotation : Int -> Annotation -> Vertex -> StartPosition -> EndPosition -> Model -> Model
resizeAnnotation index annotation vertex oldPos newPos model =
    { model | edits = UndoList.mapPresent (Array.set index (resize oldPos newPos vertex annotation)) model.edits }


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
        Lines lineType line ->
            Lines lineType (resizeVertices end vertex line)

        Shapes shapeType shape ->
            Shapes shapeType (resizeVertices end vertex shape)

        TextBox textArea ->
            TextBox (resizeVertices end vertex textArea)


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
                    selectShape TextBorder model.keyboardState

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
                                changeDrawing (transitionOnShift model.drawing) model

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
                viewImageSelector images


viewImageSelector : Zipper Image -> Html Msg
viewImageSelector images =
    images
        |> List.Zipper.toList
        |> List.map (viewImageOption images)
        |> div [ Html.class "image-selector" ]


viewImageOption : Zipper Image -> Image -> Html Msg
viewImageOption zipper image =
    button
        [ Html.class "image-option"
        , Html.width <| round image.width
        , Html.height <| round image.height
        , onClick <| SelectImage image
        ]
        [ Html.img [ src image.url, Html.height <| round image.height, Html.width <| round image.width ] []
        , Html.div [ onClick <| SelectImage image, Html.class "image-edit-pencil" ]
            [ viewPencilIcon
            ]
        ]


viewPencilIcon : Html msg
viewPencilIcon =
    svg [ Attr.width "20", Attr.height "20", viewBox "0 0 500 500" ]
        [ Svg.path [ fill "#555", d "M492.8,58L442,7.2c-9.6-9.6-25.3-9.6-34.8,0l-17.6,17.6l-1.5,1.5L377.4,37l85.5,85.5l10.8-10.8l1.5-1.5l17.6-17.6C502.4,83.2,502.4,67.6,492.8,58z" ] []
        , Svg.path [ fill "#555", d "M51.7,362.4l85.5,85.5l308.5-308.5l-85.5-85.5L51.7,362.4z M395.2,148.7L146.4,397.3l-9.3-9.3l248.8-248.8L395.2,148.7z M111.7,362.6l-9.3-9.3l248.7-248.8l9.3,9.3L111.7,362.6z" ] []
        , Svg.polygon [ fill "#555", points "36.4,377.9 14.1,452.9 47.1,485.9 122.1,463.6 79.3,420.7" ] []
        , Svg.polygon [ fill "#555", points "0,500 36,489.2 10.8,464" ] []
        ]


viewInfoScreen : Html Msg
viewInfoScreen =
    div []
        [ Html.text "please upload an image!" ]


viewImageAnnotator : Model -> Image -> Html Msg
viewImageAnnotator ({ edits, fill, strokeColor, mouse, keyboardState, currentDropdown, drawing } as model) selectedImage =
    let
        toDropdownMenu =
            viewDropdownMenu currentDropdown drawing model

        shiftPressed =
            isPressed Shift keyboardState
    in
        div
            [ Html.class "annotation-app" ]
            [ div [ Html.class "controls" ]
                [ div [ Html.class "columns" ]
                    [ button [ onClick Cancel, Html.class "cancel-button" ] [ Html.text "Cancel" ]
                    , button [ onClick Save, Html.class "save-button" ] [ Html.text "Save" ]
                    ]
                , viewHistoryControls edits
                , div [ Html.class "columns" ]
                    (List.map (viewDrawingButton drawing toDropdownMenu) (drawingOptions shiftPressed)
                        ++ [ viewFillDropdown toDropdownMenu fill
                           , viewStrokeColorDropdown toDropdownMenu strokeColor
                           , viewLineStrokeDropdown toDropdownMenu
                           ]
                    )
                ]
            , viewCanvas model selectedImage
            ]


drawingsAreEqual : Drawing -> Drawing -> Bool
drawingsAreEqual drawing drawing2 =
    case drawing of
        DrawLine lineType _ ->
            case drawing2 of
                DrawLine lineType2 _ ->
                    lineType == lineType2

                _ ->
                    False

        DrawShape shapeType _ ->
            case drawing2 of
                DrawShape shapeType2 _ ->
                    shapeType == shapeType2

                _ ->
                    False


viewDrawingButton : Drawing -> (AttributeDropdown -> Html Msg) -> Drawing -> Html Msg
viewDrawingButton selectedDrawing toDropdownMenu drawing =
    case drawing of
        DrawLine lineType lineMode ->
            button
                [ Html.classList [ "drawing-button" => True, "drawing-button--selected" => drawingsAreEqual selectedDrawing drawing ]
                , onClick <| ChangeDrawing drawing
                ]
                [ viewShapeSvg drawing ]

        DrawShape shapeType shapeMode ->
            case shapeType of
                TextBorder ->
                    viewTextSizeDropdown selectedDrawing toDropdownMenu

                _ ->
                    button
                        [ Html.classList [ "drawing-button" => True, "drawing-button--selected" => drawingsAreEqual selectedDrawing drawing ]
                        , onClick <| ChangeDrawing drawing
                        ]
                        [ viewShapeSvg drawing ]


viewHistoryControls : UndoList (Array Annotation) -> Html Msg
viewHistoryControls edits =
    div [ Html.class "history-controls" ]
        [ button [ onClick Undo, Html.class "history-button", disabled <| not <| UndoList.hasPast edits ] [ viewUndoArrow ]
        , button [ onClick Redo, Html.class "history-button flip", disabled <| not <| UndoList.hasFuture edits ] [ viewUndoArrow ]
        ]


viewTextSizeDropdown : Drawing -> (AttributeDropdown -> Html Msg) -> Html Msg
viewTextSizeDropdown drawing toDropdownMenu =
    div [ Html.class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown Fonts
            , Html.classList [ "drawing-button" => True, "drawing-button--selected" => drawingsAreEqual drawing (DrawShape TextBorder DrawingShape) ]
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


viewLineStrokeDropdown : (AttributeDropdown -> Html Msg) -> Html Msg
viewLineStrokeDropdown toDropdownMenu =
    div
        [ Html.class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown Strokes
            , Html.class "drawing-button"
            ]
            [ viewLineStrokeDropdownIcon Color.grey
            ]
        , toDropdownMenu Strokes
        ]


viewFillDropdown : (AttributeDropdown -> Html Msg) -> Fill -> Html Msg
viewFillDropdown toDropdownMenu fill =
    div
        [ Html.class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown Fills
            , Html.class "drawing-button"
            ]
            [ viewFillIcon fill
            ]
        , toDropdownMenu Fills
        ]


viewStrokeColorDropdown : (AttributeDropdown -> Html Msg) -> Color -> Html Msg
viewStrokeColorDropdown toDropdownMenu strokeColor =
    div
        [ Html.class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown StrokeColors
            , Html.class "drawing-button"
            ]
            [ viewStrokeColorIcon strokeColor
            ]
        , toDropdownMenu StrokeColors
        ]


viewDropdownMenu : Maybe AttributeDropdown -> Drawing -> Model -> AttributeDropdown -> Html Msg
viewDropdownMenu maybeDropdown drawing model selectedOption =
    Maybe.map (viewDropdownOptions drawing model selectedOption) maybeDropdown
        |> Maybe.withDefault (Html.text "")


viewDropdownOptions : Drawing -> Model -> AttributeDropdown -> AttributeDropdown -> Html Msg
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
                viewLineStrokeOptions model.strokeWidth model.strokeStyle


viewShapeSvg : Drawing -> Html Msg
viewShapeSvg drawing =
    case drawing of
        DrawLine lineType _ ->
            case lineType of
                StraightLine ->
                    viewLineIcon

                Arrow ->
                    viewArrowIcon

        DrawShape shapeType _ ->
            case shapeType of
                Rect ->
                    viewRectangleIcon

                RoundedRect ->
                    viewRoundedRectangleIcon

                Ellipse ->
                    viewEllipseIcon

                TextBorder ->
                    viewTextIcon

                SpotlightRect ->
                    viewSpotlightIcon


viewLineStrokeOptions : StrokeWidth -> StrokeStyle -> Html Msg
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


viewLineStrokeOption : StrokeWidth -> StrokeWidth -> Html Msg
viewLineStrokeOption selectedStroke stroke =
    button
        [ Html.classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedStroke == stroke
            ]
        , onClick <| SelectLineStroke stroke
        ]
        [ viewLineStroke (strokeToWidth stroke) [] ]


drawingStateEvents : Drawing -> AnnotationState -> List (Html.Attribute Msg)
drawingStateEvents drawing annotationState =
    case annotationState of
        ReadyToDraw ->
            [ onMouseDown <| Json.map (StartDrawing << toDrawingPosition) Mouse.position
            ]

        DrawingAnnotation startPos ->
            onMouseUpOrLeave <| Json.map (FinishDrawing startPos << toDrawingPosition) Mouse.position

        MovingAnnotation index annotation startPos ->
            [ Html.Events.onMouseLeave ResetToReadyToDraw
            , SE.on "mouseup" <| Json.map (FinishMovingAnnotation index annotation startPos << toDrawingPosition) Mouse.position
            ]

        ResizingAnnotation index annotation startPos vertex ->
            [ Html.Events.onMouseLeave ResetToReadyToDraw
            , SE.on "mouseup" <| Json.map (FinishResizingAnnotation index annotation vertex startPos << toDrawingPosition) Mouse.position
            ]

        SelectedAnnotation index annotation ->
            [ onMouseDown <| Json.map (StartDrawing << toDrawingPosition) Mouse.position ]

        EditingATextBox index ->
            [ SE.onClick <| FinishEditingText index ]


viewMask : Float -> Float -> Svg msg
viewMask width height =
    rect
        [ x "0"
        , y "0"
        , Attr.height <| toString height
        , Attr.width <| toString width
        , Attr.mask "url(#Mask)"
        , Attr.style "pointer-events: none;"
        ]
        []


viewSpotlights : AnnotationState -> Array Annotation -> List (Svg Msg)
viewSpotlights annotationState annotations =
    annotations
        |> Array.filter isSpotlightShape
        |> Array.map spotlightFillToMaskFill
        |> Array.toList
        |> List.indexedMap (viewAnnotation annotationState)
        |> List.concat


canvasAttributes : Image -> Drawing -> AnnotationState -> List (Svg.Attribute Msg)
canvasAttributes image drawing annotationState =
    [ Html.id "canvas"
    , Html.class "image-edit"
    , Html.style
        [ "width" => toString (round image.width) ++ "px"
        , "height" => toString (round image.height) ++ "px"
        , "cursor" => annotationStateToCursor annotationState
        ]
    ]
        ++ drawingStateEvents drawing annotationState


viewNonSpotlightAnnotations : AnnotationState -> Array Annotation -> List (Svg Msg)
viewNonSpotlightAnnotations annotationState annotations =
    annotations
        |> Array.toList
        |> List.indexedMap (viewAnnotation annotationState)
        |> List.concat


viewDefinitions : Float -> Float -> AnnotationState -> List (Svg Msg) -> List (Svg Msg)
viewDefinitions width height annotationState cutOuts =
    List.map viewArrowHeadDefinition strokeColorOptions
        |> (::) (maskDefinition annotationState width height cutOuts)
        |> (::) viewSvgFilters
        |> defs []
        |> List.singleton


getFirstSpotlightIndex : Array Annotation -> Int
getFirstSpotlightIndex annotations =
    annotations
        |> Array.toList
        |> List.Extra.findIndex isSpotlightShape
        |> Maybe.withDefault 0


getAnnotations : Image -> Array Annotation -> List (Svg Msg) -> List (Svg Msg) -> Bool -> List (Svg Msg)
getAnnotations image annotations spotlights nonSpotlights isDrawingSpotlight =
    let
        firstSpotlightIndex =
            getFirstSpotlightIndex annotations
    in
        if isDrawingSpotlight && List.isEmpty spotlights then
            nonSpotlights ++ [ viewMask image.width image.height ]
        else if List.isEmpty spotlights then
            nonSpotlights
        else
            List.take (firstSpotlightIndex) nonSpotlights
                ++ [ viewMask image.width image.height ]
                ++ List.drop firstSpotlightIndex nonSpotlights


viewDrawingAndAnnotations :
    (List (Svg Msg) -> List (Svg Msg))
    -> List (Svg Msg)
    -> (Bool -> List (Svg Msg))
    -> (StartPosition -> Bool -> Svg Msg)
    -> Drawing
    -> AnnotationState
    -> List (Svg Msg)
viewDrawingAndAnnotations definitions spotlights toAnnotations toDrawing drawing annotationState =
    let
        justAnnotations =
            definitions spotlights ++ toAnnotations False

        nonSpotlightDrawingAndAnnotations start =
            definitions spotlights ++ toAnnotations False ++ [ toDrawing start False ]

        spotlightDrawingAndAnnotations start =
            definitions (spotlights ++ [ toDrawing start True ]) ++ toAnnotations True ++ [ toDrawing start False ]
    in
        case annotationState of
            DrawingAnnotation start ->
                case drawing of
                    DrawShape shapeType _ ->
                        case shapeType of
                            SpotlightRect ->
                                spotlightDrawingAndAnnotations start

                            _ ->
                                nonSpotlightDrawingAndAnnotations start

                    _ ->
                        nonSpotlightDrawingAndAnnotations start

            _ ->
                justAnnotations


viewCanvas : Model -> Image -> Html Msg
viewCanvas model image =
    let
        annotations =
            model.edits.present

        toDrawing start isInMask =
            viewDrawing model start isInMask

        spotlights =
            viewSpotlights model.annotationState annotations

        nonSpotlights =
            viewNonSpotlightAnnotations model.annotationState annotations

        definitions =
            viewDefinitions image.width image.height model.annotationState

        toAnnotations =
            getAnnotations image annotations spotlights nonSpotlights
    in
        div (canvasAttributes image model.drawing model.annotationState)
            (viewImage image
                :: [ svg
                        [ Attr.id "drawing"
                        , Attr.class "drawing"
                        , Attr.width <| toString <| round image.width
                        , Attr.height <| toString <| round image.height
                        , Html.attribute "xmlns" "http://www.w3.org/2000/svg"
                        ]
                        (viewDrawingAndAnnotations definitions spotlights toAnnotations toDrawing model.drawing model.annotationState)
                   ]
            )


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
    marker
        [ Attr.id <| "arrow-head--" ++ Color.Convert.colorToHex color
        , orient "auto"
        , markerWidth "2"
        , markerHeight "4"
        , refX "0.1"
        , refY "2"
        , Attr.style "cursor: pointer;"
        ]
        [ Svg.path [ d "M0,0 V4 L2,2 Z", Attr.fill <| Color.Convert.colorToHex color ] []
        ]


isSpotlightShape : Annotation -> Bool
isSpotlightShape annotation =
    case annotation of
        Shapes shapeType _ ->
            shapeType == SpotlightRect

        _ ->
            False


spotlightFillToMaskFill : Annotation -> Annotation
spotlightFillToMaskFill annotation =
    case annotation of
        Shapes shapeType shape ->
            case shapeType of
                SpotlightRect ->
                    Shapes Rect { shape | fill = MaskFill }

                _ ->
                    annotation

        _ ->
            annotation


annotationStateEvents : Int -> Annotation -> AnnotationState -> List (Svg.Attribute Msg)
annotationStateEvents index annotation annotationState =
    case annotationState of
        ReadyToDraw ->
            [ SE.on "mousedown" <| Json.map (SelectAnnotation index annotation << toDrawingPosition) Mouse.position
            , Attr.style "cursor: pointer;"
            , Html.attribute "onmousedown" "event.stopPropagation();"
            ]

        DrawingAnnotation start ->
            [ Attr.style "cursor: crosshair;" ]

        SelectedAnnotation start annotation ->
            [ Attr.style "cursor: move;" ]

        MovingAnnotation index annotation startPos ->
            [ SE.on "mouseup" <| Json.map (FinishMovingAnnotation index annotation startPos << toDrawingPosition) Mouse.position
            , Attr.style "cursor: move;"
            ]

        ResizingAnnotation _ _ _ _ ->
            [ Attr.style "cursor: nesw-resize;" ]

        EditingATextBox index ->
            [ Attr.style "cursor: crosshair;" ]


getSelectState : Int -> AnnotationState -> SelectState
getSelectState index annotationState =
    case annotationState of
        SelectedAnnotation int annotation ->
            if index == int then
                SelectedWithVertices
            else
                NotSelected

        MovingAnnotation int annotation startPosition ->
            if index == int then
                SelectedWithVertices
            else
                NotSelected

        ResizingAnnotation int annotation startPosition vertex ->
            if index == int then
                SelectedWithVertices
            else
                NotSelected

        EditingATextBox int ->
            if index == int then
                Selected
            else
                NotSelected

        _ ->
            NotSelected


viewAnnotation : AnnotationState -> Int -> Annotation -> List (Svg Msg)
viewAnnotation annotationState index annotation =
    let
        selectState =
            getSelectState index annotationState

        movementEvents =
            annotationStateEvents index annotation annotationState

        toVertexEvents =
            annotationStateVertexEvents index annotation annotationState

        vertices { start, end } =
            viewVertices start end toVertexEvents selectState
    in
        case annotation of
            Lines lineType line ->
                viewLine movementEvents (vertices line) lineType line

            Shapes shapeType shape ->
                viewShape movementEvents (vertices shape) shapeType shape

            TextBox textBox ->
                viewTextBox movementEvents (vertices textBox) annotationState selectState index textBox


annotationStateVertexEvents : Int -> Annotation -> AnnotationState -> Vertex -> List (Svg.Attribute Msg)
annotationStateVertexEvents index annotation annotationState vertex =
    [ SE.on "mousedown" <| Json.map (StartResizingAnnotation index annotation vertex << toDrawingPosition) Mouse.position
    , Attr.style "cursor: nesw-resize;"
    , Html.attribute "onmousedown" "event.stopPropagation();"
    ]
        ++ case annotationState of
            ResizingAnnotation int annotation start vertex ->
                [ SE.on "mouseup" <| Json.map (FinishResizingAnnotation index annotation vertex start << toDrawingPosition) Mouse.position ]

            _ ->
                []


maskDefinition : AnnotationState -> Float -> Float -> List (Svg Msg) -> Svg Msg
maskDefinition annotationState width height shapes =
    rect
        ([ x "0"
         , y "0"
         , Attr.width <| toString width
         , Attr.height <| toString height
         , opacity "0.5"
         , fill "white"
         ]
        )
        []
        :: shapes
        |> Svg.mask [ Attr.id "Mask" ]


calcShapePos : StartPosition -> EndPosition -> ShapeMode -> EndPosition
calcShapePos start end shapeMode =
    case shapeMode of
        DrawingShape ->
            end

        DrawingEqualizedShape ->
            equalXandY start end


calcLinePos : StartPosition -> EndPosition -> LineMode -> EndPosition
calcLinePos start end lineMode =
    case lineMode of
        DrawingLine ->
            end

        DrawingDiscreteLine ->
            stepMouse start end


viewDrawing : Model -> StartPosition -> Bool -> Svg Msg
viewDrawing { drawing, fill, strokeColor, strokeWidth, strokeStyle, fontSize, mouse, keyboardState } pos isInMask =
    let
        lineAttrs lineType lineMode =
            lineAttributes lineType <| Line pos (calcLinePos pos mouse lineMode) strokeColor strokeWidth strokeStyle

        shapeAttrs shapeType shapeMode =
            shapeAttributes shapeType <| Shape pos (calcShapePos pos mouse shapeMode) fill strokeColor strokeWidth strokeStyle

        spotlightAttrs shapeType shapeMode spotlightFill spotlightColor =
            shapeAttributes shapeType <| Shape pos (calcShapePos pos mouse shapeMode) spotlightFill spotlightColor strokeWidth strokeStyle
    in
        case drawing of
            DrawLine lineType lineMode ->
                case lineType of
                    Arrow ->
                        Svg.path (lineAttrs lineType lineMode) []

                    StraightLine ->
                        Svg.path (lineAttrs lineType lineMode) []

            DrawShape shapeType shapeMode ->
                case shapeType of
                    Rect ->
                        Svg.rect (shapeAttrs shapeType shapeMode) []

                    RoundedRect ->
                        Svg.rect (shapeAttrs shapeType shapeMode) []

                    Ellipse ->
                        Svg.ellipse (shapeAttrs shapeType shapeMode) []

                    TextBorder ->
                        TextArea pos (calcShapePos pos mouse shapeMode) strokeColor fontSize "" 0 (AutoExpand.initState (config 0 fontSize))
                            |> viewTextBoxWithBorder

                    SpotlightRect ->
                        if isInMask then
                            Svg.rect (spotlightAttrs shapeType shapeMode MaskFill Color.white) []
                        else
                            Svg.rect (spotlightAttrs shapeType shapeMode EmptyFill strokeColor) []


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


viewShape : List (Svg.Attribute Msg) -> List (Svg Msg) -> ShapeType -> Shape -> List (Svg Msg)
viewShape attrs vertices shapeType shape =
    let
        allAttrs =
            shapeAttributes shapeType shape ++ attrs
    in
        flip List.append vertices <|
            case shapeType of
                Rect ->
                    [ Svg.rect allAttrs [] ]

                RoundedRect ->
                    [ Svg.rect allAttrs [] ]

                Ellipse ->
                    [ Svg.ellipse allAttrs [] ]

                SpotlightRect ->
                    [ Svg.rect allAttrs [] ]

                TextBorder ->
                    [ Svg.rect allAttrs [] ]


viewVertices : StartPosition -> EndPosition -> (Vertex -> List (Svg.Attribute Msg)) -> SelectState -> List (Svg Msg)
viewVertices start end toVertexEvents selectState =
    if selectState == SelectedWithVertices then
        shapeVertices toVertexEvents start end
    else
        []


shapeAttrs : Shape -> List (Svg.Attribute Msg)
shapeAttrs ({ strokeStyle, strokeColor, strokeWidth, fill } as shape) =
    (Attr.style <| pointerEvents fill) :: strokeAttrs strokeStyle strokeColor strokeWidth


strokeAttrs : StrokeStyle -> Color -> StrokeWidth -> List (Svg.Attribute Msg)
strokeAttrs strokeStyle strokeColor strokeWidth =
    [ Attr.strokeWidth <| toString <| strokeToWidth strokeWidth
    , Attr.stroke <| Color.Convert.colorToHex strokeColor
    , toLineStyle strokeStyle
    ]


rectAttrs : Shape -> List (Svg.Attribute Msg)
rectAttrs { start, end } =
    [ Attr.width <| toString <| abs <| end.x - start.x
    , Attr.height <| toString <| abs <| end.y - start.y
    , x <| toString <| Basics.min start.x end.x
    , y <| toString <| Basics.min start.y end.y
    ]


ellipseAttributes : Shape -> List (Svg.Attribute Msg)
ellipseAttributes { start, end } =
    [ rx <| toString <| abs <| end.x - start.x
    , ry <| toString <| abs <| end.y - start.y
    , cx <| toString <| start.x
    , cy <| toString <| start.y
      -- , Attr.filter "url(#dropShadow)"
    ]


shapeAttributes : ShapeType -> Shape -> List (Svg.Attribute Msg)
shapeAttributes shapeType shape =
    List.append (fillStyle shape.fill) <|
        case shapeType of
            Rect ->
                shapeAttrs shape ++ rectAttrs shape

            RoundedRect ->
                shapeAttrs shape ++ rectAttrs shape ++ [ rx "15", ry "15" ]

            Ellipse ->
                shapeAttrs shape ++ ellipseAttributes shape

            TextBorder ->
                shapeAttrs shape ++ rectAttrs shape

            SpotlightRect ->
                shapeAttrs shape ++ rectAttrs shape


shapeVertices : (Vertex -> List (Svg.Attribute Msg)) -> StartPosition -> EndPosition -> List (Svg Msg)
shapeVertices toVertexEvents start end =
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
        shapeVertices toVertexEvents rectStart end


viewTextArea : Int -> SelectState -> TextArea -> Svg Msg
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
            ]
            [ AutoExpand.view (config index fontSize) autoexpand text
            ]
        ]


viewTextBox : List (Svg.Attribute Msg) -> List (Svg Msg) -> AnnotationState -> SelectState -> Int -> TextArea -> List (Svg Msg)
viewTextBox attrs vertices annotationState selectState index ({ start, end, text, fill, fontSize, angle, autoexpand } as textBox) =
    case selectState of
        Selected ->
            (viewTextArea index selectState textBox)
                |> List.singleton
                |> flip List.append (viewShape ([ Attr.style "opacity: 0;" ] ++ attrs) vertices Rect (Shape start end EmptyFill Color.black Thin Solid))

        NotSelected ->
            textBox.text
                |> String.split "\n"
                |> List.map (Svg.tspan [ dy <| toString <| fontSize, x <| toString <| Basics.min start.x end.x ] << List.singleton << Svg.text)
                |> Svg.text_ [ y <| toString <| Basics.min start.y end.y, Attr.style "pointer-events: none; user-select: none;" ]
                |> List.singleton
                |> flip List.append (viewShape ([ Attr.style "stroke: transparent; pointer-events: auto; cursor: pointer;" ] ++ attrs) vertices Rect (Shape start end EmptyFill Color.black Thin Solid))

        SelectedWithVertices ->
            textBox.text
                |> String.split "\n"
                |> List.map (Svg.tspan [ dy <| toString <| fontSize, x <| toString <| Basics.min start.x end.x ] << List.singleton << Svg.text)
                |> Svg.text_ [ y <| toString <| Basics.min start.y end.y, Attr.style "pointer-events: none; user-select: none;" ]
                |> List.singleton
                |> flip List.append
                    (viewShape attrs vertices Rect (Shape start end EmptyFill Color.black Thin Solid)
                        ++ viewShape (attrs ++ [ SE.onMouseDown <| StartEditingText index textBox, Attr.style "pointer-events: fill; cursor: pointer;" ]) vertices Rect (Shape start end EmptyFill Color.black Thin Solid)
                    )


viewTextBoxWithBorder : TextArea -> Svg Msg
viewTextBoxWithBorder ({ start, end, text, fill, fontSize, angle } as textBox) =
    Svg.g []
        [ Svg.rect (shapeAttributes Rect (Shape start end EmptyFill Color.black Thin Solid)) []
        ]


viewLine : List (Svg.Attribute Msg) -> List (Svg Msg) -> LineType -> Line -> List (Svg Msg)
viewLine attrs vertices lineType line =
    let
        allAttrs =
            lineAttributes lineType line ++ attrs
    in
        List.append vertices <|
            case lineType of
                StraightLine ->
                    [ Svg.path allAttrs [] ]

                Arrow ->
                    [ Svg.path allAttrs [] ]


simpleLineAttrs : Line -> List (Svg.Attribute Msg)
simpleLineAttrs ({ start, end } as line) =
    []
        ++ [ Attr.fill "none"
           , d <| "M" ++ toString start.x ++ "," ++ toString start.y ++ " l" ++ toString (end.x - start.x) ++ "," ++ toString (end.y - start.y)
             -- , Attr.filter "url(#dropShadow)"
           ]


arrowAttributes : Line -> List (Svg.Attribute Msg)
arrowAttributes line =
    [ markerEnd <| "url(#arrow-head--" ++ Color.Convert.colorToHex line.strokeColor ++ ")" ]


lineAttributes : LineType -> Line -> List (Svg.Attribute Msg)
lineAttributes lineType line =
    case lineType of
        Arrow ->
            arrowAttributes line ++ simpleLineAttrs line ++ strokeAttrs line.strokeStyle line.strokeColor line.strokeWidth

        StraightLine ->
            simpleLineAttrs line ++ strokeAttrs line.strokeStyle line.strokeColor line.strokeWidth


viewImage : Image -> Html Msg
viewImage { width, height, url } =
    Html.img
        [ Html.class "image-to-annotate"
        , Html.width (round width)
        , Html.height (round height)
        , src url
        ]
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
    svg [ Attr.width "14", Attr.height "14", viewBox "0 0 26.676 26.676" ]
        [ Svg.path [ d "M26.105,21.891c-0.229,0-0.439-0.131-0.529-0.346l0,0c-0.066-0.156-1.716-3.857-7.885-4.59c-1.285-0.156-2.824-0.236-4.693-0.25v4.613c0,0.213-0.115,0.406-0.304,0.508c-0.188,0.098-0.413,0.084-0.588-0.033L0.254,13.815C0.094,13.708,0,13.528,0,13.339c0-0.191,0.094-0.365,0.254-0.477l11.857-7.979c0.175-0.121,0.398-0.129,0.588-0.029c0.19,0.102,0.303,0.295,0.303,0.502v4.293c2.578,0.336,13.674,2.33,13.674,11.674c0,0.271-0.191,0.508-0.459,0.562C26.18,21.891,26.141,21.891,26.105,21.891z", fill "currentColor" ] []
        ]


viewArrowIcon : Html msg
viewArrowIcon =
    svg [ Attr.width "20", Attr.height "20", viewBox "0 0 347.341 347.341" ]
        [ polygon [ points "347.341,107.783 347.339,0 239.559,0.002 282.843,43.285 0,326.128 21.213,347.341 304.056,64.498", fill "grey" ] []
        ]


viewRectangleIcon : Html msg
viewRectangleIcon =
    svg [ Attr.width "14", Attr.height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M2 12h10V2H2v10zM0 0h14v14H0V0z", fillRule "nonzero", fill "#555" ] [] ]


viewSpotlightIcon : Html msg
viewSpotlightIcon =
    svg [ Attr.width "14", Attr.height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M4 10h6V4H4v6zM0 0h14v14H0V0z", fillRule "nonzero", fill "#555" ] [] ]


viewRoundedRectangleIcon : Html msg
viewRoundedRectangleIcon =
    svg [ Attr.width "14", Attr.height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M2 3v8c0 .552.448 1 1 1h8c.552 0 1-.448 1-1V3c0-.552-.448-1-1-1H3c-.552 0-1 .448-1 1zM0 3c0-1.655 1.342-3 3-3h8c1.655 0 3 1.342 3 3v8c0 1.655-1.342 3-3 3H3c-1.655 0-3-1.342-3-3V3z", fillRule "nonzero", fill "#555" ] [] ]


viewEllipseIcon : Html msg
viewEllipseIcon =
    svg [ Attr.width "14", Attr.height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M2 7c0 2.757 2.242 5 5 5 2.757 0 5-2.242 5-5 0-2.757-2.242-5-5-5-2.757 0-5 2.242-5 5zM0 7c0-3.866 3.142-7 7-7 3.866 0 7 3.142 7 7 0 3.866-3.142 7-7 7-3.866 0-7-3.142-7-7z", fillRule "nonzero", fill "#555" ] [] ]


viewFillIcon : Fill -> Html msg
viewFillIcon fill =
    let
        ( fillAttr, showLine ) =
            case fill of
                SolidFill color ->
                    if color == Color.white then
                        [ Attr.fill <| Color.Convert.colorToHex Color.white
                        , Attr.stroke "#555"
                        , r "6"
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
                    [ Attr.fill "#555", r "6" ]
                        => True
    in
        svg [ Attr.width "14", Attr.height "14", viewBox "0 0 14 14" ]
            ([ circle
                ([ cx "7", cy "7", r "6" ]
                    ++ fillAttr
                )
                []
             ]
                ++ if showLine then
                    [ Svg.path [ d "M11 0L0 11l1 1L12 1z", fillRule "nonzero", Attr.fill "white" ] [] ]
                   else
                    []
            )


viewStrokeColorIcon : Color -> Html msg
viewStrokeColorIcon strokeColor =
    svg [ Attr.width "14", Attr.height "14", viewBox "0 0 14 14" ]
        [ Svg.path ([ d "M2 7c0 2.757 2.242 5 5 5 2.757 0 5-2.242 5-5 0-2.757-2.242-5-5-5-2.757 0-5 2.242-5 5zM0 7c0-3.866 3.142-7 7-7 3.866 0 7 3.142 7 7 0 3.866-3.142 7-7 7-3.866 0-7-3.142-7-7z", fillRule "nonzero", fill <| Color.Convert.colorToHex strokeColor ]) [] ]


viewLineStrokeDropdownIcon : Color -> Html msg
viewLineStrokeDropdownIcon strokeColor =
    svg [ Attr.width "20", Attr.height "20", viewBox "0 0 20 20" ]
        [ g [ stroke <| Color.Convert.colorToHex strokeColor ]
            [ line [ x1 "0", x2 "20", y1 "10", y2 "10", strokeWidth "6" ] []
            ]
        ]


viewLineIcon : Html msg
viewLineIcon =
    svg [ Attr.width "20", Attr.height "20", viewBox "0 0 12 12" ]
        [ Svg.path [ d "M11 0L0 11l1 1L12 1z", fillRule "nonzero", fill "#555" ] [] ]


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
    svg [ viewBox "0 0 12 15", Attr.height "12", Attr.width "15" ]
        [ Svg.path [ d "M0 0v4l2-2h3v10.03H3l-1 2h8l-1-2H7V2h3l2 2V0z", fillRule "evenodd" ] []
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


strokeToWidth : StrokeWidth -> Int
strokeToWidth stroke =
    case stroke of
        Thin ->
            4

        Medium ->
            6

        Thick ->
            8


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
    { mouse | x = mouse.x - 72, y = mouse.y }


annotationStateToCursor : AnnotationState -> String
annotationStateToCursor annotationState =
    case annotationState of
        ReadyToDraw ->
            "crosshair"

        DrawingAnnotation _ ->
            "crosshair"

        MovingAnnotation _ _ _ ->
            "move"

        ResizingAnnotation _ _ _ vertex ->
            "nesw-resize"

        EditingATextBox _ ->
            "default"

        _ ->
            "crosshair"



-- Configuration


config : Int -> Float -> AutoExpand.Config Msg
config index fontSize =
    AutoExpand.config
        { onInput = AutoExpandInput index
        , padding = 2
        , lineHeight = fontSize
        , minRows = 1
        , maxRows = 4
        , attributes =
            [ Html.id <| "text-box-edit--" ++ toString index
            , Html.class "text-box-textarea"
            , Html.style [ "font-size" => toPx fontSize ]
            ]
        }



-- PORTS


port exportToImage : Image -> Cmd msg


port setImages : (List Image -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        case model.images of
            Nothing ->
                [ setImages SetImages ]

            Just images ->
                if not model.imageSelected then
                    []
                else
                    case model.annotationState of
                        DrawingAnnotation drawing ->
                            [ Mouse.moves (ContinueDrawing << toDrawingPosition)
                            , Sub.map KeyboardMsg Keyboard.subscriptions
                            ]

                        ResizingAnnotation index annotation startPos vertex ->
                            [ Mouse.moves (ResizeAnnotation index annotation vertex startPos << toDrawingPosition)
                            , Sub.map KeyboardMsg Keyboard.subscriptions
                            ]

                        MovingAnnotation index annotation startPos ->
                            [ Mouse.moves (MoveAnnotation index annotation startPos << toDrawingPosition) ]

                        _ ->
                            [ Sub.map KeyboardMsg Keyboard.subscriptions ]



-- MAIN


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init >> Rocket.batchInit
        , update = update >> Rocket.batchUpdate
        , view = view
        , subscriptions = subscriptions
        }
