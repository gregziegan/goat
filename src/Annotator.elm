port module Annotator exposing (..)

import Array exposing (Array)
import Char exposing (KeyCode)
import Color exposing (Color)
import Color.Convert
import Dom
import Html exposing (Attribute, Html, button, div, p, text)
import Html.Attributes as Html exposing (class, classList, height, id, src, start, style, type_, width)
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput, onMouseEnter, onWithOptions)
import Json.Decode as Json
import Keyboard.Extra exposing (Key, KeyChange(..))
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


type TextMode
    = DrawingTextBox StartPosition
    | EditingText TextBox
    | RotatingText TextBox


type LineMode
    = DrawingLine StartPosition
    | DrawingDiscreteLine StartPosition


type Drawing
    = DrawArrow ArrowMode
    | DrawRect RectMode
    | DrawRoundedRect RoundedRectMode
    | DrawEllipse EllipseMode
    | DrawTextBox TextMode
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


type alias Arrow =
    { start : Position
    , end : Position
    , fill : Color
    , stroke : LineStroke
    , strokeStyle : StrokeStyle
    }


type alias Rect =
    { start : Position
    , end : Position
    , fill : Fill
    , strokeColor : Color
    , stroke : LineStroke
    , strokeStyle : StrokeStyle
    , rounded : Bool
    , showVertices : Bool
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
    = Shapes
    | Lines
    | Fonts
    | Fills
    | StrokeColors
    | Strokes
    | SpotlightShapes


type Annotation
    = Arrow_ Arrow
    | Rect_ Rect
    | Ellipse_ Ellipse
    | TextBox_ TextBox
    | Line_ Line


type alias EditState =
    { annotations : Array Annotation
    , drawing : Maybe Drawing
    }


type MovementState
    = DrawingAnnotation
    | HoveringOverAnnotation
    | HoveringOverShownVertices
    | OutsideShownVertices
    | MovingAnnotation Int Annotation StartPosition


type alias Model =
    { edits : UndoList EditState
    , fill : Fill
    , strokeColor : Color
    , stroke : LineStroke
    , strokeStyle : StrokeStyle
    , fontSize : Float
    , editMode : EditMode
    , lastShapeOption : EditMode
    , lastLineOption : EditMode
    , lastSpotlightOption : EditMode
    , mouse : Mouse.Position
    , keyboardState : Keyboard.Extra.State
    , images : Maybe (Zipper Image)
    , imageSelected : Bool
    , currentDropdown : Maybe EditOption
    , movementState : MovementState
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


shapeOptions : List EditMode
shapeOptions =
    [ EditRect
    , EditRoundedRect
    , EditEllipse
    ]


lineOptions : List EditMode
lineOptions =
    [ EditArrow
    , EditLine
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


spotlightShapeOptions : List EditMode
spotlightShapeOptions =
    [ EditSpotlightRect ]


initialEditState : EditState
initialEditState =
    { annotations = Array.empty
    , drawing = Nothing
    }


init : ( Model, List (Cmd Msg) )
init =
    { edits = UndoList.fresh initialEditState
    , fill = EmptyFill
    , strokeColor = Color.red
    , stroke = Medium
    , strokeStyle = Solid
    , fontSize = 14
    , editMode = EditArrow
    , lastShapeOption = EditRect
    , lastLineOption = EditArrow
    , lastSpotlightOption = EditSpotlightRect
    , mouse = Mouse.Position 0 0
    , keyboardState = Keyboard.Extra.initialState
    , images = List.Zipper.fromList []
    , imageSelected = False
    , currentDropdown = Nothing
    , movementState = DrawingAnnotation
    }
        => []



-- UPDATE


type Msg
    = StartRect StartPosition
    | AddRect StartPosition EndPosition
    | StartRoundedRect StartPosition
    | AddRoundedRect StartPosition EndPosition
    | StartArrow StartPosition
    | AddArrow StartPosition EndPosition
    | StartEllipse StartPosition
    | AddEllipse StartPosition EndPosition
    | StartTextBox StartPosition
    | PlaceTextBox StartPosition EndPosition
    | TextBoxInput TextBox String
    | BeginRotatingTextBox Float TextBox
    | FinishRotatingTextBox TextBox Float
    | AddTextBox TextBox
    | StartLine StartPosition
    | AddLine StartPosition EndPosition
    | StartSpotlightRect StartPosition
    | AddSpotlightRect StartPosition EndPosition
    | SetMouse Image Mouse.Position
    | SetImages (List Image)
    | KeyboardMsg Keyboard.Extra.Msg
    | SelectImage Image
    | ChangeEditMode EditMode
    | SelectFill Fill
    | SelectStrokeColor Color
    | SelectLineStroke LineStroke
    | SelectStrokeStyle StrokeStyle
    | SelectFontSize Float
    | ToggleDropdown EditOption
    | CloseDropdown
    | RemoveVertices
    | ShowGrabHand
    | HideGrabHand
    | StartMovingAnnotation Int Annotation StartPosition
    | MoveAnnotation Int Annotation StartPosition EndPosition
    | FinishMovingAnnotation Int Annotation StartPosition EndPosition
    | Undo
    | Redo
    | Export


update : Msg -> Model -> ( Model, List (Cmd Msg) )
update msg ({ edits, fill, fontSize, stroke, strokeColor, strokeStyle, mouse, images, keyboardState } as model) =
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
                { editState | drawing = Just <| DrawRect <| DrawingRect pos }
                    |> logChange model
                    |> updateMouse images pos
                    => []

            AddRect start end ->
                { editState
                    | annotations = Array.push (Rect_ <| Rect start end fill strokeColor stroke strokeStyle False False) editState.annotations
                    , drawing = Nothing
                }
                    |> skipChange model
                    |> hoverOverAnnotation
                    => []

            StartRoundedRect pos ->
                { editState | drawing = Just <| DrawRoundedRect <| DrawingRoundedRect pos }
                    |> logChange model
                    |> updateMouse images pos
                    => []

            AddRoundedRect start end ->
                { editState
                    | annotations = Array.push (Rect_ <| Rect start end fill strokeColor stroke strokeStyle True False) editState.annotations
                    , drawing = Nothing
                }
                    |> skipChange model
                    |> hoverOverAnnotation
                    => []

            StartArrow pos ->
                { editState | drawing = Just <| DrawArrow <| DrawingArrow pos }
                    |> logChange model
                    |> updateMouse images pos
                    => []

            AddArrow startPos endPos ->
                { editState
                    | annotations = Array.push (Arrow_ <| Arrow startPos endPos strokeColor stroke strokeStyle) editState.annotations
                    , drawing = Nothing
                }
                    |> skipChange model
                    => []

            StartEllipse pos ->
                { editState | drawing = Just <| DrawEllipse <| DrawingOval pos }
                    |> logChange model
                    |> updateMouse images pos
                    => []

            AddEllipse startPos endPos ->
                { editState
                    | annotations = Array.push (Ellipse_ <| Ellipse startPos endPos fill strokeColor stroke strokeStyle) editState.annotations
                    , drawing = Nothing
                }
                    |> skipChange model
                    => []

            StartTextBox pos ->
                { editState | drawing = Just <| DrawTextBox <| DrawingTextBox pos }
                    |> logChange model
                    |> updateMouse images pos
                    => []

            PlaceTextBox startPos endPos ->
                let
                    initialEditState =
                        TextBox startPos endPos "" strokeColor stroke fontSize 0

                    tryToEdit result =
                        case result of
                            Ok _ ->
                                TextBoxInput initialEditState ""

                            Err _ ->
                                Undo
                in
                    { editState | drawing = Just <| DrawTextBox <| EditingText initialEditState }
                        |> skipChange model
                        => [ Dom.focus "text-box-edit"
                                |> Task.attempt tryToEdit
                           ]

            TextBoxInput { start, end, angle } text ->
                { editState | drawing = Just <| DrawTextBox <| EditingText <| TextBox start end text strokeColor stroke fontSize angle }
                    |> skipChange model
                    => []

            BeginRotatingTextBox angle { start, end, text } ->
                { editState | drawing = Just <| DrawTextBox <| RotatingText <| TextBox start end text strokeColor stroke fontSize angle }
                    |> skipChange model
                    => []

            FinishRotatingTextBox { start, end, text } angle ->
                { editState | drawing = Just <| DrawTextBox <| EditingText <| TextBox start end text strokeColor stroke fontSize angle }
                    |> skipChange model
                    => []

            AddTextBox { start, end, text, angle } ->
                { editState
                    | annotations = Array.push (TextBox_ <| TextBox start end text strokeColor stroke fontSize angle) editState.annotations
                    , drawing = Nothing
                }
                    |> skipChange model
                    => []

            StartLine pos ->
                { editState | drawing = Just <| DrawLine <| DrawingLine pos }
                    |> logChange model
                    |> updateMouse images pos
                    => []

            AddLine startPos endPos ->
                { editState
                    | annotations = Array.push (Line_ <| Line startPos endPos strokeColor stroke strokeStyle) editState.annotations
                    , drawing = Nothing
                }
                    |> skipChange model
                    => []

            StartSpotlightRect pos ->
                { editState | drawing = Just <| DrawSpotlightRect <| DrawingRoundedRect pos }
                    |> logChange model
                    |> updateMouse images pos
                    => []

            AddSpotlightRect startPos endPos ->
                { editState
                    | annotations = Array.push (Rect_ <| Rect startPos endPos SpotlightFill strokeColor stroke strokeStyle True False) editState.annotations
                    , drawing = Nothing
                }
                    |> skipChange model
                    => []

            SetMouse { width, height } pos ->
                editState
                    |> updateDrawingIfRotating pos
                    |> skipChange model
                    |> setMouse pos
                    => []

            SetImages images ->
                { model | images = List.Zipper.fromList images }
                    => []

            KeyboardMsg keyMsg ->
                let
                    ( keyboardState, maybeKeyChange ) =
                        Keyboard.Extra.updateWithKeyChange keyMsg model.keyboardState
                in
                    { model | keyboardState = keyboardState }
                        |> alterDrawingsWithKeyboard maybeKeyChange
                        => []

            SelectImage image ->
                case model.images of
                    Just images ->
                        { model | images = List.Zipper.find ((==) image.url << .url) images, imageSelected = True }
                            => []

                    Nothing ->
                        model => []

            ChangeEditMode editMode ->
                { model | editMode = editMode }
                    |> updateLastDrawOption editMode
                    |> closeDropdown
                    => []

            SelectFill fill ->
                { model | fill = fill }
                    |> closeDropdown
                    => []

            SelectStrokeColor strokeColor ->
                { model | strokeColor = strokeColor }
                    |> closeDropdown
                    => []

            SelectLineStroke lineStroke ->
                { model | stroke = lineStroke }
                    |> closeDropdown
                    => []

            SelectStrokeStyle strokeStyle ->
                { model | strokeStyle = strokeStyle }
                    |> closeDropdown
                    => []

            SelectFontSize fontSize ->
                { model | fontSize = fontSize }
                    |> closeDropdown
                    => []

            ToggleDropdown editOption ->
                { editState
                    | drawing = updateDrawingWithFontSize editOption editState
                }
                    |> skipChange model
                    |> toggleDropdown editOption
                    => []

            CloseDropdown ->
                model
                    |> closeDropdown
                    => []

            ShowGrabHand ->
                { model
                    | movementState =
                        case model.movementState of
                            DrawingAnnotation ->
                                HoveringOverAnnotation

                            HoveringOverAnnotation ->
                                HoveringOverAnnotation

                            HoveringOverShownVertices ->
                                HoveringOverShownVertices

                            OutsideShownVertices ->
                                HoveringOverShownVertices

                            MovingAnnotation _ _ _ ->
                                model.movementState
                }
                    => []

            HideGrabHand ->
                { model
                    | movementState =
                        case model.movementState of
                            DrawingAnnotation ->
                                DrawingAnnotation

                            HoveringOverAnnotation ->
                                DrawingAnnotation

                            HoveringOverShownVertices ->
                                OutsideShownVertices

                            OutsideShownVertices ->
                                OutsideShownVertices

                            MovingAnnotation _ _ _ ->
                                model.movementState
                }
                    => []

            StartMovingAnnotation index annotation startPos ->
                { editState | annotations = removeAllVertices editState.annotations }
                    |> showVertices index annotation
                    |> skipChange model
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
                    |> hoverOverShownVertices
                    => []

            RemoveVertices ->
                { editState | annotations = Array.map removeVertices editState.annotations }
                    |> skipChange model
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


updateTextBoxWithNewFontSize : Drawing -> Drawing
updateTextBoxWithNewFontSize drawing =
    case drawing of
        DrawTextBox textMode ->
            DrawTextBox textMode

        _ ->
            drawing


updateDrawingWithFontSize : EditOption -> EditState -> Maybe Drawing
updateDrawingWithFontSize editOption editState =
    if editOption == Fonts then
        Maybe.map updateTextBoxWithNewFontSize editState.drawing
    else
        editState.drawing


updateLastDrawOption : EditMode -> Model -> Model
updateLastDrawOption editMode model =
    if List.member editMode shapeOptions then
        { model | lastShapeOption = editMode }
    else if List.member editMode lineOptions then
        { model | lastLineOption = editMode }
    else if List.member editMode spotlightShapeOptions then
        { model | lastSpotlightOption = editMode }
    else
        model


verticesAreShown : Model -> Model
verticesAreShown model =
    { model | movementState = HoveringOverShownVertices }


showVertices : Int -> Annotation -> EditState -> EditState
showVertices index newAnnotation editState =
    { editState | annotations = Array.set index newAnnotation editState.annotations }


removeAllVertices : Array Annotation -> Array Annotation
removeAllVertices annotations =
    Array.map removeVertices annotations


removeVertices : Annotation -> Annotation
removeVertices annotation =
    case annotation of
        Arrow_ arrow ->
            Arrow_ arrow

        Rect_ rect ->
            Rect_ { rect | showVertices = False }

        Ellipse_ ellipse ->
            Ellipse_ ellipse

        TextBox_ textBox ->
            TextBox_ textBox

        Line_ line ->
            Line_ line


startMovingAnnotation : Int -> Annotation -> StartPosition -> Model -> Model
startMovingAnnotation index annotation startPos model =
    { model | movementState = MovingAnnotation index annotation startPos }


moveAnnotation : Int -> Annotation -> StartPosition -> EndPosition -> EditState -> EditState
moveAnnotation index annotation oldPos newPos editState =
    { editState | annotations = Array.set index (move oldPos newPos annotation) editState.annotations }


move : Mouse.Position -> Mouse.Position -> Annotation -> Annotation
move oldPos newPos annotation =
    let
        dX =
            newPos.x - oldPos.x

        dY =
            newPos.y - oldPos.y
    in
        case annotation of
            Arrow_ arrow ->
                Arrow_ arrow

            Rect_ rect ->
                Rect_ { rect | start = shiftPosition dX dY rect.start, end = shiftPosition dX dY rect.end }

            Ellipse_ ellipse ->
                Ellipse_ ellipse

            TextBox_ textBox ->
                TextBox_ textBox

            Line_ line ->
                Line_ line


shiftPosition : Int -> Int -> Mouse.Position -> Mouse.Position
shiftPosition dx dy pos =
    { pos | x = pos.x + dx, y = pos.y + dy }


hoverOverAnnotation : Model -> Model
hoverOverAnnotation model =
    { model | movementState = HoveringOverAnnotation }


hoverOverShownVertices : Model -> Model
hoverOverShownVertices model =
    { model | movementState = HoveringOverShownVertices }


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


updateRotatingDrawing : Position -> Drawing -> Drawing
updateRotatingDrawing position drawing =
    case drawing of
        DrawTextBox textMode ->
            case textMode of
                RotatingText textBox ->
                    DrawTextBox <| RotatingText { textBox | angle = arrowAngle textBox.start position }

                _ ->
                    drawing

        _ ->
            drawing


updateDrawingIfRotating : Position -> EditState -> EditState
updateDrawingIfRotating position editState =
    Maybe.map (updateDrawing editState << updateRotatingDrawing position) editState.drawing
        |> Maybe.withDefault editState


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

        DrawRoundedRect rectMode ->
            DrawRoundedRect <|
                case rectMode of
                    DrawingRoundedRect start ->
                        DrawingRoundedSquare start

                    DrawingRoundedSquare start ->
                        DrawingRoundedRect start

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

        _ ->
            drawing


alterDrawingsWithKeyboard : Maybe KeyChange -> Model -> Model
alterDrawingsWithKeyboard maybeKeyChange model =
    let
        drawingUpdate =
            updateDrawing model.edits.present

        updateDrawingsOnShift editState =
            case editState.drawing of
                Just drawing ->
                    transitionOnShift drawing
                        |> drawingUpdate

                Nothing ->
                    editState
    in
        case maybeKeyChange of
            Just keyChange ->
                case keyChange of
                    KeyDown key ->
                        case key of
                            Keyboard.Extra.Shift ->
                                { model | edits = UndoList.mapPresent updateDrawingsOnShift model.edits }

                            _ ->
                                model

                    KeyUp key ->
                        case key of
                            Keyboard.Extra.Shift ->
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
viewImageAnnotator ({ edits, fill, strokeColor, mouse, keyboardState, currentDropdown, editMode, lastLineOption, lastShapeOption, lastSpotlightOption } as model) selectedImage =
    let
        { drawing } =
            edits.present

        toDropdownMenu =
            viewDropdownMenu currentDropdown editMode model
    in
        div
            [ Html.class "annotation-app" ]
            [ div [ Html.class "controls" ]
                [ viewButtonGroup [ viewLineDropdown editMode lastLineOption toDropdownMenu, viewShapeDropdown editMode lastShapeOption toDropdownMenu, viewTextSizeDropdown toDropdownMenu, viewSpotlightDropdown editMode lastSpotlightOption toDropdownMenu ]
                , viewButtonGroup [ viewFillDropdown toDropdownMenu fill, viewStrokeColorDropdown toDropdownMenu strokeColor, viewLineStrokeDropdown toDropdownMenu ]
                , viewHistoryControls
                , button [ onClick Export, Html.class "export-button" ] [ Html.text "Export" ]
                ]
            , viewCanvas model selectedImage
            ]


viewSpotlightDropdown : EditMode -> EditMode -> (EditOption -> Html Msg) -> Html Msg
viewSpotlightDropdown curEditMode lastSpotlightOption toDropdownMenu =
    div [ Html.class "dropdown-things" ]
        [ button
            [ onClick <| ChangeEditMode lastSpotlightOption
            , Html.classList [ "dropdown-button" => True, "dropdown-button--selected" => lastSpotlightOption == curEditMode ]
            ]
            [ viewRoundedRectangleIcon
            ]
        , button
            [ onClick <| ToggleDropdown SpotlightShapes
            , Html.classList [ "dropdown-arrow" => True, "dropdown-button--selected" => lastSpotlightOption == curEditMode ]
            ]
            [ viewDownArrow ]
        , toDropdownMenu SpotlightShapes
        ]


viewButtonGroup : List (Html Msg) -> Html Msg
viewButtonGroup buttons =
    div [ Html.class "button-group" ] buttons


viewHistoryControls : Html Msg
viewHistoryControls =
    div [ Html.class "history-controls" ]
        [ button [ onClick Undo, Html.class "history-button" ] [ viewUndoArrow ]
        , button [ onClick Redo, Html.class "history-button flip" ] [ viewUndoArrow ]
        ]


viewTextSizeDropdown : (EditOption -> Html Msg) -> Html Msg
viewTextSizeDropdown toDropdownMenu =
    div
        [ Html.class "dropdown-things"
        ]
        [ button
            [ onClick <| ToggleDropdown Fonts
            , Html.class "dropdown-button"
            ]
            [ viewTextIcon
            , viewDownArrow
            ]
        , toDropdownMenu Fonts
        ]


viewFontSizeOptions : Float -> Html Msg
viewFontSizeOptions fontSize =
    fontSizes
        |> List.map (viewFontSizeOption fontSize)
        |> div [ Html.class "dropdown-option" ]


viewFillOptions : Fill -> Html Msg
viewFillOptions fill =
    fillOptions
        |> List.map (viewFillOption fill)
        |> div [ Html.class "dropdown-option" ]


viewStrokeColorOptions : Color -> Html Msg
viewStrokeColorOptions strokeColor =
    strokeColorOptions
        |> List.map (viewStrokeColorOption strokeColor)
        |> div [ Html.class "dropdown-option" ]


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
            , viewDownArrow
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
            , viewDownArrow
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
            , viewDownArrow
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
            Shapes ->
                viewShapeOptions curEditMode

            Lines ->
                viewLineOptions curEditMode

            Fonts ->
                viewFontSizeOptions model.fontSize

            Fills ->
                viewFillOptions model.fill

            StrokeColors ->
                viewStrokeColorOptions model.strokeColor

            Strokes ->
                viewLineStrokeOptions model.stroke model.strokeStyle

            SpotlightShapes ->
                viewSpotlightShapeOptions curEditMode


viewSpotlightShapeOptions : EditMode -> Html Msg
viewSpotlightShapeOptions curEditMode =
    spotlightShapeOptions
        |> List.map (viewSpotlightShapeOption curEditMode)
        |> div [ Html.class "dropdown-option" ]


viewSpotlightShapeOption : EditMode -> EditMode -> Html Msg
viewSpotlightShapeOption curEditMode editMode =
    button
        [ Html.classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => curEditMode == editMode
            ]
        , onClick <| ChangeEditMode editMode
        ]
        [ viewShapeSvg editMode ]


viewShapeDropdown : EditMode -> EditMode -> (EditOption -> Html Msg) -> Html Msg
viewShapeDropdown curEditMode lastShapeOption toDropdownMenu =
    div
        [ Html.class "dropdown-things" ]
        [ button
            [ onClick <| ChangeEditMode lastShapeOption
            , Html.classList [ "dropdown-button" => True, "dropdown-button--selected" => lastShapeOption == curEditMode ]
            ]
            [ viewShapeSvg lastShapeOption
            ]
        , button
            [ onClick <| ToggleDropdown Shapes
            , Html.classList [ "dropdown-arrow" => True, "dropdown-button--selected" => lastShapeOption == curEditMode ]
            ]
            [ viewDownArrow ]
        , toDropdownMenu Shapes
        ]


viewLineDropdown : EditMode -> EditMode -> (EditOption -> Html Msg) -> Html Msg
viewLineDropdown curEditMode lastLineOption toDropdownMenu =
    div
        [ Html.class "dropdown-things" ]
        [ button
            [ onClick <| ChangeEditMode lastLineOption
            , Html.classList
                [ "dropdown-button" => True
                , "dropdown-button--selected" => lastLineOption == curEditMode
                ]
            ]
            [ viewShapeSvg lastLineOption ]
        , button
            [ onClick <| ToggleDropdown Lines
            , Html.classList [ "dropdown-arrow" => True, "dropdown-button--selected" => lastLineOption == curEditMode ]
            ]
            [ viewDownArrow ]
        , toDropdownMenu Lines
        ]


viewShapeOptions : EditMode -> Html Msg
viewShapeOptions curEditMode =
    shapeOptions
        |> List.map (viewShapeOption curEditMode)
        |> div [ Html.class "dropdown-option" ]


viewShapeOption : EditMode -> EditMode -> Html Msg
viewShapeOption curEditMode editMode =
    button
        [ Html.classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => curEditMode == editMode
            ]
        , onClick <| ChangeEditMode editMode
        ]
        [ viewShapeSvg editMode ]


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


viewLineOptions : EditMode -> Html Msg
viewLineOptions curEditMode =
    lineOptions
        |> List.map (viewShapeOption curEditMode)
        |> div [ Html.class "dropdown-option" ]


viewLineStrokeOptions : LineStroke -> StrokeStyle -> Html Msg
viewLineStrokeOptions strokeWidth strokeStyle =
    [ List.map (viewLineStrokeOption strokeWidth) lineStrokeOptions
    , List.map (viewStrokeStyleOption strokeStyle) strokeStyles
    ]
        |> List.concat
        |> div [ Html.class "dropdown-option" ]


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
                    [ onMouseUp <| Json.map (AddRect startPos << toDrawingPosition) Mouse.position ]

                DrawingSquare startPos ->
                    [ onMouseUp <| Json.map (AddRect startPos << circleMouse startPos << toDrawingPosition) Mouse.position ]

        DrawRoundedRect rectMode ->
            case rectMode of
                DrawingRoundedRect startPos ->
                    [ onMouseUp <| Json.map (AddRoundedRect startPos << toDrawingPosition) Mouse.position ]

                DrawingRoundedSquare startPos ->
                    [ onMouseUp <| Json.map (AddRoundedRect startPos << circleMouse startPos << toDrawingPosition) Mouse.position ]

        DrawArrow arrowMode ->
            case arrowMode of
                DrawingArrow startPos ->
                    [ onMouseUp <| Json.map (AddArrow startPos << toDrawingPosition) Mouse.position ]

                DrawingDiscreteArrow startPos ->
                    [ onMouseUp <| Json.map (AddArrow startPos << stepMouse startPos << toDrawingPosition) Mouse.position ]

        DrawEllipse ellipseDrawing ->
            case ellipseDrawing of
                DrawingOval startPos ->
                    [ onMouseUp <| Json.map (AddEllipse startPos << toDrawingPosition) Mouse.position ]

                DrawingCircle startPos ->
                    [ onMouseUp <| Json.map (AddEllipse startPos << circleMouse startPos << toDrawingPosition) Mouse.position ]

        DrawTextBox textBoxDrawing ->
            case textBoxDrawing of
                DrawingTextBox start ->
                    [ onMouseUp (Json.map (PlaceTextBox start << toDrawingPosition) Mouse.position) ]

                EditingText ({ start, end, text, angle } as textBox) ->
                    [ -- if mouseIsOverRotateButton (rotateButtonPosition start end) (toPos curMouse) then
                      -- Html.Events.onMouseDown <| BeginRotatingTextBox 0 textBox
                      if text == "" then
                        onClick Undo
                      else
                        onClick <| AddTextBox textBox
                    ]

                RotatingText ({ start } as textBox) ->
                    [ onMouseUp <| Json.map (FinishRotatingTextBox textBox << arrowAngle start << toDrawingPosition) Mouse.position ]

        DrawLine lineDrawing ->
            case lineDrawing of
                DrawingLine startPos ->
                    [ onMouseUp (Json.map (AddLine startPos << toDrawingPosition) Mouse.position) ]

                DrawingDiscreteLine startPos ->
                    [ onMouseUp <| Json.map (AddLine startPos << stepMouse startPos << toDrawingPosition) Mouse.position ]

        DrawSpotlightRect roundedRectMode ->
            case roundedRectMode of
                DrawingRoundedRect startPos ->
                    [ onMouseUp (Json.map (AddSpotlightRect startPos << toDrawingPosition) Mouse.position) ]

                DrawingRoundedSquare startPos ->
                    [ onMouseUp (Json.map (AddSpotlightRect startPos << circleMouse startPos << toDrawingPosition) Mouse.position) ]


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
                    DrawingAnnotation ->
                        drawingStateEvents model.editMode editState.drawing model.mouse

                    HoveringOverAnnotation ->
                        []

                    HoveringOverShownVertices ->
                        []

                    OutsideShownVertices ->
                        onClick RemoveVertices :: drawingStateEvents model.editMode editState.drawing model.mouse

                    MovingAnnotation _ _ _ ->
                        []

        toDrawing =
            case editState.drawing of
                Just drawing ->
                    viewDrawing image.width image.height drawing model

                Nothing ->
                    (\_ -> [ Svg.text "" ])

        annotations =
            editState.annotations
                |> Array.toList
                |> List.indexedMap (viewAnnotation image.width image.height model.movementState)
                |> List.concat

        spotlights =
            editState.annotations
                |> Array.filter isSpotlightShape
                |> Array.map spotlightFillToMaskFill
                |> Array.toList
                |> List.indexedMap (viewAnnotation image.width image.height model.movementState)
                |> List.concat

        cutOuts =
            if isDrawingSpotlight editState.drawing then
                spotlights ++ toDrawing True
            else
                spotlights

        definitions =
            List.map viewArrowHeadDefinition strokeColorOptions
                |> (::) (viewMask model.movementState image.width image.height cutOuts)
                |> defs []
                |> List.singleton

        mask =
            [ rect [ x "0", y "0", Attr.height <| toString image.height, Attr.width <| toString image.width, Attr.mask "url(#Mask)" ] [] ]

        firstSpotlightIndex =
            editState.annotations
                |> Array.toList
                |> List.Extra.findIndex isSpotlightShape
                |> Maybe.withDefault 0

        svgChildren =
            if List.isEmpty spotlights && not (isDrawingSpotlight editState.drawing) then
                annotations ++ definitions ++ toDrawing False
            else if isDrawingSpotlight editState.drawing then
                definitions ++ (List.take firstSpotlightIndex annotations) ++ mask ++ (List.drop firstSpotlightIndex annotations) ++ toDrawing False
            else
                definitions ++ (List.take firstSpotlightIndex annotations) ++ mask ++ (List.drop firstSpotlightIndex annotations) ++ toDrawing False

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


viewAnnotation : Float -> Float -> MovementState -> Int -> Annotation -> List (Svg Msg)
viewAnnotation width height movementState index annotation =
    case annotation of
        Arrow_ arrow ->
            viewArrow arrow

        Rect_ rect ->
            viewRect
                (case movementState of
                    MovingAnnotation index annotation startPos ->
                        [ SE.on "mouseup" <| Json.map (FinishMovingAnnotation index annotation startPos << toDrawingPosition) Mouse.position
                        ]

                    _ ->
                        [ SE.on "mousedown" <| Json.map (StartMovingAnnotation index (Rect_ { rect | showVertices = True }) << toDrawingPosition) Mouse.position
                        , SE.onMouseOver ShowGrabHand
                        , SE.onMouseOut HideGrabHand
                        ]
                )
                rect

        Ellipse_ ellipse ->
            viewEllipse ellipse

        Line_ line ->
            viewLine line

        TextBox_ textBox ->
            viewTextBox textBox


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
            ++ case movementState of
                HoveringOverAnnotation ->
                    [ onMouseEnter HideGrabHand ]

                _ ->
                    []
        )
        []
        :: shapes
        |> Svg.mask [ Attr.id "Mask" ]


viewDrawing : Float -> Float -> Drawing -> Model -> Bool -> List (Svg Msg)
viewDrawing width height drawing { fill, strokeColor, stroke, strokeStyle, fontSize, mouse, keyboardState } isInMask =
    case drawing of
        DrawRect rectMode ->
            case rectMode of
                DrawingRect startPos ->
                    Rect startPos mouse fill strokeColor stroke strokeStyle False False
                        |> viewRect []

                DrawingSquare startPos ->
                    Rect startPos (circleMouse startPos mouse) fill strokeColor stroke strokeStyle False False
                        |> viewRect []

        DrawRoundedRect rectMode ->
            case rectMode of
                DrawingRoundedRect startPos ->
                    Rect startPos mouse fill strokeColor stroke strokeStyle True False
                        |> viewRect []

                DrawingRoundedSquare startPos ->
                    Rect startPos (circleMouse startPos mouse) fill strokeColor stroke strokeStyle True False
                        |> viewRect []

        DrawArrow arrowDrawing ->
            case arrowDrawing of
                DrawingArrow pos ->
                    Arrow pos mouse strokeColor stroke strokeStyle
                        |> viewArrow

                DrawingDiscreteArrow pos ->
                    Arrow pos (stepMouse pos mouse) strokeColor stroke strokeStyle
                        |> viewArrow

        DrawEllipse ellipseDrawing ->
            case ellipseDrawing of
                DrawingOval pos ->
                    Ellipse pos mouse fill strokeColor stroke strokeStyle
                        |> viewEllipse

                DrawingCircle pos ->
                    Ellipse pos (circleMouse pos mouse) fill strokeColor stroke strokeStyle
                        |> viewEllipse

        DrawTextBox textBoxDrawing ->
            case textBoxDrawing of
                DrawingTextBox pos ->
                    TextBox pos mouse "" strokeColor stroke fontSize 0
                        |> viewTextBoxWithBorder

                EditingText { start, end, text, angle } ->
                    TextBox start end text strokeColor stroke fontSize angle
                        |> viewTextInputBox

                RotatingText { start, end, text, angle } ->
                    TextBox start end text strokeColor stroke fontSize angle
                        |> viewTextBoxWithRotateButton

        DrawLine lineMode ->
            case lineMode of
                DrawingLine pos ->
                    Line pos mouse strokeColor stroke strokeStyle
                        |> viewLine

                DrawingDiscreteLine pos ->
                    Line pos (stepMouse pos mouse) strokeColor stroke strokeStyle
                        |> viewLine

        DrawSpotlightRect rectMode ->
            let
                ( rectFill, rectStrokeColor ) =
                    if isInMask then
                        ( MaskFill, Color.white )
                    else
                        ( EmptyFill, strokeColor )
            in
                case rectMode of
                    DrawingRoundedRect startPos ->
                        Rect startPos mouse rectFill rectStrokeColor stroke strokeStyle True False
                            |> viewRect []

                    DrawingRoundedSquare startPos ->
                        Rect startPos (circleMouse startPos mouse) rectFill rectStrokeColor stroke strokeStyle True False
                            |> viewRect []


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


viewRect : List (Svg.Attribute Msg) -> Rect -> List (Svg Msg)
viewRect attrs ({ start, end, fill, strokeColor, stroke, rounded } as rect) =
    let
        strokeStyle =
            [ toLineStyle rect.strokeStyle ]
                ++ if rounded then
                    [ rx "15", ry "15" ]
                   else
                    []

        vertices =
            [ viewVertex start.x start.y
            , viewVertex end.x start.y
            , viewVertex start.x end.y
            , viewVertex end.x end.y
            ]
    in
        [ Svg.rect
            ([ Attr.width <| toString <| abs <| end.x - start.x
             , Attr.height <| toString <| abs <| end.y - start.y
             , x <| toString <| Basics.min start.x end.x
             , y <| toString <| Basics.min start.y end.y
             , strokeWidth <| toString <| strokeToWidth stroke
             , Attr.stroke <| Color.Convert.colorToHex strokeColor
             ]
                ++ fillStyle fill
                ++ strokeStyle
                ++ attrs
            )
            []
        ]
            ++ if rect.showVertices then
                vertices
               else
                []


viewVertex : Int -> Int -> Svg Msg
viewVertex x y =
    circle
        [ cx <| toString x
        , cy <| toString y
        , r "7"
        , fill <| Color.Convert.colorToHex Color.blue
        ]
        []


viewArrow : Arrow -> List (Svg Msg)
viewArrow ({ start, end, fill, stroke, strokeStyle } as arrow) =
    [ Svg.path
        [ markerEnd <| "url(#arrow-head--" ++ Color.Convert.colorToHex fill ++ ")"
        , strokeWidth <| toString <| strokeToWidth stroke
        , toLineStyle strokeStyle
        , Attr.fill "none"
        , Attr.stroke <| Color.Convert.colorToHex fill
        , d <| "M" ++ toString start.x ++ "," ++ toString start.y ++ " l" ++ toString (end.x - start.x) ++ "," ++ toString (end.y - start.y)
        ]
        []
    ]


viewEllipse : Ellipse -> List (Svg Msg)
viewEllipse ({ start, end, fill, strokeColor, stroke, strokeStyle } as ellipse) =
    [ Svg.ellipse
        ([ rx <| toString <| abs <| end.x - start.x
         , ry <| toString <| abs <| end.y - start.y
         , cx <| toString <| start.x
         , cy <| toString <| start.y
         , Attr.strokeWidth <| toString <| strokeToWidth stroke
         , Attr.stroke <| Color.Convert.colorToHex strokeColor
         , toLineStyle strokeStyle
         ]
            ++ fillStyle fill
        )
        []
    ]


viewText : TextBox -> Svg Msg
viewText ({ start, end, text, fill, fontSize, angle } as textBox) =
    text_
        [ x <| toString <| start.x
        , y <| toString <| start.y
        , Attr.fontSize <| toString fontSize
        , Attr.stroke <| toString fill
        ]
        [ Svg.text text ]


viewTextBoxBorder : StartPosition -> EndPosition -> List (Svg Msg)
viewTextBoxBorder start end =
    viewRect [] <| Rect start end EmptyFill Color.black Thin Dashed False False


viewRotateButton : StartPosition -> EndPosition -> Html Msg
viewRotateButton start end =
    circle
        [ cx <| toString <| start.x + ((end.x - start.x) // 2)
        , cy <| toString <| start.y - ((end.y - start.y) // 2)
        , r <| toString <| 7
        ]
        []


viewTextBox : TextBox -> List (Svg Msg)
viewTextBox ({ start, end, text, fill, fontSize, angle } as textBox) =
    [ viewText textBox ]


viewTextBoxWithBorder : TextBox -> List (Svg Msg)
viewTextBoxWithBorder ({ start, end, text, fill, fontSize, angle } as textBox) =
    viewTextBoxBorder start end
        ++ [ viewText textBox
           ]


viewTextBoxWithRotateButton : TextBox -> List (Svg Msg)
viewTextBoxWithRotateButton ({ start, end, text, fill, fontSize, angle } as textBox) =
    viewTextBoxBorder start end
        ++ [ viewRotateButton start end
           , viewText textBox
           ]


viewTextInputBox : TextBox -> List (Svg Msg)
viewTextInputBox ({ start, end, text, fill, fontSize, angle } as textBox) =
    viewTextBoxBorder start end
        ++ [ viewRotateButton start end
           , foreignObject
                []
                [ Html.input
                    [ Html.id "text-box-edit"
                    , Html.class "text-box--edit"
                    , onInput <| TextBoxInput textBox
                    , Html.value text
                    , Html.style [ "top" => toString start.y ++ "px", "left" => toString start.x ++ "px" ]
                    ]
                    []
                ]
           ]


viewLine : Line -> List (Svg Msg)
viewLine { start, end, fill, stroke, strokeStyle } =
    [ Svg.path
        [ strokeWidth <| toString <| strokeToWidth stroke
        , Attr.fill "none"
        , Attr.stroke <| Color.Convert.colorToHex fill
        , d <| "M" ++ toString start.x ++ "," ++ toString start.y ++ " l" ++ toString (end.x - start.x) ++ "," ++ toString (end.y - start.y)
        ]
        []
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
                    [ onMouseEnter HideGrabHand ]

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
        [ g [ fill "grey" ]
            [ Svg.path [ d "M26.105,21.891c-0.229,0-0.439-0.131-0.529-0.346l0,0c-0.066-0.156-1.716-3.857-7.885-4.59c-1.285-0.156-2.824-0.236-4.693-0.25v4.613c0,0.213-0.115,0.406-0.304,0.508c-0.188,0.098-0.413,0.084-0.588-0.033L0.254,13.815C0.094,13.708,0,13.528,0,13.339c0-0.191,0.094-0.365,0.254-0.477l11.857-7.979c0.175-0.121,0.398-0.129,0.588-0.029c0.19,0.102,0.303,0.295,0.303,0.502v4.293c2.578,0.336,13.674,2.33,13.674,11.674c0,0.271-0.191,0.508-0.459,0.562C26.18,21.891,26.141,21.891,26.105,21.891z" ] []
            ]
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


handleTextBoxInputKey : Msg -> KeyCode -> Result String Msg
handleTextBoxInputKey submitMsg code =
    if code == 13 then
        Ok submitMsg
    else if code == 27 then
        Ok Undo
    else
        Err "not handling that key"


fromKeyResult : Result String Msg -> Json.Decoder Msg
fromKeyResult result =
    case result of
        Ok msg ->
            Json.succeed msg

        Err errMsg ->
            Json.fail errMsg


decodeTextInputKey : Msg -> Json.Decoder Msg
decodeTextInputKey submitMsg =
    Json.map (handleTextBoxInputKey submitMsg) keyCode
        |> Json.andThen fromKeyResult


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


drawingToEditMode : Drawing -> EditMode
drawingToEditMode drawing =
    case drawing of
        DrawRect _ ->
            EditRect

        DrawRoundedRect _ ->
            EditRoundedRect

        DrawArrow _ ->
            EditArrow

        DrawEllipse _ ->
            EditEllipse

        DrawTextBox _ ->
            EditTextBox

        DrawLine _ ->
            EditLine

        DrawSpotlightRect roundedRectMode ->
            EditSpotlightRect


trackMouse : Drawing -> Bool
trackMouse drawing =
    case drawing of
        DrawArrow arrowMode ->
            True

        DrawRoundedRect rectMode ->
            True

        DrawRect rectMode ->
            True

        DrawEllipse ellipseMode ->
            True

        DrawTextBox textMode ->
            case textMode of
                EditingText _ ->
                    False

                _ ->
                    True

        DrawLine lineMode ->
            True

        DrawSpotlightRect roundedRectMode ->
            True


toDeltas : Float -> Float -> Position
toDeltas h theta =
    Position (round (cos theta * h)) (round (sin theta * h))


halfDistance : Position -> Position -> Position
halfDistance start end =
    let
        ( dx, dy ) =
            ( end.x - start.x, end.y - start.y )
    in
        Position (dx // 2) (dy // 2)


calcDistance : Position -> Position -> Float
calcDistance a b =
    sqrt <| toFloat <| (b.x - a.x) ^ 2 + (b.y - b.x) ^ 2


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


circleMouse : StartPosition -> EndPosition -> EndPosition
circleMouse a b =
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


mouseIsOverRotateButton : ( Float, Float ) -> ( Float, Float ) -> Bool
mouseIsOverRotateButton ( x1, y1 ) ( x2, y2 ) =
    abs (x1 - x2) < 10 && (abs y2 - y1) < 10


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
    { mouse | x = mouse.x - 10, y = mouse.y - 64 }


movementStateToCursor : MovementState -> String
movementStateToCursor movementState =
    case movementState of
        DrawingAnnotation ->
            "crosshair"

        HoveringOverAnnotation ->
            "pointer"

        HoveringOverShownVertices ->
            "move"

        OutsideShownVertices ->
            "crosshair"

        MovingAnnotation _ _ _ ->
            "move"



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
                if Maybe.withDefault False <| Maybe.map trackMouse model.edits.present.drawing then
                    Mouse.moves (SetMouse (List.Zipper.current images) << toDrawingPosition)
                else
                    case model.movementState of
                        MovingAnnotation index annotation startPos ->
                            Mouse.moves (MoveAnnotation index annotation startPos << toDrawingPosition)

                        _ ->
                            Sub.none
        , Sub.map KeyboardMsg Keyboard.Extra.subscriptions
        , setImages SetImages
        ]



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = Rocket.batchInit init
        , update = update >> Rocket.batchUpdate
        , view = view
        , subscriptions = subscriptions
        }
