port module Annotator exposing (..)

import Char exposing (KeyCode)
import Collage exposing (collage, filled, move, rotate, toForm)
import Color exposing (Color)
import Dom
import Element exposing (toHtml)
import Html exposing (Attribute, Html, button, div, p, text)
import Html.Attributes exposing (class, classList, height, id, src, start, style, type_, width)
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput, onWithOptions)
import Json.Decode as Json
import Keyboard.Extra exposing (Key, KeyChange(..))
import List.Zipper exposing (Zipper)
import Mouse
import Rocket exposing ((=>))
import Svgs exposing (viewDownArrow, viewRectangleIcon)
import Task exposing (succeed)
import Text
import UndoList exposing (UndoList)


-- MODEL


type alias Position =
    ( Float, Float )


type alias StartPosition =
    Position


type alias EndPosition =
    Position


type ArrowMode
    = NoArrow
    | DrawingArrow StartPosition
    | DrawingDiscreteArrow StartPosition


type RectMode
    = NoRect
    | DrawingRect StartPosition
    | DrawingSquare StartPosition


type RoundedRectMode
    = NoRoundedRect
    | DrawingRoundedRect StartPosition
    | DrawingRoundedSquare StartPosition


type EllipseMode
    = NoEllipse
    | DrawingOval StartPosition
    | DrawingCircle StartPosition


type TextMode
    = NoText
    | DrawingTextBox StartPosition
    | EditingText TextBox
    | RotatingText TextBox


type LineMode
    = NoLine
    | DrawingLine StartPosition
    | DrawingDiscreteLine StartPosition


type Drawing
    = DrawArrow ArrowMode
    | DrawRect RectMode
    | DrawRoundedRect RoundedRectMode
    | DrawEllipse EllipseMode
    | DrawTextBox TextMode
    | DrawLine LineMode
    | DrawSpotlightRect RoundedRectMode
    | Selection


type EditMode
    = EditRect
    | EditRoundedRect
    | EditOval
    | EditArrow
    | EditLine
    | EditTextBox
    | EditSpotlightRect
    | Select


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
    }


type Fill
    = SolidFill Color
    | EmptyFill
    | SpotlightFill


type alias Ellipse =
    { start : Position
    , end : Position
    , fill : Color
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
    | Strokes
    | SpotlightShapes


type Annotation
    = Arrow_ Arrow
    | Rect_ Rect
    | Ellipse_ Ellipse
    | TextBox_ TextBox
    | Line_ Line
    | Mask_


type alias EditState =
    { annotations : List Annotation
    , drawing : Drawing
    , fill : Fill
    , strokeColor : Color
    , stroke : LineStroke
    , strokeStyle : StrokeStyle
    , fontSize : Float
    , showMask : Bool
    }


type alias Model =
    { edits : UndoList EditState
    , mouse : Mouse.Position
    , keyboardState : Keyboard.Extra.State
    , images : Maybe (Zipper Image)
    , imageSelected : Bool
    , currentDropdown : Maybe EditOption
    }


fillOptions : List Color
fillOptions =
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
    , EditOval
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
    { annotations = []
    , drawing = Selection
    , fill = EmptyFill
    , strokeColor = Color.red
    , stroke = Medium
    , strokeStyle = Solid
    , fontSize = 14
    , showMask = False
    }


init : ( Model, List (Cmd Msg) )
init =
    { edits = UndoList.fresh initialEditState
    , mouse = Mouse.Position 0 0
    , keyboardState = Keyboard.Extra.initialState
    , images = List.Zipper.fromList []
    , imageSelected = False
    , currentDropdown = Nothing
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
    | ChangeDrawing Drawing
    | SelectFill Fill
    | SelectStrokeColor Color
    | SelectLineStroke LineStroke
    | SelectStrokeStyle StrokeStyle
    | SelectFontSize Float
    | ToggleDropdown EditOption
    | CloseDropdown
    | Undo
    | Redo
    | Export


update : Msg -> Model -> ( Model, List (Cmd Msg) )
update msg ({ edits, mouse, images } as model) =
    let
        editState =
            edits.present

        { fill, fontSize, stroke, strokeColor, strokeStyle } =
            editState
    in
        case msg of
            StartRect pos ->
                { editState | drawing = DrawRect <| DrawingRect pos }
                    |> logChange model
                    |> updateMouse images pos
                    => []

            AddRect start end ->
                { editState
                    | annotations = Rect_ (Rect start end fill strokeColor stroke strokeStyle False) :: editState.annotations
                    , drawing = DrawRect <| NoRect
                }
                    |> logChange model
                    => []

            StartRoundedRect pos ->
                { editState | drawing = DrawRoundedRect <| DrawingRoundedRect pos }
                    |> logChange model
                    |> updateMouse images pos
                    => []

            AddRoundedRect start end ->
                { editState
                    | annotations = Rect_ (Rect start end fill strokeColor stroke strokeStyle True) :: editState.annotations
                    , drawing = DrawRoundedRect <| NoRoundedRect
                }
                    |> logChange model
                    => []

            StartArrow pos ->
                { editState | drawing = DrawArrow (DrawingArrow pos) }
                    |> logChange model
                    |> updateMouse images pos
                    => []

            AddArrow startPos endPos ->
                { editState
                    | annotations = Arrow_ (Arrow startPos endPos strokeColor stroke strokeStyle) :: editState.annotations
                    , drawing = DrawArrow NoArrow
                }
                    |> skipChange model
                    => []

            StartEllipse pos ->
                { editState | drawing = DrawEllipse <| DrawingOval pos }
                    |> logChange model
                    |> updateMouse images pos
                    => []

            AddEllipse startPos endPos ->
                { editState
                    | annotations = Ellipse_ (Ellipse startPos endPos strokeColor stroke strokeStyle) :: editState.annotations
                    , drawing = DrawEllipse NoEllipse
                }
                    |> skipChange model
                    => []

            StartTextBox pos ->
                { editState | drawing = DrawTextBox (DrawingTextBox pos) }
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
                    { editState | drawing = DrawTextBox <| EditingText initialEditState }
                        |> skipChange model
                        => [ Dom.focus "text-box-edit"
                                |> Task.attempt tryToEdit
                           ]

            TextBoxInput { start, end, angle } text ->
                { editState | drawing = DrawTextBox <| EditingText <| TextBox start end text strokeColor stroke fontSize angle }
                    |> skipChange model
                    => []

            BeginRotatingTextBox angle { start, end, text } ->
                { editState | drawing = DrawTextBox <| RotatingText <| TextBox start end text strokeColor stroke fontSize angle }
                    |> skipChange model
                    => []

            FinishRotatingTextBox { start, end, text } angle ->
                { editState | drawing = DrawTextBox <| EditingText <| TextBox start end text strokeColor stroke fontSize angle }
                    |> skipChange model
                    => []

            AddTextBox { start, end, text, angle } ->
                { editState
                    | annotations = TextBox_ (TextBox start end text strokeColor stroke fontSize angle) :: editState.annotations
                    , drawing = DrawTextBox NoText
                }
                    |> skipChange model
                    => []

            StartLine pos ->
                { editState | drawing = DrawLine <| DrawingLine pos }
                    |> logChange model
                    |> updateMouse images pos
                    => []

            AddLine startPos endPos ->
                { editState
                    | annotations = Line_ (Line startPos endPos strokeColor stroke strokeStyle) :: editState.annotations
                    , drawing = DrawLine NoLine
                }
                    |> skipChange model
                    => []

            StartSpotlightRect pos ->
                { editState | drawing = DrawSpotlightRect <| DrawingRoundedRect pos }
                    |> logChange model
                    |> updateMouse images pos
                    => []

            AddSpotlightRect startPos endPos ->
                let
                    newEditState =
                        if editState.showMask then
                            { editState | annotations = Rect_ (Rect startPos endPos SpotlightFill strokeColor stroke strokeStyle True) :: editState.annotations, drawing = DrawSpotlightRect NoRoundedRect }
                        else
                            { editState | annotations = Rect_ (Rect startPos endPos SpotlightFill strokeColor stroke strokeStyle True) :: Mask_ :: editState.annotations, showMask = True, drawing = DrawSpotlightRect NoRoundedRect }
                in
                    newEditState
                        |> skipChange model
                        => []

            SetMouse { width, height } pos ->
                editState
                    |> updateDrawingIfRotating (toPosition width height pos)
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

            ChangeDrawing drawing ->
                { editState | drawing = drawing }
                    |> skipChange model
                    |> closeDropdown
                    => []

            SelectFill fill ->
                { editState | fill = fill }
                    |> skipChange model
                    |> closeDropdown
                    => []

            SelectStrokeColor strokeColor ->
                { editState | strokeColor = strokeColor }
                    |> skipChange model
                    |> closeDropdown
                    => []

            SelectLineStroke lineStroke ->
                { editState | stroke = lineStroke }
                    |> skipChange model
                    |> closeDropdown
                    => []

            SelectStrokeStyle strokeStyle ->
                { editState | strokeStyle = strokeStyle }
                    |> skipChange model
                    |> closeDropdown
                    => []

            SelectFontSize fontSize ->
                { editState | fontSize = fontSize }
                    |> skipChange model
                    |> closeDropdown
                    => []

            ToggleDropdown editOption ->
                { editState
                    | drawing =
                        if editOption == Fonts then
                            case editState.drawing of
                                DrawTextBox textMode ->
                                    DrawTextBox textMode

                                _ ->
                                    DrawTextBox NoText
                        else
                            editState.drawing
                }
                    |> skipChange model
                    |> toggleDropdown editOption
                    => []

            CloseDropdown ->
                model
                    |> closeDropdown
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

                pos =
                    fromPosition width height mouse
            in
                { model | mouse = pos }


setMouse : Mouse.Position -> Model -> Model
setMouse mouse model =
    { model | mouse = mouse }


updateDrawing : EditState -> Drawing -> EditState
updateDrawing editState drawing =
    { editState | drawing = drawing }


updateDrawingIfRotating : Position -> EditState -> EditState
updateDrawingIfRotating position editState =
    case editState.drawing of
        DrawTextBox textMode ->
            case textMode of
                RotatingText textBox ->
                    { editState | drawing = DrawTextBox <| RotatingText { textBox | angle = arrowAngle textBox.start position } }

                _ ->
                    editState

        _ ->
            editState


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

                    _ ->
                        rectMode

        DrawRoundedRect rectMode ->
            DrawRoundedRect <|
                case rectMode of
                    DrawingRoundedRect start ->
                        DrawingRoundedSquare start

                    DrawingRoundedSquare start ->
                        DrawingRoundedRect start

                    _ ->
                        rectMode

        DrawArrow arrowMode ->
            DrawArrow <|
                case arrowMode of
                    DrawingArrow startPos ->
                        DrawingDiscreteArrow startPos

                    DrawingDiscreteArrow startPos ->
                        DrawingArrow startPos

                    _ ->
                        arrowMode

        DrawEllipse ellipseMode ->
            DrawEllipse <|
                case ellipseMode of
                    DrawingOval startPos ->
                        DrawingCircle startPos

                    DrawingCircle startPos ->
                        DrawingOval startPos

                    _ ->
                        ellipseMode

        DrawLine lineMode ->
            DrawLine <|
                case lineMode of
                    DrawingLine startPos ->
                        DrawingDiscreteLine startPos

                    DrawingDiscreteLine startPos ->
                        DrawingLine startPos

                    _ ->
                        lineMode

        _ ->
            drawing


alterDrawingsWithKeyboard : Maybe KeyChange -> Model -> Model
alterDrawingsWithKeyboard maybeKeyChange model =
    let
        drawingUpdate =
            updateDrawing model.edits.present
    in
        case maybeKeyChange of
            Just keyChange ->
                case keyChange of
                    KeyDown key ->
                        case key of
                            Keyboard.Extra.Shift ->
                                { model | edits = UndoList.mapPresent (drawingUpdate << transitionOnShift << .drawing) model.edits }

                            _ ->
                                model

                    KeyUp key ->
                        case key of
                            Keyboard.Extra.Shift ->
                                { model | edits = UndoList.mapPresent (drawingUpdate << transitionOnShift << .drawing) model.edits }

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
        |> div [ class "image-selector" ]


viewImageOption : Zipper Image -> Image -> Image -> Html Msg
viewImageOption zipper highlightedImage image =
    div
        [ class "image-option"
        , width <| round image.width
        , height <| round image.height
        , onClick <| SelectImage image
        ]
        [ Html.img [ src image.url, height <| round image.height, width <| round image.width ] []
        ]


viewInfoScreen : Html Msg
viewInfoScreen =
    div []
        [ text "please upload an image!" ]


viewImageAnnotator : Model -> Image -> Html Msg
viewImageAnnotator ({ edits, mouse, keyboardState, currentDropdown } as model) selectedImage =
    let
        editState =
            edits.present

        toDropdownMenu =
            viewDropdownMenu currentDropdown editState
    in
        div [ id "annotation-app" ]
            [ viewCanvas editState mouse keyboardState selectedImage
            , viewControls toDropdownMenu editState.strokeColor
            , viewOffscreenInput editState.drawing
            ]


viewOffscreenInput : Drawing -> Html Msg
viewOffscreenInput drawing =
    let
        options =
            { preventDefault = True, stopPropagation = False }
    in
        case drawing of
            DrawTextBox textBoxDrawing ->
                case textBoxDrawing of
                    EditingText textBox ->
                        Html.input
                            [ id "text-box-edit"
                            , onInput <| TextBoxInput textBox
                            , onWithOptions "keydown" options (decodeTextInputKey <| AddTextBox textBox)
                            , class "hidden-input"
                            ]
                            [ Html.text textBox.text ]

                    _ ->
                        text ""

            _ ->
                text ""


viewControls : (EditOption -> Html Msg) -> Color -> Html Msg
viewControls toDropdownMenu strokeColor =
    div [ class "controls" ]
        [ viewButtonGroup [ viewLineDropdown toDropdownMenu, viewShapeDropdown toDropdownMenu, viewTextSizeDropdown toDropdownMenu, viewSpotlightDropdown toDropdownMenu ]
        , viewButtonGroup [ viewFillDropdown toDropdownMenu strokeColor, viewLineStrokeDropdown toDropdownMenu ]
        , viewHistoryControls
        , button [ onClick Export, class "export-button" ] [ text "Export" ]
        ]


viewSpotlightDropdown : (EditOption -> Html Msg) -> Html Msg
viewSpotlightDropdown toDropdownMenu =
    div [ class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown SpotlightShapes
            , class "dropdown-button"
            ]
            [ Svgs.viewRoundedRectangleIcon
            , Svgs.viewDownArrow
            ]
        , toDropdownMenu SpotlightShapes
        ]


viewButtonGroup : List (Html Msg) -> Html Msg
viewButtonGroup buttons =
    div [ class "button-group" ] buttons


viewHistoryControls : Html Msg
viewHistoryControls =
    div [ class "history-controls" ]
        [ button [ onClick Undo, class "history-button" ] [ Svgs.viewUndoArrow ]
        , button [ onClick Redo, class "history-button flip" ] [ Svgs.viewUndoArrow ]
        ]


viewTextSizeDropdown : (EditOption -> Html Msg) -> Html Msg
viewTextSizeDropdown toDropdownMenu =
    div
        [ class "dropdown-things"
        ]
        [ button
            [ onClick <| ToggleDropdown Fonts
            , class "dropdown-button"
            ]
            [ Svgs.viewTextIcon
            , Svgs.viewDownArrow
            ]
        , toDropdownMenu Fonts
        ]


viewFontSizeOptions : EditState -> Html Msg
viewFontSizeOptions editState =
    fontSizes
        |> List.map (viewFontSizeOption editState.fontSize)
        |> div [ class "dropdown-option" ]


viewFillOptions : EditState -> Html Msg
viewFillOptions editState =
    fillOptions
        |> List.map (viewFillOption editState.strokeColor)
        |> div [ class "dropdown-option" ]


viewFillOption : Color -> Color -> Html Msg
viewFillOption selectedFill fill =
    button
        [ classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedFill == fill
            ]
        , onClick (SelectStrokeColor fill)
        ]
        [ Svgs.viewFillIcon fill ]


viewFontSizeOption : Float -> Float -> Html Msg
viewFontSizeOption selectedFontSize fontSize =
    button
        [ classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedFontSize == fontSize
            ]
        , onClick (SelectFontSize fontSize)
        ]
        [ text <| toString <| fontSize ]


viewLineStrokeDropdown : (EditOption -> Html Msg) -> Html Msg
viewLineStrokeDropdown toDropdownMenu =
    div
        [ class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown Strokes
            , class "dropdown-button"
            ]
            [ Svgs.viewLineStrokeDropdownIcon
            , Svgs.viewDownArrow
            ]
        , toDropdownMenu Strokes
        ]


viewFillDropdown : (EditOption -> Html Msg) -> Color -> Html Msg
viewFillDropdown toDropdownMenu fill =
    div
        [ class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown Fills
            , class "dropdown-button"
            ]
            [ Svgs.viewFillIcon fill
            , viewDownArrow
            ]
        , toDropdownMenu Fills
        ]


viewDropdownMenu : Maybe EditOption -> EditState -> EditOption -> Html Msg
viewDropdownMenu maybeDropdown editState selectedOption =
    Maybe.map (viewDropdownOptions editState selectedOption) maybeDropdown
        |> Maybe.withDefault (text "")


viewDropdownOptions : EditState -> EditOption -> EditOption -> Html Msg
viewDropdownOptions editState selectedOption editOption =
    if selectedOption /= editOption then
        text ""
    else
        case editOption of
            Shapes ->
                viewShapeOptions editState

            Lines ->
                viewLineOptions editState

            Fonts ->
                viewFontSizeOptions editState

            Fills ->
                viewFillOptions editState

            Strokes ->
                viewLineStrokeOptions editState

            SpotlightShapes ->
                viewSpotlightShapeOptions editState


viewSpotlightShapeOptions : EditState -> Html Msg
viewSpotlightShapeOptions editState =
    spotlightShapeOptions
        |> List.map (viewSpotlightShapeOption editState.drawing)
        |> div [ class "dropdown-option" ]


viewSpotlightShapeOption : Drawing -> EditMode -> Html Msg
viewSpotlightShapeOption drawing editMode =
    button
        [ classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => drawingToEditMode drawing == editMode
            ]
        , onClick <| ChangeDrawing <| editToDrawing editMode
        ]
        [ viewShapeSvg editMode ]


viewShapeDropdown : (EditOption -> Html Msg) -> Html Msg
viewShapeDropdown toDropdownMenu =
    div
        [ class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown Shapes
            , class "dropdown-button"
            ]
            [ viewShapeSvg EditOval
            , viewDownArrow
            ]
        , toDropdownMenu Shapes
        ]


viewLineDropdown : (EditOption -> Html Msg) -> Html Msg
viewLineDropdown toDropdownMenu =
    div
        [ class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown Lines
            , class "dropdown-button"
            ]
            [ viewShapeSvg EditArrow
            , viewDownArrow
            ]
        , toDropdownMenu Lines
        ]


viewShapeOptions : EditState -> Html Msg
viewShapeOptions editState =
    shapeOptions
        |> List.map (viewShapeOption editState.drawing)
        |> div [ class "dropdown-option" ]


viewShapeOption : Drawing -> EditMode -> Html Msg
viewShapeOption drawing editMode =
    button
        [ classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => drawingToEditMode drawing == editMode
            ]
        , onClick <| ChangeDrawing <| editToDrawing editMode
        ]
        [ viewShapeSvg editMode ]


viewShapeSvg : EditMode -> Html Msg
viewShapeSvg editMode =
    case editMode of
        EditRect ->
            Svgs.viewRectangleIcon

        EditRoundedRect ->
            Svgs.viewRoundedRectangleIcon

        EditOval ->
            Svgs.viewEllipseIcon

        EditArrow ->
            Svgs.viewArrowIcon

        EditLine ->
            Svgs.viewLineStroke 4 []

        EditTextBox ->
            Svgs.viewTextIcon

        EditSpotlightRect ->
            Svgs.viewRoundedRectangleIcon

        Select ->
            text "Select"


viewLineOptions : EditState -> Html Msg
viewLineOptions editState =
    lineOptions
        |> List.map (viewShapeOption editState.drawing)
        |> div [ class "dropdown-option" ]


viewLineStrokeOptions : EditState -> Html Msg
viewLineStrokeOptions editState =
    [ List.map (viewLineStrokeOption editState.stroke) lineStrokeOptions
    , List.map (viewStrokeStyleOption editState.strokeStyle) strokeStyles
    ]
        |> List.concat
        |> div [ class "dropdown-option" ]


viewStrokeStyleOption : StrokeStyle -> StrokeStyle -> Html Msg
viewStrokeStyleOption selectedStrokeStyle strokeStyle =
    button
        [ classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedStrokeStyle == strokeStyle
            ]
        , onClick (SelectStrokeStyle strokeStyle)
        ]
        [ case strokeStyle of
            Solid ->
                Svgs.viewSolidIcon

            Dotted ->
                Svgs.viewDottedIcon

            Dashed ->
                Svgs.viewDashedIcon
        ]


viewLineStrokeOption : LineStroke -> LineStroke -> Html Msg
viewLineStrokeOption selectedStroke stroke =
    button
        [ classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedStroke == stroke
            ]
        , onClick <| SelectLineStroke stroke
        ]
        [ Svgs.viewLineStroke (strokeToWidth stroke) [] ]


canvasEvents : (Mouse.Position -> Position) -> Drawing -> Mouse.Position -> List (Attribute Msg)
canvasEvents toPos drawing curMouse =
    case drawing of
        DrawRect rectMode ->
            case rectMode of
                NoRect ->
                    [ onMouseDown <| Json.map (StartRect << toPos) Mouse.position ]

                DrawingRect startPos ->
                    [ onMouseUp <| Json.map (AddRect startPos << toPos) Mouse.position ]

                DrawingSquare startPos ->
                    [ onMouseUp <| Json.map (AddRect startPos << circleMouse startPos << toPos) Mouse.position ]

        DrawRoundedRect rectMode ->
            case rectMode of
                NoRoundedRect ->
                    [ onMouseDown <| Json.map (StartRoundedRect << toPos) Mouse.position ]

                DrawingRoundedRect startPos ->
                    [ onMouseUp <| Json.map (AddRoundedRect startPos << toPos) Mouse.position ]

                DrawingRoundedSquare startPos ->
                    [ onMouseUp <| Json.map (AddRoundedRect startPos << circleMouse startPos << toPos) Mouse.position ]

        DrawArrow arrowMode ->
            case arrowMode of
                NoArrow ->
                    [ onMouseDown (Json.map (StartArrow << toPos) Mouse.position) ]

                DrawingArrow startPos ->
                    [ onMouseUp <| Json.map (AddArrow startPos << toPos) Mouse.position ]

                DrawingDiscreteArrow startPos ->
                    [ onMouseUp <| Json.map (AddArrow startPos << stepMouse startPos << toPos) Mouse.position ]

        DrawEllipse ellipseDrawing ->
            case ellipseDrawing of
                NoEllipse ->
                    [ onMouseDown (Json.map (StartEllipse << toPos) Mouse.position) ]

                DrawingOval startPos ->
                    [ onMouseUp <| Json.map (AddEllipse startPos << toPos) Mouse.position ]

                DrawingCircle startPos ->
                    [ onMouseUp <| Json.map (AddEllipse startPos << circleMouse startPos << toPos) Mouse.position ]

        DrawTextBox textBoxDrawing ->
            case textBoxDrawing of
                NoText ->
                    [ onMouseDown (Json.map (StartTextBox << toPos) Mouse.position) ]

                DrawingTextBox start ->
                    [ onMouseUp (Json.map (PlaceTextBox start << toPos) Mouse.position) ]

                EditingText ({ start, end, text, angle } as textBox) ->
                    [ if mouseIsOverRotateButton (rotateButtonPosition start end) (toPos curMouse) then
                        Html.Events.onMouseDown <| BeginRotatingTextBox 0 textBox
                      else if text == "" then
                        onClick Undo
                      else
                        onClick <| AddTextBox textBox
                    ]

                RotatingText ({ start } as textBox) ->
                    [ onMouseUp <| Json.map (FinishRotatingTextBox textBox << arrowAngle start << toPos) Mouse.position ]

        DrawLine lineDrawing ->
            case lineDrawing of
                NoLine ->
                    [ onMouseDown (Json.map (StartLine << toPos) Mouse.position) ]

                DrawingLine startPos ->
                    [ onMouseUp (Json.map (AddLine startPos << toPos) Mouse.position) ]

                DrawingDiscreteLine startPos ->
                    [ onMouseUp <| Json.map (AddLine startPos << stepMouse startPos << toPos) Mouse.position ]

        DrawSpotlightRect roundedRectMode ->
            case roundedRectMode of
                NoRoundedRect ->
                    [ onMouseDown (Json.map (StartSpotlightRect << toPos) Mouse.position) ]

                DrawingRoundedRect startPos ->
                    [ onMouseUp (Json.map (AddSpotlightRect startPos << toPos) Mouse.position) ]

                DrawingRoundedSquare startPos ->
                    [ onMouseUp (Json.map (AddSpotlightRect startPos << circleMouse startPos << toPos) Mouse.position) ]

        Selection ->
            []


viewCanvas : EditState -> Mouse.Position -> Keyboard.Extra.State -> Image -> Html Msg
viewCanvas editState curMouse keyboardState image =
    let
        toPos =
            toPosition image.width image.height

        attrs =
            canvasEvents toPos editState.drawing curMouse ++ [ id "canvas", class "image-edit" ]

        currentDrawing =
            viewDrawing editState (toPos curMouse) keyboardState

        annotations =
            editState.annotations
                |> List.reverse
                |> List.map (viewAnnotation image.width image.height editState)

        forms =
            viewImage image :: (annotations ++ [ currentDrawing ])
    in
        forms
            |> collage (round image.width) (round image.height)
            |> toHtml
            |> List.singleton
            |> div attrs


viewAnnotation : Float -> Float -> EditState -> Annotation -> Collage.Form
viewAnnotation width height editState annotation =
    case annotation of
        Arrow_ arrow ->
            viewArrow arrow

        Rect_ rect ->
            viewRect rect

        Ellipse_ ellipse ->
            viewEllipse ellipse

        Line_ line ->
            viewLine line

        TextBox_ textBox ->
            viewTextBox textBox

        Mask_ ->
            viewMask width height


viewMask : Float -> Float -> Collage.Form
viewMask width height =
    Collage.rect width height
        |> Collage.filled (Color.rgba 100 100 100 0.7)


viewDrawing : EditState -> Position -> Keyboard.Extra.State -> Collage.Form
viewDrawing { drawing, fill, strokeColor, stroke, strokeStyle, fontSize } mouse keyboardState =
    case drawing of
        DrawRect rectMode ->
            case rectMode of
                NoRect ->
                    toForm Element.empty

                DrawingRect startPos ->
                    Rect startPos mouse fill strokeColor stroke strokeStyle False
                        |> viewRect

                DrawingSquare startPos ->
                    Rect startPos (circleMouse startPos mouse) fill strokeColor stroke strokeStyle False
                        |> viewRect

        DrawRoundedRect rectMode ->
            case rectMode of
                NoRoundedRect ->
                    toForm Element.empty

                DrawingRoundedRect startPos ->
                    Rect startPos mouse fill strokeColor stroke strokeStyle True
                        |> viewRect

                DrawingRoundedSquare startPos ->
                    Rect startPos (circleMouse startPos mouse) fill strokeColor stroke strokeStyle True
                        |> viewRect

        DrawArrow arrowDrawing ->
            case arrowDrawing of
                NoArrow ->
                    toForm Element.empty

                DrawingArrow pos ->
                    Arrow pos mouse strokeColor stroke strokeStyle
                        |> viewArrow

                DrawingDiscreteArrow pos ->
                    Arrow pos (stepMouse pos mouse) strokeColor stroke strokeStyle
                        |> viewArrow

        DrawEllipse ellipseDrawing ->
            case ellipseDrawing of
                NoEllipse ->
                    toForm Element.empty

                DrawingOval pos ->
                    Ellipse pos mouse strokeColor stroke strokeStyle
                        |> viewEllipse

                DrawingCircle pos ->
                    Ellipse pos (circleMouse pos mouse) strokeColor stroke strokeStyle
                        |> viewEllipse

        DrawTextBox textBoxDrawing ->
            case textBoxDrawing of
                NoText ->
                    toForm Element.empty

                DrawingTextBox pos ->
                    TextBox pos mouse "" strokeColor stroke fontSize 0
                        |> viewTextBoxWithBorder

                EditingText { start, end, text, angle } ->
                    TextBox start end text strokeColor stroke fontSize angle
                        |> viewTextBoxWithRotateButton

                RotatingText { start, end, text, angle } ->
                    TextBox start end text strokeColor stroke fontSize angle
                        |> viewRotatingTextBox

        DrawLine lineMode ->
            case lineMode of
                NoLine ->
                    toForm Element.empty

                DrawingLine pos ->
                    Line pos mouse strokeColor stroke strokeStyle
                        |> viewLine

                DrawingDiscreteLine pos ->
                    Line pos (stepMouse pos mouse) strokeColor stroke strokeStyle
                        |> viewLine

        DrawSpotlightRect rectMode ->
            case rectMode of
                NoRoundedRect ->
                    toForm Element.empty

                DrawingRoundedRect startPos ->
                    Rect startPos mouse SpotlightFill strokeColor stroke strokeStyle True
                        |> viewRect

                DrawingRoundedSquare startPos ->
                    Rect startPos (circleMouse startPos mouse) SpotlightFill strokeColor stroke strokeStyle True
                        |> viewRect

        Selection ->
            toForm Element.empty


viewRect : Rect -> Collage.Form
viewRect ({ start, end, fill, strokeColor, stroke, strokeStyle, rounded } as rect) =
    let
        styledLine =
            styleLine strokeColor stroke strokeStyle

        lineStyle =
            { styledLine
                | join =
                    if rounded then
                        Collage.Smooth
                    else
                        Collage.Sharp 10
            }
    in
        Collage.rect (dx start end) (dy start end)
            |> Collage.outlined lineStyle
            |> alignWithMouse start end 0


styleLine : Color -> LineStroke -> StrokeStyle -> Collage.LineStyle
styleLine fill stroke strokeStyle =
    let
        strokeWidth =
            toFloat <| strokeToWidth stroke

        lineStrokeStyled =
            toLineStyle strokeStyle
    in
        { lineStrokeStyled | width = strokeWidth, color = fill }


viewArrow : Arrow -> Collage.Form
viewArrow ({ start, end, fill, stroke, strokeStyle } as arrow) =
    let
        strokeWidth =
            toFloat <| strokeToWidth stroke

        arrowHeadRadius =
            strokeWidth * 2
    in
        [ Collage.ngon 3 arrowHeadRadius
            |> filled fill
            |> rotate (arrowAngle start end)
            |> rotate (degrees 60)
            |> move start
        , Collage.segment start end
            |> Collage.traced (styleLine fill stroke strokeStyle)
        ]
            |> Collage.group


viewEllipse : Ellipse -> Collage.Form
viewEllipse ({ start, end, fill, stroke, strokeStyle } as ellipse) =
    Collage.oval (dx start end) (dy start end)
        |> Collage.outlined (styleLine fill stroke strokeStyle)
        |> alignWithMouse start end 0


viewText : String -> Color -> Float -> Collage.Form
viewText text fill fontSize =
    Text.fromString text
        |> Text.height fontSize
        |> Text.color fill
        |> Collage.text


viewTextBoxBorder : StartPosition -> EndPosition -> Collage.Form
viewTextBoxBorder start end =
    Collage.rect (dx start end) (dy start end)
        |> Collage.outlined (Collage.dotted Color.black)


viewRotateButton : StartPosition -> EndPosition -> Collage.Form
viewRotateButton start end =
    Collage.circle 7
        |> Collage.filled Color.green
        |> Collage.moveY (3 * (dy start end) / 4)


viewTextBox : TextBox -> Collage.Form
viewTextBox ({ start, end, text, fill, fontSize, angle } as textBox) =
    viewText text fill fontSize
        |> alignWithMouse start end angle


viewTextBoxWithBorder : TextBox -> Collage.Form
viewTextBoxWithBorder ({ start, end, text, fill, fontSize, angle } as textBox) =
    [ viewTextBoxBorder start end
    , viewText text fill fontSize
    ]
        |> Collage.group
        |> alignWithMouse start end angle


viewTextBoxWithRotateButton : TextBox -> Collage.Form
viewTextBoxWithRotateButton ({ start, end, text, fill, fontSize, angle } as textBox) =
    [ viewRotateButton start end
    , viewTextBoxBorder start end
    , viewText text fill fontSize
    ]
        |> Collage.group
        |> alignWithMouse start end angle


viewRotatingTextBox : TextBox -> Collage.Form
viewRotatingTextBox ({ start, end, text, fill, fontSize, angle } as textBox) =
    [ viewRotateButton start end
    , viewTextBoxBorder start end
    , viewText text fill fontSize
    ]
        |> Collage.group
        |> alignWithMouse start end angle


viewLine : Line -> Collage.Form
viewLine { start, end, fill, stroke, strokeStyle } =
    Collage.segment start end
        |> Collage.traced (styleLine fill stroke strokeStyle)


viewImage : Image -> Collage.Form
viewImage { width, height, url } =
    toForm <| Element.image (round width) (round height) url



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


toPosition : Float -> Float -> Mouse.Position -> Position
toPosition width height { x, y } =
    ( toFloat x - ((width + 30) / 2), ((height + 30) / 2) - toFloat y )


fromPosition : Float -> Float -> Position -> Mouse.Position
fromPosition width height ( x, y ) =
    Mouse.Position (round (x + ((width + 30) / 2))) (round ((y * -1) + ((height + 30) / 2)))


arrowAngle : StartPosition -> EndPosition -> Float
arrowAngle ( x1, y1 ) ( x2, y2 ) =
    let
        theta =
            atan2 (y2 - y1) (x2 - x1)

        radians =
            if theta < 0.0 then
                (2 * pi) + theta
            else
                theta
    in
        radians


editToDrawing : EditMode -> Drawing
editToDrawing editMode =
    case editMode of
        EditRect ->
            DrawRect NoRect

        EditRoundedRect ->
            DrawRoundedRect NoRoundedRect

        EditArrow ->
            DrawArrow NoArrow

        EditOval ->
            DrawEllipse NoEllipse

        EditTextBox ->
            DrawTextBox NoText

        EditLine ->
            DrawLine NoLine

        EditSpotlightRect ->
            DrawSpotlightRect NoRoundedRect

        Select ->
            Selection


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
            EditOval

        DrawTextBox _ ->
            EditTextBox

        DrawLine _ ->
            EditLine

        DrawSpotlightRect roundedRectMode ->
            EditSpotlightRect

        Selection ->
            Select


trackMouse : Drawing -> Bool
trackMouse drawing =
    case drawing of
        DrawArrow arrowMode ->
            case arrowMode of
                DrawingArrow _ ->
                    True

                DrawingDiscreteArrow _ ->
                    True

                _ ->
                    False

        DrawRoundedRect rectMode ->
            case rectMode of
                DrawingRoundedRect _ ->
                    True

                DrawingRoundedSquare _ ->
                    True

                _ ->
                    False

        DrawRect rectMode ->
            case rectMode of
                DrawingRect _ ->
                    True

                DrawingSquare _ ->
                    True

                _ ->
                    False

        DrawEllipse ellipseMode ->
            case ellipseMode of
                DrawingOval _ ->
                    True

                DrawingCircle _ ->
                    True

                _ ->
                    False

        DrawTextBox textMode ->
            case textMode of
                DrawingTextBox _ ->
                    True

                EditingText _ ->
                    True

                RotatingText _ ->
                    True

                _ ->
                    False

        DrawLine lineMode ->
            case lineMode of
                DrawingLine _ ->
                    True

                DrawingDiscreteLine _ ->
                    True

                _ ->
                    False

        DrawSpotlightRect roundedRectMode ->
            case roundedRectMode of
                DrawingRoundedRect _ ->
                    True

                _ ->
                    False

        Selection ->
            False


toDeltas : Float -> Float -> Position
toDeltas h theta =
    ( cos theta * h, (sin theta) * h )


toDelta : Position -> Position -> Position
toDelta ( x1, y1 ) ( x2, y2 ) =
    ( x2 - x1, y2 - y1 )


dx : Position -> Position -> Float
dx ( x1, _ ) ( x2, _ ) =
    x2 - x1


dy : Position -> Position -> Float
dy ( _, y1 ) ( _, y2 ) =
    y2 - y1


halfDistance : Position -> Position -> Position
halfDistance start end =
    let
        ( dx, dy ) =
            toDelta start end
    in
        ( dx / 2, dy / 2 )


calcDistance : Position -> Position -> Float
calcDistance ( x1, y1 ) ( x2, y2 ) =
    sqrt <| (x2 - x1) ^ 2 + (y2 - y1) ^ 2


stepMouse : StartPosition -> EndPosition -> EndPosition
stepMouse (( x1, y1 ) as startPos) (( x2, y2 ) as curPos) =
    arrowAngle startPos curPos
        / (pi / 4)
        |> round
        |> toFloat
        |> (*) (pi / 4)
        |> toDeltas (calcDistance startPos curPos)
        |> Tuple.mapFirst ((+) x1)
        |> Tuple.mapSecond ((+) y1)


circleMouse : StartPosition -> EndPosition -> EndPosition
circleMouse ( x1, y1 ) ( x2, y2 ) =
    if y2 < y1 then
        ( x2, y1 - abs x2 - x1 )
    else
        ( x2, y1 + abs x2 - x1 )


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


rotateButtonPosition : StartPosition -> EndPosition -> Position
rotateButtonPosition ( x1, y1 ) ( x2, y2 ) =
    let
        dy =
            y2 - y1

        buttonY =
            Basics.max y1 y2 + dy / 4
    in
        ( (x1 + ((x2 - x1) / 2)), buttonY )


mouseIsOverRotateButton : ( Float, Float ) -> ( Float, Float ) -> Bool
mouseIsOverRotateButton ( x1, y1 ) ( x2, y2 ) =
    abs (x1 - x2) < 10 && (abs y2 - y1) < 10


alignWithMouse : StartPosition -> EndPosition -> Float -> Collage.Form -> Collage.Form
alignWithMouse start end angle form =
    form
        |> Collage.move start
        |> Collage.move (halfDistance start end)
        |> Collage.rotate angle


toLineStyle : StrokeStyle -> Collage.LineStyle
toLineStyle strokeStyle =
    case strokeStyle of
        Solid ->
            Collage.defaultLine

        Dotted ->
            Collage.dotted Color.black

        Dashed ->
            Collage.dashed Color.black



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
                if trackMouse model.edits.present.drawing then
                    Mouse.moves (SetMouse (List.Zipper.current images))
                else
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
