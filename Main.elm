port module Main exposing (main)

import Char exposing (KeyCode)
import Collage exposing (collage, defaultLine, filled, move, rotate, toForm)
import Color exposing (Color)
import Dom
import Element exposing (image, toHtml)
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class, classList, id, start, style, type_)
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput, onWithOptions)
import Json.Decode as Json
import Keyboard.Extra exposing (Key, KeyChange(..))
import Mouse
import Rocket exposing ((=>))
import Svgs exposing (viewDownArrow)
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


type EllipseMode
    = NoOval
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
    | DrawEllipse EllipseMode
    | DrawTextBox TextMode
    | DrawLine LineMode
    | Selection


type EditMode
    = EditOval
    | EditArrow
    | EditLine
    | EditTextBox
    | Select


type alias Arrow =
    { start : Position
    , end : Position
    , fill : Color
    , stroke : LineStroke
    }


type alias Ellipse =
    { start : Position
    , end : Position
    , fill : Color
    , stroke : LineStroke
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
    }


type LineStroke
    = VeryThin
    | Thin
    | Medium
    | Thick
    | VeryThick


type EditOption
    = Shapes
    | Lines
    | Fonts
    | Fills
    | Strokes


type alias EditState =
    { photo : String
    , arrows : List Arrow
    , ellipses : List Ellipse
    , textBoxes : List TextBox
    , lines : List Line
    , drawing : Drawing
    , fill : Color
    , stroke : LineStroke
    , fontSize : Float
    , currentDropdown : Maybe EditOption
    }


type alias Model =
    { edits : UndoList EditState
    , mouse : Mouse.Position
    , keyboardState : Keyboard.Extra.State
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


shapeOptions : List EditMode
shapeOptions =
    [ EditOval
    ]


lineOptions : List EditMode
lineOptions =
    [ EditArrow
    , EditLine
    ]


{-|
  The Order of Edit Controls in the UI
-}
editModes : List EditMode
editModes =
    [ EditArrow
    , EditOval
    , EditTextBox
    , EditLine
    , Select
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
    { photo = ""
    , arrows = []
    , ellipses = []
    , textBoxes = []
    , lines = []
    , drawing = Selection
    , fill = Color.black
    , stroke = Medium
    , fontSize = 14
    , currentDropdown = Nothing
    }


init : ( Model, List (Cmd Msg) )
init =
    { edits = UndoList.fresh initialEditState
    , mouse = Mouse.Position 0 0
    , keyboardState = Keyboard.Extra.initialState
    }
        => []



-- UPDATE


type Msg
    = StartArrow StartPosition
    | AddArrow StartPosition EndPosition
    | StartOval StartPosition
    | AddEllipse StartPosition EndPosition
    | StartTextBox StartPosition
    | PlaceTextBox StartPosition EndPosition
    | TextBoxInput TextBox String
    | BeginRotatingTextBox Float TextBox
    | FinishRotatingTextBox TextBox Float
    | AddTextBox TextBox
    | StartLine StartPosition
    | AddLine StartPosition EndPosition
    | SetMouse Mouse.Position
    | SetImage String
    | KeyboardMsg Keyboard.Extra.Msg
    | ChangeDrawing Drawing
    | SelectFill Color
    | SelectLineStroke LineStroke
    | SelectFontSize Float
    | ToggleDropdown EditOption
    | CloseDropdown
    | Undo
    | Redo
    | Reset
    | Export


update : Msg -> Model -> ( Model, List (Cmd Msg) )
update msg ({ edits, mouse } as model) =
    let
        editState =
            edits.present

        { fill, fontSize, stroke } =
            editState
    in
        case msg of
            StartArrow pos ->
                { editState | drawing = DrawArrow (DrawingArrow pos) }
                    |> logChange model
                    |> updateMouse (fromPosition pos)
                    => []

            AddArrow startPos endPos ->
                { editState
                    | arrows = Arrow startPos endPos fill stroke :: editState.arrows
                    , drawing = DrawArrow NoArrow
                }
                    |> skipChange model
                    => []

            StartOval pos ->
                { editState | drawing = DrawEllipse <| DrawingOval pos }
                    |> logChange model
                    |> updateMouse (fromPosition pos)
                    => []

            AddEllipse startPos endPos ->
                { editState
                    | ellipses = Ellipse startPos endPos fill stroke :: editState.ellipses
                    , drawing = DrawEllipse NoOval
                }
                    |> skipChange model
                    => []

            StartTextBox pos ->
                { editState | drawing = DrawTextBox (DrawingTextBox pos) }
                    |> logChange model
                    |> updateMouse (fromPosition pos)
                    => []

            PlaceTextBox startPos endPos ->
                let
                    initialEditState =
                        TextBox startPos endPos "" editState.fill editState.stroke editState.fontSize 0

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
                { editState | drawing = DrawTextBox <| EditingText <| TextBox start end text fill stroke fontSize angle }
                    |> skipChange model
                    => []

            BeginRotatingTextBox angle { start, end, text } ->
                { editState | drawing = DrawTextBox <| RotatingText <| TextBox start end text fill stroke fontSize angle }
                    |> skipChange model
                    => []

            FinishRotatingTextBox { start, end, text } angle ->
                { editState | drawing = DrawTextBox <| EditingText <| TextBox start end text fill stroke fontSize angle }
                    |> skipChange model
                    => []

            AddTextBox { start, end, text, angle } ->
                { editState
                    | textBoxes = TextBox start end text fill stroke fontSize angle :: editState.textBoxes
                    , drawing = DrawTextBox NoText
                }
                    |> skipChange model
                    => []

            StartLine pos ->
                { editState | drawing = DrawLine <| DrawingLine pos }
                    |> logChange model
                    |> updateMouse (fromPosition pos)
                    => []

            AddLine startPos endPos ->
                { editState
                    | lines = Line startPos endPos fill stroke :: editState.lines
                    , drawing = DrawLine NoLine
                }
                    |> skipChange model
                    => []

            SetMouse pos ->
                editState
                    |> updateDrawingIfRotating pos
                    |> skipChange model
                    |> updateMouse pos
                    => []

            SetImage imageUrl ->
                { editState | photo = imageUrl }
                    |> logChange model
                    => []

            KeyboardMsg keyMsg ->
                let
                    ( keyboardState, maybeKeyChange ) =
                        Keyboard.Extra.updateWithKeyChange keyMsg model.keyboardState
                in
                    { model | keyboardState = keyboardState }
                        |> alterDrawingsWithKeyboard maybeKeyChange
                        => []

            ChangeDrawing drawing ->
                { editState | drawing = drawing }
                    |> skipChange model
                    => []

            SelectFill fill ->
                { editState | fill = fill }
                    |> logChange model
                    => []

            SelectLineStroke lineStroke ->
                { editState | stroke = lineStroke }
                    |> logChange model
                    => []

            SelectFontSize fontSize ->
                { editState | fontSize = fontSize }
                    |> logChange model
                    => []

            ToggleDropdown editOption ->
                { editState
                    | currentDropdown =
                        case editState.currentDropdown of
                            Just dropdown ->
                                if dropdown == editOption then
                                    Nothing
                                else
                                    Just editOption

                            Nothing ->
                                Just editOption
                    , drawing =
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
                    => []

            CloseDropdown ->
                { editState | currentDropdown = Nothing }
                    |> skipChange model
                    => []

            Undo ->
                { model | edits = UndoList.undo model.edits }
                    => []

            Redo ->
                { model | edits = UndoList.redo model.edits }
                    => []

            Reset ->
                { model | edits = UndoList.reset model.edits }
                    => []

            Export ->
                model
                    => [ exportToImage "annotation-app" ]


{-| Add this editState change to app history
-}
logChange : Model -> EditState -> Model
logChange model editState =
    { model | edits = UndoList.new editState model.edits }


{-| Do not add this editState change to app history
-}
skipChange : Model -> EditState -> Model
skipChange model editState =
    { model | edits = UndoList.mapPresent (\_ -> editState) model.edits }


updateMouse : Mouse.Position -> Model -> Model
updateMouse pos model =
    { model | mouse = pos }


updateDrawing : EditState -> Drawing -> EditState
updateDrawing editState drawing =
    { editState | drawing = drawing }


updateDrawingIfRotating : Mouse.Position -> EditState -> EditState
updateDrawingIfRotating mouse editState =
    case editState.drawing of
        DrawTextBox textMode ->
            case textMode of
                RotatingText textBox ->
                    { editState | drawing = DrawTextBox <| RotatingText { textBox | angle = arrowAngle textBox.start (toPosition mouse) } }

                _ ->
                    editState

        _ ->
            editState


transitionOnShift : Drawing -> Drawing
transitionOnShift drawing =
    case drawing of
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
view ({ edits, mouse, keyboardState } as model) =
    let
        editState =
            edits.present

        options =
            { preventDefault = True, stopPropagation = False }

        offscreenInput =
            case editState.drawing of
                DrawTextBox textBoxDrawing ->
                    case textBoxDrawing of
                        EditingText textBox ->
                            [ Html.input
                                [ id "text-box-edit"
                                , onInput <| TextBoxInput textBox
                                , onWithOptions "keydown" options (decodeTextInputKey <| AddTextBox textBox)
                                , class "hidden-input"
                                ]
                                [ Html.text textBox.text ]
                            ]

                        _ ->
                            []

                _ ->
                    []
    in
        div [ id "annotation-app" ]
            ([ viewCanvas editState mouse keyboardState
             , viewControls editState
             ]
                ++ offscreenInput
            )


viewControls : EditState -> Html Msg
viewControls editState =
    div [ class "controls" ]
        [ viewButtonGroup [ viewShapeDropdown editState, viewLineDropdown editState, viewTextSizeDropdown editState ]
        , viewButtonGroup [ viewFillDropdown editState, viewLineStrokeDropdown editState ]
        , viewHistoryControls
        , button [ Html.Events.onClick Export, class "export-button" ] [ text "Export" ]
        ]


viewButtonGroup : List (Html Msg) -> Html Msg
viewButtonGroup buttons =
    div [ class "button-group" ] buttons


viewHistoryControls : Html Msg
viewHistoryControls =
    div [ class "history-controls" ]
        [ button [ Html.Events.onClick Reset ] [ text "Reset" ]
        , button [ Html.Events.onClick Undo ] [ text "Undo" ]
        , button [ Html.Events.onClick Redo ] [ text "Redo" ]
        ]


viewTextSizeDropdown : EditState -> Html Msg
viewTextSizeDropdown editState =
    div
        [ class "dropdown-things"
        ]
        [ button
            [ Html.Events.onClick <| ToggleDropdown Fonts
            , class "dropdown-button"
            ]
            [ Svgs.viewTextIcon
            , Svgs.viewDownArrow
            ]
        , viewDropdownMenu editState Fonts
        ]


viewFontSizeOptions : EditState -> Html Msg
viewFontSizeOptions editState =
    fontSizes
        |> List.map (viewFontSizeOption editState.fontSize)
        |> div [ class "dropdown-option" ]


viewFillOptions : EditState -> Html Msg
viewFillOptions editState =
    fillOptions
        |> List.map (viewFillOption editState.fill)
        |> div [ class "dropdown-option" ]


viewFillOption : Color -> Color -> Html Msg
viewFillOption selectedFill fill =
    button
        [ classList
            [ "color-option" => True
            , "color-option--selected" => selectedFill == fill
            ]
        , Html.Events.onClick (SelectFill fill)
        ]
        [ Svgs.viewFillIcon fill ]


viewFontSizeOption : Float -> Float -> Html Msg
viewFontSizeOption selectedFontSize fontSize =
    button
        [ classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedFontSize == fontSize
            ]
        , Html.Events.onClick (SelectFontSize fontSize)
        ]
        [ text <| toString <| fontSize ]


viewLineStrokeDropdown : EditState -> Html Msg
viewLineStrokeDropdown editState =
    div
        [ class "dropdown-things" ]
        [ button
            [ Html.Events.onClick <| ToggleDropdown Strokes
            , class "dropdown-button"
            ]
            [ Svgs.viewLineStrokeDropdownIcon
            , Svgs.viewDownArrow
            ]
        , viewDropdownMenu editState Strokes
        ]


viewFillDropdown : EditState -> Html Msg
viewFillDropdown editState =
    div
        [ class "dropdown-things" ]
        [ button
            [ Html.Events.onClick <| ToggleDropdown Fills
            , class "dropdown-button"
            ]
            [ Svgs.viewFillIcon editState.fill
            , viewDownArrow
            ]
        , viewDropdownMenu editState Fills
        ]


viewDropdownMenu : EditState -> EditOption -> Html Msg
viewDropdownMenu editState selectedOption =
    Maybe.map (viewDropdownOptions editState selectedOption) editState.currentDropdown
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


viewShapeDropdown : EditState -> Html Msg
viewShapeDropdown editState =
    div
        [ class "dropdown-things" ]
        [ button
            [ Html.Events.onClick <| ToggleDropdown Shapes
            , class "dropdown-button"
            ]
            [ viewShapeSvg EditOval
            , viewDownArrow
            ]
        , viewDropdownMenu editState Shapes
        ]


viewLineDropdown : EditState -> Html Msg
viewLineDropdown editState =
    div
        [ class "dropdown-things" ]
        [ button
            [ Html.Events.onClick <| ToggleDropdown Lines
            , class "dropdown-button"
            ]
            [ viewShapeSvg EditArrow
            , viewDownArrow
            ]
        , viewDropdownMenu editState Lines
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
        , Html.Events.onClick <| ChangeDrawing <| editToDrawing editMode
        ]
        [ viewShapeSvg editMode ]


viewShapeSvg : EditMode -> Html Msg
viewShapeSvg editMode =
    case editMode of
        EditOval ->
            Svgs.viewOvalIcon

        EditArrow ->
            Svgs.viewArrowIcon

        EditLine ->
            Svgs.viewLineStroke 4

        EditTextBox ->
            Svgs.viewTextIcon

        Select ->
            text "Select"


viewLineOptions : EditState -> Html Msg
viewLineOptions editState =
    lineOptions
        |> List.map (viewShapeOption editState.drawing)
        |> div [ class "dropdown-option" ]


viewLineStrokeOptions : EditState -> Html Msg
viewLineStrokeOptions editState =
    lineStrokeOptions
        |> List.map (viewLineStrokeOption editState.stroke)
        |> div [ class "dropdown-option" ]


viewLineStrokeOption : LineStroke -> LineStroke -> Html Msg
viewLineStrokeOption selectedStroke stroke =
    button
        [ classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedStroke == stroke
            ]
        , Html.Events.onClick (SelectLineStroke stroke)
        ]
        [ Svgs.viewLineStroke (strokeToWidth stroke) ]


viewEditOption : EditState -> EditMode -> Html Msg
viewEditOption editState editMode =
    button
        [ Html.Events.onClick (ChangeDrawing <| editToDrawing editMode)
        , classList [ "edit-mode--selected" => drawingToEditMode editState.drawing == editMode ]
        ]
        [ text <| modeToString editMode ]


viewCanvas : EditState -> Mouse.Position -> Keyboard.Extra.State -> Html Msg
viewCanvas editState curMouse keyboardState =
    let
        attrs =
            case editState.drawing of
                DrawArrow arrowDrawing ->
                    case arrowDrawing of
                        NoArrow ->
                            [ onClick (Json.map (StartArrow << toPosition) Mouse.position) ]

                        DrawingArrow startPos ->
                            [ onClick <| Json.map (AddArrow startPos << toPosition) Mouse.position ]

                        DrawingDiscreteArrow startPos ->
                            [ onClick <| Json.map (AddArrow startPos << stepMouse startPos << toPosition) Mouse.position ]

                DrawEllipse ellipseDrawing ->
                    case ellipseDrawing of
                        NoOval ->
                            [ onClick (Json.map (StartOval << toPosition) Mouse.position) ]

                        DrawingOval startPos ->
                            [ onClick <| Json.map (AddEllipse startPos << toPosition) Mouse.position ]

                        DrawingCircle startPos ->
                            [ onClick <| Json.map (AddEllipse startPos << circleMouse startPos << toPosition) Mouse.position ]

                DrawTextBox textBoxDrawing ->
                    case textBoxDrawing of
                        NoText ->
                            [ onClick (Json.map (StartTextBox << toPosition) Mouse.position) ]

                        DrawingTextBox start ->
                            [ onClick (Json.map (PlaceTextBox start << toPosition) Mouse.position) ]

                        EditingText ({ start, end, text, angle } as textBox) ->
                            [ if mouseIsOverRotateButton (rotateButtonPosition start end) (toPosition curMouse) then
                                Html.Events.onClick <| BeginRotatingTextBox 0 textBox
                              else if text == "" then
                                Html.Events.onClick Undo
                              else
                                Html.Events.onClick <| AddTextBox textBox
                            ]

                        RotatingText ({ start } as textBox) ->
                            [ onClick <| Json.map (FinishRotatingTextBox textBox << arrowAngle start << toPosition) Mouse.position ]

                DrawLine lineDrawing ->
                    case lineDrawing of
                        NoLine ->
                            [ onClick (Json.map (StartLine << toPosition) Mouse.position) ]

                        DrawingLine startPos ->
                            [ onClick (Json.map (AddLine startPos << toPosition) Mouse.position) ]

                        DrawingDiscreteLine startPos ->
                            [ onClick <| Json.map (AddLine startPos << stepMouse startPos << toPosition) Mouse.position ]

                Selection ->
                    []

        currentDrawing =
            viewDrawing editState (toPosition curMouse) keyboardState

        arrows =
            List.map viewArrow editState.arrows

        ellipses =
            List.map viewEllipse editState.ellipses

        textBoxes =
            List.map viewTextBox editState.textBoxes

        lines =
            List.map viewLine editState.lines

        forms =
            List.concat
                [ [ toForm <| viewImage editState.photo
                  , currentDrawing
                  ]
                , arrows
                , ellipses
                , textBoxes
                , lines
                ]
    in
        forms
            |> collage 300 200
            |> toHtml
            |> List.singleton
            |> div (attrs ++ [ id "canvas" ])


viewDrawing : EditState -> Position -> Keyboard.Extra.State -> Collage.Form
viewDrawing { drawing, fill, stroke, fontSize } mouse keyboardState =
    case drawing of
        DrawArrow arrowDrawing ->
            case arrowDrawing of
                NoArrow ->
                    toForm Element.empty

                DrawingArrow pos ->
                    Arrow pos mouse fill stroke
                        |> viewArrow

                DrawingDiscreteArrow pos ->
                    Arrow pos (stepMouse pos mouse) fill stroke
                        |> viewArrow

        DrawEllipse ellipseDrawing ->
            case ellipseDrawing of
                NoOval ->
                    toForm Element.empty

                DrawingOval pos ->
                    Ellipse pos mouse fill stroke
                        |> viewEllipse

                DrawingCircle pos ->
                    Ellipse pos (circleMouse pos mouse) fill stroke
                        |> viewEllipse

        DrawTextBox textBoxDrawing ->
            case textBoxDrawing of
                NoText ->
                    toForm Element.empty

                DrawingTextBox pos ->
                    TextBox pos mouse "" fill stroke fontSize 0
                        |> viewTextBoxWithBorder

                EditingText { start, end, text, angle } ->
                    TextBox start end text fill stroke fontSize angle
                        |> viewTextBoxWithRotateButton

                RotatingText { start, end, text, angle } ->
                    TextBox start end text fill stroke fontSize angle
                        |> viewRotatingTextBox

        DrawLine lineMode ->
            case lineMode of
                NoLine ->
                    toForm Element.empty

                DrawingLine pos ->
                    Line pos mouse fill stroke
                        |> viewLine

                DrawingDiscreteLine pos ->
                    Line pos (stepMouse pos mouse) fill stroke
                        |> viewLine

        Selection ->
            toForm Element.empty


viewArrow : Arrow -> Collage.Form
viewArrow ({ start, end, fill, stroke } as arrow) =
    let
        strokeWidth =
            toFloat <| strokeToWidth stroke

        lineStyle =
            { defaultLine | width = strokeWidth, color = fill }

        arrowHeadRadius =
            strokeWidth * 2
    in
        [ Collage.ngon 3 arrowHeadRadius
            |> filled fill
            |> rotate (arrowAngle start end)
            |> rotate (degrees 60)
            |> move start
        , Collage.segment start end
            |> Collage.traced lineStyle
        ]
            |> Collage.group


viewEllipse : Ellipse -> Collage.Form
viewEllipse ({ start, end, fill, stroke } as ellipse) =
    Collage.oval (dx start end) (dy start end)
        |> Collage.outlined { defaultLine | color = fill, width = toFloat <| strokeToWidth stroke }
        |> alignWithMouse start end 0


viewText : String -> Color -> Float -> Collage.Form
viewText text fill fontSize =
    Text.fromString text
        |> Text.height (Debug.log "fontSize" fontSize)
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
viewLine { start, end, fill, stroke } =
    let
        lineStyle =
            { defaultLine | width = toFloat <| strokeToWidth stroke, color = fill }
    in
        Collage.segment start end
            |> Collage.traced lineStyle


viewImage : String -> Element.Element
viewImage photo =
    image 300 200 photo



-- HELPERS


onClick : Json.Decoder msg -> Html.Attribute msg
onClick decodeToMsg =
    on "click" decodeToMsg


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


toPosition : Mouse.Position -> Position
toPosition { x, y } =
    ( toFloat (x - 150), toFloat (100 - y) )


fromPosition : Position -> Mouse.Position
fromPosition ( x, y ) =
    Mouse.Position (round x + 150) (round ((y * -1) + 100))


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


modeToString : EditMode -> String
modeToString mode =
    case mode of
        EditArrow ->
            "Arrow"

        EditOval ->
            "Oval"

        EditTextBox ->
            "Text"

        EditLine ->
            "Line"

        Select ->
            "Select"


editToDrawing : EditMode -> Drawing
editToDrawing editMode =
    case editMode of
        EditArrow ->
            DrawArrow NoArrow

        EditOval ->
            DrawEllipse NoOval

        EditTextBox ->
            DrawTextBox NoText

        EditLine ->
            DrawLine NoLine

        Select ->
            Selection


drawingToEditMode : Drawing -> EditMode
drawingToEditMode drawing =
    case drawing of
        DrawArrow _ ->
            EditArrow

        DrawEllipse _ ->
            EditOval

        DrawTextBox _ ->
            EditTextBox

        DrawLine _ ->
            EditLine

        Selection ->
            Select


trackMouse : EditState -> Bool
trackMouse editState =
    case editState.drawing of
        DrawArrow arrowMode ->
            case arrowMode of
                DrawingArrow _ ->
                    True

                DrawingDiscreteArrow _ ->
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



-- PORTS


port exportToImage : String -> Cmd msg


port importImage : (String -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if trackMouse model.edits.present then
            Mouse.moves SetMouse
          else
            Sub.none
        , Sub.map KeyboardMsg Keyboard.Extra.subscriptions
        , importImage SetImage
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
