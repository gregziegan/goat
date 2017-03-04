port module Main exposing (main)

import Char exposing (KeyCode)
import Collage exposing (collage, defaultLine, filled, move, rotate, toForm)
import Color exposing (Color)
import Color.Convert
import Dom
import Element exposing (image, toHtml)
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class, classList, id, style, type_)
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput, onWithOptions)
import Json.Decode as Json
import Keyboard.Extra
import Mouse
import Rocket exposing ((=>))
import Svgs
import Task exposing (succeed)
import Text
import UndoList exposing (UndoList)


-- MODEL


type alias StartPosition =
    Mouse.Position


type alias EndPosition =
    Mouse.Position


type ArrowMode
    = NoArrow
    | DrawingArrow StartPosition


type OvalMode
    = NoOval
    | DrawingOval StartPosition


type TextMode
    = NoText
    | DrawingTextBox StartPosition
    | EditingTextMode StartPosition EndPosition String


type LineMode
    = NoLine
    | DrawingLine Mouse.Position


type Drawing
    = DrawArrow ArrowMode
    | DrawOval OvalMode
    | DrawTextBox TextMode
    | DrawLine LineMode
    | Selection


type EditMode
    = EditArrow
    | EditOval
    | EditTextBox
    | EditLine
    | Select


type alias Arrow =
    { start : Mouse.Position
    , end : Mouse.Position
    , fill : Color
    , stroke : LineStroke
    }


type alias Oval =
    { start : Mouse.Position
    , end : Mouse.Position
    , fill : Color
    , stroke : LineStroke
    }


type alias TextBox =
    { start : Mouse.Position
    , end : Mouse.Position
    , text : String
    , fill : Color
    , stroke : LineStroke
    , fontSize : Float
    }


type alias Line =
    { start : Mouse.Position
    , end : Mouse.Position
    , fill : Color
    , stroke : LineStroke
    }


type LineStroke
    = VeryThin
    | Thin
    | Medium
    | Thick
    | VeryThick


type alias EditState =
    { photo : String
    , arrows : List Arrow
    , ovals : List Oval
    , textBoxes : List TextBox
    , lines : List Line
    , drawing : Drawing
    , fill : Color
    , stroke : LineStroke
    , fontSize : Float
    , showLineStrokeOptions : Bool
    , showFontSizeOptions : Bool
    }


type alias Model =
    { edits : UndoList EditState
    , mouse : Mouse.Position
    , keyboardState : Keyboard.Extra.State
    }


colorOptions : List Color
colorOptions =
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
    , ovals = []
    , textBoxes = []
    , lines = []
    , drawing = Selection
    , fill = Color.black
    , stroke = Medium
    , fontSize = 14
    , showLineStrokeOptions = False
    , showFontSizeOptions = False
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
    = StartArrow Mouse.Position
    | AddArrow StartPosition Mouse.Position
    | StartOval Mouse.Position
    | AddOval StartPosition Mouse.Position
    | StartTextBox Mouse.Position
    | PlaceTextBox Mouse.Position Mouse.Position
    | TextBoxInput TextMode String
    | AddTextBox TextMode
    | StartLine Mouse.Position
    | AddLine Mouse.Position Mouse.Position
    | SetMouse Mouse.Position
    | SetImage String
    | KeyboardMsg Keyboard.Extra.Msg
    | ChangeDrawing Drawing
    | SelectColor Color
    | SelectLineStroke LineStroke
    | SelectFontSize Float
    | ToggleLineStrokeDropdown
    | ToggleFontSizeDropdown
    | Undo
    | Redo
    | Reset
    | Export


update : Msg -> Model -> ( Model, List (Cmd Msg) )
update msg ({ edits, mouse } as model) =
    let
        editState =
            edits.present
    in
        case msg of
            StartArrow pos ->
                { editState | drawing = DrawArrow (DrawingArrow pos) }
                    |> logChange model
                    |> updateMouse pos
                    => []

            AddArrow startPos endPos ->
                { editState
                    | arrows = Arrow startPos endPos editState.fill editState.stroke :: editState.arrows
                    , drawing = DrawArrow NoArrow
                }
                    |> skipChange model
                    => []

            StartOval pos ->
                { editState | drawing = DrawOval (DrawingOval pos) }
                    |> logChange model
                    |> updateMouse pos
                    => []

            AddOval startPos endPos ->
                { editState
                    | ovals = Oval startPos endPos editState.fill editState.stroke :: editState.ovals
                    , drawing = DrawOval NoOval
                }
                    |> skipChange model
                    => []

            StartTextBox pos ->
                { editState | drawing = DrawTextBox (DrawingTextBox pos) }
                    |> logChange model
                    |> updateMouse pos
                    => []

            PlaceTextBox startPos endPos ->
                let
                    initialEdit =
                        EditingTextMode startPos endPos ""

                    tryToEdit result =
                        case result of
                            Ok _ ->
                                TextBoxInput initialEdit ""

                            Err _ ->
                                Undo
                in
                    { editState | drawing = DrawTextBox initialEdit }
                        |> skipChange model
                        => [ Dom.focus "text-box-edit"
                                |> Task.attempt tryToEdit
                           ]

            TextBoxInput textMode inputString ->
                let
                    newEditState =
                        case textMode of
                            EditingTextMode startPos endPos text ->
                                { editState | drawing = DrawTextBox (EditingTextMode startPos endPos inputString) }

                            _ ->
                                editState
                in
                    newEditState
                        |> skipChange model
                        => []

            AddTextBox textMode ->
                let
                    newEditState =
                        case textMode of
                            EditingTextMode startPos endPos text ->
                                { editState
                                    | textBoxes = TextBox startPos endPos text editState.fill editState.stroke editState.fontSize :: editState.textBoxes
                                    , drawing = DrawTextBox NoText
                                }

                            _ ->
                                editState
                in
                    newEditState
                        |> skipChange model
                        => []

            StartLine pos ->
                { editState | drawing = DrawLine (DrawingLine pos) }
                    |> logChange model
                    |> updateMouse pos
                    => []

            AddLine startPos endPos ->
                { editState
                    | lines = Line startPos endPos editState.fill editState.stroke :: editState.lines
                    , drawing = DrawLine NoLine
                }
                    |> skipChange model
                    => []

            SetMouse pos ->
                { model | mouse = pos }
                    => []

            SetImage imageUrl ->
                { editState | photo = imageUrl }
                    |> logChange model
                    => []

            KeyboardMsg keyMsg ->
                { model
                    | keyboardState =
                        Keyboard.Extra.update keyMsg model.keyboardState
                }
                    => []

            ChangeDrawing drawing ->
                { editState | drawing = drawing }
                    |> skipChange model
                    => []

            SelectColor color ->
                { editState | fill = color }
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

            ToggleLineStrokeDropdown ->
                { editState | showLineStrokeOptions = not editState.showLineStrokeOptions }
                    |> skipChange model
                    => []

            ToggleFontSizeDropdown ->
                { editState | showFontSizeOptions = not editState.showFontSizeOptions }
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
                        EditingTextMode startPos endPos text ->
                            [ Html.input
                                [ id "text-box-edit"
                                , onInput (TextBoxInput (EditingTextMode startPos endPos text))
                                , onWithOptions "keydown" options (decodeTextInputKey <| AddTextBox <| EditingTextMode startPos endPos text)
                                , class "hidden-input"
                                ]
                                [ Html.text text ]
                            ]

                        _ ->
                            []

                _ ->
                    []
    in
        div [ id "annotation-app" ]
            ([ viewCanvas editState mouse keyboardState
             , viewControls editState
             , viewColorSelection editState
             , viewLineStrokeDropdown editState
             , viewTextSizeDropdown editState
             , button [ Html.Events.onClick Export ] [ text "Export" ]
             ]
                ++ offscreenInput
            )


viewControls : EditState -> Html Msg
viewControls editState =
    div []
        (button [ Html.Events.onClick Reset ] [ text "Reset" ]
            :: button [ Html.Events.onClick Undo ] [ text "Undo" ]
            :: button [ Html.Events.onClick Redo ] [ text "Redo" ]
            :: List.map (viewEditOption editState) editModes
        )


viewTextSizeDropdown : EditState -> Html Msg
viewTextSizeDropdown editState =
    div
        [ class "dropdown-things"
        ]
        [ button
            [ Html.Events.onClick ToggleFontSizeDropdown
            , class "dropdown-button"
            ]
            [ Svgs.viewTextIcon
            , Svgs.viewDownArrow
            ]
        , if editState.showFontSizeOptions then
            viewTextSizeOptions editState
          else
            text ""
        ]


viewTextSizeOptions : EditState -> Html Msg
viewTextSizeOptions editState =
    fontSizes
        |> List.map (viewFontSizeOption editState.fontSize)
        |> div [ class "dropdown-option" ]


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


viewColorSelection : EditState -> Html Msg
viewColorSelection editState =
    colorOptions
        |> List.map (viewColorOption editState.fill)
        |> div []


viewColorOption : Color -> Color -> Html Msg
viewColorOption selectedColor color =
    button
        [ classList [ "color-option" => True, "color-option--selected" => selectedColor == color ]
        , style [ "background-color" => Color.Convert.colorToHex color ]
        , Html.Events.onClick (SelectColor color)
        ]
        []


viewLineStrokeDropdown : EditState -> Html Msg
viewLineStrokeDropdown editState =
    div
        [ class "dropdown-things" ]
        [ button
            [ Html.Events.onClick ToggleLineStrokeDropdown
            , class "dropdown-button"
            ]
            [ Svgs.viewLineStrokeDropdownIcon
            , Html.span [ style [ "font-size" => "20px", "color" => "grey" ] ] [ text "⌄" ]
            ]
        , if editState.showLineStrokeOptions then
            viewLineStrokeOptions editState
          else
            text ""
        ]


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
                            [ onClick (Json.map StartArrow Mouse.position) ]

                        DrawingArrow startPos ->
                            [ onClick (Json.map (AddArrow startPos) Mouse.position) ]

                DrawOval ovalDrawing ->
                    case ovalDrawing of
                        NoOval ->
                            [ onClick (Json.map StartOval Mouse.position) ]

                        DrawingOval startPos ->
                            [ onClick (Json.map (AddOval startPos) Mouse.position) ]

                DrawTextBox textBoxDrawing ->
                    case textBoxDrawing of
                        NoText ->
                            [ onClick (Json.map StartTextBox Mouse.position) ]

                        DrawingTextBox startPos ->
                            [ onClick (Json.map (PlaceTextBox startPos) Mouse.position) ]

                        EditingTextMode startPos endPos text ->
                            [ if text == "" then
                                Html.Events.onClick Undo
                              else
                                Html.Events.onClick <| AddTextBox <| EditingTextMode startPos endPos text
                            ]

                DrawLine lineDrawing ->
                    case lineDrawing of
                        NoLine ->
                            [ onClick (Json.map StartLine Mouse.position) ]

                        DrawingLine startPos ->
                            [ onClick (Json.map (AddLine startPos) Mouse.position) ]

                Selection ->
                    []

        currentDrawing =
            viewDrawing editState curMouse keyboardState

        arrows =
            List.map viewArrow editState.arrows

        ovals =
            List.map viewOval editState.ovals

        textBoxes =
            List.map (viewTextBox False) editState.textBoxes

        lines =
            List.map viewLine editState.lines

        forms =
            List.concat
                [ [ toForm <| viewImage editState.photo
                  , currentDrawing
                  ]
                , arrows
                , ovals
                , textBoxes
                , lines
                ]
    in
        forms
            |> collage 300 200
            |> toHtml
            |> List.singleton
            |> div (attrs ++ [ id "canvas" ])


viewDrawing : EditState -> Mouse.Position -> Keyboard.Extra.State -> Collage.Form
viewDrawing { drawing, fill, stroke, fontSize } mouse keyboardState =
    case drawing of
        DrawArrow arrowDrawing ->
            case arrowDrawing of
                NoArrow ->
                    toForm Element.empty

                DrawingArrow pos ->
                    Arrow pos (normalizeMouse pos mouse keyboardState) fill stroke
                        |> viewArrow

        DrawOval ovalDrawing ->
            case ovalDrawing of
                NoOval ->
                    toForm Element.empty

                DrawingOval pos ->
                    Oval pos mouse fill stroke
                        |> viewOval

        DrawTextBox textBoxDrawing ->
            case textBoxDrawing of
                NoText ->
                    toForm Element.empty

                DrawingTextBox pos ->
                    TextBox pos mouse "" fill stroke fontSize
                        |> viewTextBox True

                EditingTextMode startPos endPos text ->
                    TextBox startPos endPos text fill stroke fontSize
                        |> viewTextBox True

        DrawLine lineMode ->
            case lineMode of
                NoLine ->
                    toForm Element.empty

                DrawingLine pos ->
                    Line pos (normalizeMouse pos mouse keyboardState) fill stroke
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
        Collage.group
            [ Collage.ngon 3 arrowHeadRadius
                |> filled fill
                |> rotate (arrowAngle arrow)
                |> rotate (degrees 90)
                |> move (mouseOffset start)
            , Collage.segment (mouseOffset start) (mouseOffset end)
                |> Collage.traced lineStyle
            ]


viewOval : Oval -> Collage.Form
viewOval ({ start, end, fill, stroke } as oval) =
    let
        lineStyle =
            { defaultLine | color = fill, width = toFloat <| strokeToWidth stroke }

        delta =
            end.x
                - start.x
                |> toFloat

        ( offsetX, offsetY ) =
            mouseOffset start
    in
        Collage.oval (toFloat (end.x - start.x)) (toFloat (end.y - start.y))
            |> Collage.outlined lineStyle
            |> Collage.move ( offsetX, offsetY )
            |> Collage.moveX (delta / 2)


viewTextBox : Bool -> TextBox -> Collage.Form
viewTextBox showBorder ({ start, end, text, fill, fontSize } as textBox) =
    let
        delta =
            end.x
                - start.x
                |> toFloat

        ( offsetX, offsetY ) =
            mouseOffset start
    in
        Collage.group
            [ if showBorder then
                Collage.rect (toFloat (end.x - start.x)) (toFloat (end.y - start.y))
                    |> Collage.outlined (Collage.dotted fill)
              else
                toForm Element.empty
            , Text.fromString text
                |> Text.height fontSize
                |> Collage.text
            ]
            |> Collage.move ( offsetX, offsetY )
            |> Collage.move ( (delta / 2), (toFloat <| start.y - end.y) )


viewLine : Line -> Collage.Form
viewLine { start, end, fill, stroke } =
    let
        lineStyle =
            { defaultLine | width = toFloat <| strokeToWidth stroke, color = fill }
    in
        Collage.segment (mouseOffset start) (mouseOffset end)
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


mouseOffset : Mouse.Position -> ( Float, Float )
mouseOffset { x, y } =
    ( toFloat (x - 150), toFloat (100 - y) )


arrowAngle : Arrow -> Float
arrowAngle { start, end } =
    let
        theta =
            atan2 (toFloat (end.x - start.x)) (toFloat (end.y - start.y))

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
            DrawOval NoOval

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

        DrawOval _ ->
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

                _ ->
                    False

        DrawOval ovalMode ->
            case ovalMode of
                DrawingOval _ ->
                    True

                _ ->
                    False

        DrawTextBox textMode ->
            case textMode of
                DrawingTextBox _ ->
                    True

                _ ->
                    False

        DrawLine lineMode ->
            case lineMode of
                DrawingLine _ ->
                    True

                _ ->
                    False

        Selection ->
            False


shiftPressed : Keyboard.Extra.State -> Bool
shiftPressed keyboardState =
    Keyboard.Extra.isPressed Keyboard.Extra.Shift keyboardState


altPressed : Keyboard.Extra.State -> Bool
altPressed keyboardState =
    Keyboard.Extra.isPressed Keyboard.Extra.Alt keyboardState


normalizeMouse : Mouse.Position -> Mouse.Position -> Keyboard.Extra.State -> Mouse.Position
normalizeMouse startPos curPos keyboardState =
    if shiftPressed keyboardState then
        { curPos | y = startPos.y }
    else if altPressed keyboardState then
        { curPos | x = startPos.x }
    else
        curPos


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
