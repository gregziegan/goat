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
import Keyboard.Extra exposing (KeyChange(..), Key)
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
    | DrawingDiscreteArrow StartPosition


type EllipseMode
    = NoOval
    | DrawingOval StartPosition
    | DrawingCircle StartPosition


type TextMode
    = NoText
    | DrawingTextBox StartPosition
    | EditingTextMode StartPosition EndPosition String


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


type alias Ellipse =
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
    , ellipses : List Ellipse
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
    , ellipses = []
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
    = StartArrow StartPosition
    | AddArrow StartPosition EndPosition
    | StartOval Mouse.Position
    | AddEllipse StartPosition EndPosition
    | StartTextBox StartPosition
    | PlaceTextBox StartPosition EndPosition
    | TextBoxInput TextMode String
    | AddTextBox TextMode
    | StartLine StartPosition
    | AddLine StartPosition EndPosition
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
                { editState | drawing = DrawEllipse (DrawingOval pos) }
                    |> logChange model
                    |> updateMouse pos
                    => []

            AddEllipse startPos endPos ->
                { editState
                    | ellipses = Ellipse startPos endPos editState.fill editState.stroke :: editState.ellipses
                    , drawing = DrawEllipse NoOval
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


updateDrawing : EditState -> Drawing -> EditState
updateDrawing editState drawing =
    { editState | drawing = drawing }


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
                            [ onClick <| Json.map (AddArrow startPos) Mouse.position ]

                        DrawingDiscreteArrow startPos ->
                            [ onClick <| Json.map (AddArrow startPos << stepMouse startPos) Mouse.position ]

                DrawEllipse ellipseDrawing ->
                    case ellipseDrawing of
                        NoOval ->
                            [ onClick (Json.map StartOval Mouse.position) ]

                        DrawingOval startPos ->
                            [ onClick <| Json.map (AddEllipse startPos) Mouse.position ]

                        DrawingCircle startPos ->
                            [ onClick <| Json.map (AddEllipse startPos << circleMouse startPos) Mouse.position ]

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

                        DrawingDiscreteLine startPos ->
                            [ onClick <| Json.map (AddLine startPos << stepMouse startPos) Mouse.position ]

                Selection ->
                    []

        currentDrawing =
            viewDrawing editState curMouse keyboardState

        arrows =
            List.map viewArrow editState.arrows

        ellipses =
            List.map viewEllipse editState.ellipses

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


viewDrawing : EditState -> Mouse.Position -> Keyboard.Extra.State -> Collage.Form
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
        Collage.group
            [ Collage.ngon 3 arrowHeadRadius
                |> filled fill
                |> rotate (arrowAngle arrow.start arrow.end)
                |> rotate (degrees 60)
                |> move (mouseOffset start)
            , Collage.segment (mouseOffset start) (mouseOffset end)
                |> Collage.traced lineStyle
            ]


viewEllipse : Ellipse -> Collage.Form
viewEllipse ({ start, end, fill, stroke } as ellipse) =
    let
        lineStyle =
            { defaultLine | color = fill, width = toFloat <| strokeToWidth stroke }

        ( x1, y1 ) =
            mouseOffset start

        ( x2, y2 ) =
            mouseOffset end
    in
        Collage.oval (x2 - x1) (y2 - y1)
            |> Collage.outlined lineStyle
            |> Collage.move ( x1, y1 )
            |> Collage.moveX ((x2 - x1) / 2)
            |> Collage.moveY ((y2 - y1) / 2)


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


arrowAngle : Mouse.Position -> Mouse.Position -> Float
arrowAngle start end =
    let
        ( x1, y1 ) =
            mouseOffset start

        ( x2, y2 ) =
            mouseOffset end

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


toDeltas : Float -> Float -> ( Int, Int )
toDeltas h theta =
    ( round <| cos theta * h, round <| (sin theta) * h )


calcDistance : ( number, number ) -> ( number, number ) -> Float
calcDistance ( x1, y1 ) ( x2, y2 ) =
    sqrt <| (x2 - x1) ^ 2 + (y2 - y1) ^ 2


stepMouse : StartPosition -> EndPosition -> EndPosition
stepMouse startPos curPos =
    arrowAngle startPos curPos
        / (pi / 4)
        |> round
        |> toFloat
        |> (*) (pi / 4)
        |> toDeltas (calcDistance (mouseOffset startPos) (mouseOffset curPos))
        |> Tuple.mapFirst ((+) startPos.x)
        |> Tuple.mapSecond ((-) startPos.y)
        |> uncurry Mouse.Position


circleMouse : StartPosition -> EndPosition -> EndPosition
circleMouse startPos endPos =
    let
        ( x1, _ ) =
            mouseOffset startPos

        ( x2, _ ) =
            mouseOffset endPos
    in
        if endPos.y < startPos.y then
            { endPos | y = startPos.y - (round <| abs x2 - x1) }
        else
            { endPos | y = startPos.y + (round <| abs x2 - x1) }


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
