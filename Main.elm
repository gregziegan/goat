port module Main exposing (main)

import Collage exposing (collage, defaultLine, filled, move, rotate, toForm)
import Color exposing (Color)
import Color.Convert
import Dom
import Element exposing (image, toHtml)
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (id, style, type_)
import Html.Events exposing (keyCode, on, onInput, onWithOptions)
import Json.Decode as Json
import Mouse
import Rocket exposing ((=>))
import Task exposing (succeed)
import Text
import UndoList exposing (UndoList)


-- MODEL


type ArrowMode
    = NoArrow
    | DrawingArrow Mouse.Position


type OvalMode
    = NoOval
    | DrawingOval Mouse.Position


type TextMode
    = NoText
    | DrawingTextBox Mouse.Position
    | EditingTextMode Mouse.Position Mouse.Position String


type Drawing
    = DrawArrow ArrowMode
    | DrawOval OvalMode
    | DrawTextBox TextMode
    | Selection


type EditMode
    = EditArrow
    | EditOval
    | EditTextBox
    | Select


type alias Arrow =
    { start : Mouse.Position
    , end : Mouse.Position
    , fill : Color
    }


type alias Oval =
    { start : Mouse.Position
    , end : Mouse.Position
    , fill : Color
    }


type alias TextBox =
    { start : Mouse.Position
    , end : Mouse.Position
    , text : String
    , fill : Color
    }


type alias EditState =
    { arrows : List Arrow
    , ovals : List Oval
    , textBoxes : List TextBox
    , drawing : Drawing
    , fill : Color
    }


type alias Model =
    { edits : UndoList EditState
    , mouse : Mouse.Position
    }


{-|
  The Order of Edit Controls in the UI
-}
editModes : List EditMode
editModes =
    [ EditArrow
    , EditOval
    , EditTextBox
    , Select
    ]


initialEditState : EditState
initialEditState =
    { arrows = []
    , ovals = []
    , textBoxes = []
    , drawing = Selection
    , fill = Color.red
    }


init : ( Model, List (Cmd Msg) )
init =
    { edits = UndoList.fresh initialEditState
    , mouse = Mouse.Position 0 0
    }
        => []



-- UPDATE


type Msg
    = StartArrow Mouse.Position
    | AddArrow Mouse.Position Mouse.Position
    | StartOval Mouse.Position
    | AddOval Mouse.Position Mouse.Position
    | StartTextBox Mouse.Position
    | PlaceTextBox Mouse.Position Mouse.Position
    | TextBoxInput TextMode String
    | AddTextBox TextMode
    | SetMouse Mouse.Position
    | ChangeDrawing Drawing
    | SelectColor Color
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
                    | arrows = Arrow startPos endPos editState.fill :: editState.arrows
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
                    | ovals = Oval startPos endPos editState.fill :: editState.ovals
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
                                    | textBoxes = TextBox startPos endPos text editState.fill :: editState.textBoxes
                                    , drawing = DrawTextBox NoText
                                }

                            _ ->
                                editState
                in
                    newEditState
                        |> skipChange model
                        => []

            SetMouse pos ->
                { model | mouse = pos }
                    => []

            ChangeDrawing drawing ->
                { editState | drawing = drawing }
                    |> skipChange model
                    => []

            SelectColor color ->
                { editState | fill = color }
                    |> logChange model
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
logChange model editState =
    { model | edits = UndoList.new editState model.edits }


{-| Do not add this editState change to app history
-}
skipChange model editState =
    { model | edits = UndoList.mapPresent (\_ -> editState) model.edits }


updateMouse pos model =
    { model | mouse = pos }



-- VIEW


view : Model -> Html Msg
view ({ edits, mouse } as model) =
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
                                , style [ ( "opacity", "0" ) ]
                                ]
                                [ Html.text text ]
                            ]

                        _ ->
                            []

                _ ->
                    []
    in
        div [ id "annotation-app" ]
            ([ viewCanvas editState mouse
             , viewControls editState
             , viewColorSelection editState
             , button [ Html.Events.onClick Export ] [ text "Export" ]
             , p [] [ text <| toString <| model ]
             ]
                ++ offscreenInput
            )


viewControls editState =
    div []
        (button [ Html.Events.onClick Reset ] [ text "Reset" ]
            :: button [ Html.Events.onClick Undo ] [ text "Undo" ]
            :: button [ Html.Events.onClick Redo ] [ text "Redo" ]
            :: List.map (viewEditOption editState) editModes
        )


viewColorSelection editState =
    [ Color.red
    , Color.orange
    , Color.yellow
    , Color.green
    , Color.blue
    , Color.purple
    , Color.brown
    ]
        |> List.map (viewColorOption editState.fill)
        |> div []


viewColorOption selectedColor color =
    button
        [ style
            ([ "width" => "20px"
             , "height" => "20px"
             , "background-color" => Color.Convert.colorToHex color
             ]
                ++ (if selectedColor == color then
                        [ "border" => "1px solid black" ]
                    else
                        []
                   )
            )
        , Html.Events.onClick (SelectColor color)
        ]
        []


viewEditOption editState editMode =
    let
        buttonStyle =
            style <|
                if drawingToEditMode editState.drawing == editMode then
                    [ ( "background-color", "cyan" )
                    , ( "color", "white" )
                    ]
                else
                    []
    in
        button
            [ Html.Events.onClick (ChangeDrawing <| editToDrawing editMode)
            , buttonStyle
            ]
            [ text <| modeToString editMode ]


viewCanvas : EditState -> Mouse.Position -> Html Msg
viewCanvas editState curMouse =
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
                            [ Html.Events.onClick <| AddTextBox <| EditingTextMode startPos endPos text ]

                Selection ->
                    []

        currentDrawing =
            viewDrawing editState curMouse

        arrows =
            List.map viewArrow editState.arrows

        ovals =
            List.map viewOval editState.ovals

        textBoxes =
            List.map viewTextBox editState.textBoxes

        forms =
            List.concat
                [ [ toForm viewImage
                  , currentDrawing
                  ]
                , arrows
                , ovals
                , textBoxes
                ]
    in
        forms
            |> collage 300 200
            |> toHtml
            |> List.singleton
            |> div (attrs ++ [ id "canvas" ])


viewDrawing { drawing, fill } mouse =
    case drawing of
        DrawArrow arrowDrawing ->
            case arrowDrawing of
                NoArrow ->
                    toForm Element.empty

                DrawingArrow pos ->
                    Arrow pos mouse fill
                        |> viewArrow

        DrawOval ovalDrawing ->
            case ovalDrawing of
                NoOval ->
                    toForm Element.empty

                DrawingOval pos ->
                    Oval pos mouse fill
                        |> viewOval

        DrawTextBox textBoxDrawing ->
            case textBoxDrawing of
                NoText ->
                    toForm Element.empty

                DrawingTextBox pos ->
                    TextBox pos mouse "" fill
                        |> viewTextBox

                EditingTextMode startPos endPos text ->
                    TextBox startPos endPos text fill
                        |> viewTextBox

        Selection ->
            toForm Element.empty


viewArrow : Arrow -> Collage.Form
viewArrow ({ start, end, fill } as arrow) =
    let
        lineStyle =
            { defaultLine | width = 10, color = fill }
    in
        Collage.group
            [ Collage.ngon 3 15
                |> filled fill
                |> rotate (arrowAngle arrow)
                |> rotate (degrees 90)
                |> move (mouseOffset start)
            , Collage.segment (mouseOffset start) (mouseOffset end)
                |> Collage.traced lineStyle
            ]


viewOval : Oval -> Collage.Form
viewOval ({ start, end, fill } as oval) =
    let
        lineStyle =
            { defaultLine | color = fill }

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


viewTextBox : TextBox -> Collage.Form
viewTextBox ({ start, end, text, fill } as textBox) =
    let
        delta =
            end.x
                - start.x
                |> toFloat

        ( offsetX, offsetY ) =
            mouseOffset start
    in
        Collage.group
            [ Collage.rect (toFloat (end.x - start.x)) (toFloat (end.y - start.y))
                |> Collage.outlined (Collage.dotted fill)
            , Collage.text <| Text.fromString text
            ]
            |> Collage.move ( offsetX, offsetY )
            |> Collage.move ( (delta / 2), (toFloat <| start.y - end.y) )


viewImage : Element.Element
viewImage =
    image 300 200 "photo.jpg"



-- HELPERS


onClick toMsg =
    on "click" toMsg


handleTextBoxInputKey submitMsg code =
    if code == 13 then
        Ok submitMsg
    else if code == 27 then
        Ok Undo
    else
        Err "not handling that key"


fromKeyResult result =
    case result of
        Ok msg ->
            Json.succeed msg

        Err errMsg ->
            Json.fail errMsg


decodeTextInputKey submitMsg =
    Json.map (handleTextBoxInputKey submitMsg) keyCode
        |> Json.andThen fromKeyResult


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


modeToString mode =
    case mode of
        EditArrow ->
            "Arrow"

        EditOval ->
            "Oval"

        EditTextBox ->
            "Text"

        Select ->
            "Select"


editToDrawing editMode =
    case editMode of
        EditArrow ->
            DrawArrow NoArrow

        EditOval ->
            DrawOval NoOval

        EditTextBox ->
            DrawTextBox NoText

        Select ->
            Selection


drawingToEditMode drawing =
    case drawing of
        DrawArrow _ ->
            EditArrow

        DrawOval _ ->
            EditOval

        DrawTextBox _ ->
            EditTextBox

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

        Selection ->
            False



-- PORTS


port exportToImage : String -> Cmd msg



-- SUBSCRIPTIONS


subscriptions model =
    Sub.batch
        [ if trackMouse model.edits.present then
            Mouse.moves SetMouse
          else
            Sub.none
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
