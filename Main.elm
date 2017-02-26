port module Main exposing (main)

import Collage exposing (collage, defaultLine, filled, move, rotate, toForm)
import Color
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
    }


type alias Oval =
    { start : Mouse.Position
    , end : Mouse.Position
    }


type alias TextBox =
    { start : Mouse.Position
    , end : Mouse.Position
    , text : String
    }


type alias Model =
    { arrows : List Arrow
    , ovals : List Oval
    , textBoxes : List TextBox
    , drawing : Drawing
    , mouse : Mouse.Position
    }


type alias TimeTravel =
    UndoList Model


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


initialModel : Model
initialModel =
    { arrows = []
    , ovals = []
    , textBoxes = []
    , drawing = Selection
    , mouse = Mouse.Position 0 0
    }


init : ( TimeTravel, List (Cmd Msg) )
init =
    UndoList.fresh initialModel
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
    | Undo
    | Redo
    | Reset
    | Export


update : Msg -> TimeTravel -> ( TimeTravel, List (Cmd Msg) )
update msg ({ present } as timeTravel) =
    let
        model =
            present
    in
        case msg of
            StartArrow pos ->
                { model | drawing = DrawArrow (DrawingArrow pos) }
                    |> logChange timeTravel
                    => []

            AddArrow startPos endPos ->
                { model
                    | arrows = Arrow startPos endPos :: model.arrows
                    , drawing = DrawArrow NoArrow
                }
                    |> skipChange timeTravel
                    => []

            StartOval pos ->
                { model | drawing = DrawOval (DrawingOval pos) }
                    |> logChange timeTravel
                    => []

            AddOval startPos endPos ->
                { model
                    | ovals = Oval startPos endPos :: model.ovals
                    , drawing = DrawOval NoOval
                }
                    |> skipChange timeTravel
                    => []

            StartTextBox pos ->
                { model | drawing = DrawTextBox (DrawingTextBox pos) }
                    |> logChange timeTravel
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
                    { model | drawing = DrawTextBox initialEdit }
                        |> skipChange timeTravel
                        => [ Dom.focus "text-box-edit"
                                |> Task.attempt tryToEdit
                           ]

            TextBoxInput textMode inputString ->
                let
                    newModel =
                        case textMode of
                            EditingTextMode startPos endPos text ->
                                { model | drawing = DrawTextBox (EditingTextMode startPos endPos inputString) }

                            _ ->
                                model
                in
                    newModel
                        |> skipChange timeTravel
                        => []

            AddTextBox textMode ->
                let
                    newModel =
                        case textMode of
                            EditingTextMode startPos endPos text ->
                                { model
                                    | textBoxes = TextBox startPos endPos text :: model.textBoxes
                                    , drawing = DrawTextBox NoText
                                }

                            _ ->
                                model
                in
                    newModel
                        |> skipChange timeTravel
                        => []

            SetMouse pos ->
                { model | mouse = pos }
                    |> skipChange timeTravel
                    => []

            ChangeDrawing drawing ->
                { model | drawing = drawing }
                    |> skipChange timeTravel
                    => []

            Undo ->
                UndoList.undo timeTravel
                    => []

            Redo ->
                UndoList.redo timeTravel
                    => []

            Reset ->
                UndoList.reset timeTravel
                    => []

            Export ->
                timeTravel
                    => [ exportToImage "annotation-app" ]


{-| Add this model change to app history
-}
logChange timeTravel model =
    UndoList.new model timeTravel


{-| Do not add this model change to app history
-}
skipChange timeTravel model =
    { timeTravel | present = model }



-- VIEW


view : TimeTravel -> Html Msg
view { present } =
    viewPresent present


viewPresent : Model -> Html Msg
viewPresent model =
    let
        options =
            { preventDefault = True, stopPropagation = False }

        offscreenInput =
            case model.drawing of
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
            ([ viewCanvas model
             , viewControls model
             , button [ Html.Events.onClick Export ] [ text "Export" ]
             , p [] [ text <| toString <| model ]
             ]
                ++ offscreenInput
            )


viewControls model =
    div []
        (button [ Html.Events.onClick Reset ] [ text "Reset" ]
            :: button [ Html.Events.onClick Undo ] [ text "Undo" ]
            :: button [ Html.Events.onClick Redo ] [ text "Redo" ]
            :: List.map (viewEditOption model) editModes
        )


viewEditOption model editMode =
    let
        buttonStyle =
            style <|
                if drawingToEditMode model.drawing == editMode then
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


viewCanvas : Model -> Html Msg
viewCanvas model =
    let
        attrs =
            case model.drawing of
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
            viewDrawing model.drawing model.mouse

        arrows =
            List.map viewArrow model.arrows

        ovals =
            List.map viewOval model.ovals

        textBoxes =
            List.map viewTextBox model.textBoxes

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


viewDrawing drawing mouse =
    case drawing of
        DrawArrow arrowDrawing ->
            case arrowDrawing of
                NoArrow ->
                    toForm Element.empty

                DrawingArrow pos ->
                    Arrow pos mouse
                        |> viewArrow

        DrawOval ovalDrawing ->
            case ovalDrawing of
                NoOval ->
                    toForm Element.empty

                DrawingOval pos ->
                    Oval pos mouse
                        |> viewOval

        DrawTextBox textBoxDrawing ->
            case textBoxDrawing of
                NoText ->
                    toForm Element.empty

                DrawingTextBox pos ->
                    TextBox pos mouse ""
                        |> viewTextBox

                EditingTextMode startPos endPos text ->
                    TextBox startPos endPos text
                        |> viewTextBox

        Selection ->
            toForm Element.empty


viewArrow : Arrow -> Collage.Form
viewArrow ({ start, end } as arrow) =
    let
        lineStyle =
            Collage.defaultLine
    in
        Collage.group
            [ Collage.ngon 3 15
                |> filled Color.black
                |> rotate (arrowAngle arrow)
                |> rotate (degrees 90)
                |> move (mouseOffset start)
            , Collage.segment (mouseOffset start) (mouseOffset end)
                |> Collage.traced { lineStyle | width = 10 }
            ]


viewOval : Oval -> Collage.Form
viewOval ({ start, end } as oval) =
    let
        delta =
            end.x
                - start.x
                |> toFloat

        ( offsetX, offsetY ) =
            mouseOffset start
    in
        Collage.oval (toFloat (end.x - start.x)) (toFloat (end.y - start.y))
            |> Collage.outlined Collage.defaultLine
            |> Collage.move ( offsetX, offsetY )
            |> Collage.moveX (delta / 2)


viewTextBox : TextBox -> Collage.Form
viewTextBox ({ start, end, text } as textBox) =
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
                |> Collage.outlined (Collage.dotted Color.black)
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



-- PORTS


port exportToImage : String -> Cmd msg



-- MAIN


main : Program Never TimeTravel Msg
main =
    Html.program
        { init = Rocket.batchInit init
        , update = update >> Rocket.batchUpdate
        , view = view
        , subscriptions = \model -> Mouse.moves SetMouse
        }
