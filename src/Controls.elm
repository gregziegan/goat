module Controls exposing (Config, Dropdown, DropdownTrigger(..), DropdownType(..), Msg(..), State, closeDropdown, defaultDrawingStyles, initialState, onKeyDown, subscriptions, update, view)

import Color exposing (Color)
import Drawing exposing (Drawing(..), LineType(..), ShapeType(..))
import Drawing.Options as Drawing exposing (DrawingStyles, Fill, FontSize, StrokeColor, StrokeStyle(..))
import Environment exposing (OperatingSystem(..))
import EventUtils exposing (stopPropagationAndDefault)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList, title)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Icons
import Json.Decode as Decode
import Keyboard exposing (Key(..))
import Palette
import Time


type DropdownTrigger
    = DelayedDropdown { waiting : Bool }
    | ImmediateDropdown


type DropdownType
    = ShapesDropdown
    | SpotlightsDropdown
    | Fonts
    | Fills
    | StrokeColors
    | Strokes


type alias Dropdown =
    { trigger : DropdownTrigger
    , kind : DropdownType
    }


type alias State =
    { currentDropdown : Maybe Dropdown
    , drawing : Drawing
    , shape : Drawing
    , spotlight : Drawing
    , drawingStyles : DrawingStyles
    }


type alias Config =
    { styles : DrawingStyles
    , os : OperatingSystem
    }


type alias DropdownConfig selection =
    { toTitle : selection -> String
    , selected : selection
    , render : DropdownType -> Html Msg
    }


initialState : State
initialState =
    { drawing = DrawLine Arrow
    , shape = DrawShape RoundedRect
    , spotlight = DrawSpotlight RoundedRect
    , drawingStyles = defaultDrawingStyles
    , currentDropdown = Nothing
    }


defaultDrawingStyles : DrawingStyles
defaultDrawingStyles =
    DrawingStyles Palette.purple Nothing SolidMedium 20


toStrokeStylesTitle : OperatingSystem -> String
toStrokeStylesTitle os =
    case os of
        MacOS ->
            "Stroke Styles (S)"

        Windows ->
            "S̲troke Styles"


toFillsTitle : OperatingSystem -> String
toFillsTitle os =
    case os of
        MacOS ->
            "Fills (F)"

        Windows ->
            "F̲ills"


toStrokeColorsTitle : OperatingSystem -> String
toStrokeColorsTitle os =
    case os of
        MacOS ->
            "Stroke Colors (K)"

        Windows ->
            "Strok̲e Colors"


toFontSizeTitle : OperatingSystem -> String
toFontSizeTitle os =
    case os of
        MacOS ->
            "Font Sizes (N)"

        Windows ->
            "Foṉt Sizes"



-- UPDATE


type Msg
    = ChangeDrawing Drawing
    | SelectFill (Maybe Color)
    | SelectStrokeColor Color
    | SelectStrokeStyle StrokeStyle
    | SelectFontSize Int
    | WaitForDropdownToOpen Dropdown
    | CancelDropdownWait
    | ToggleDropdown Dropdown


update : Msg -> State -> State
update msg state =
    case msg of
        SelectFill newFill ->
            state
                |> updateStyles (setFill newFill state.drawingStyles)
                |> closeDropdown

        SelectStrokeColor newStrokeColor ->
            state
                |> updateStyles (setStrokeColor newStrokeColor state.drawingStyles)
                |> closeDropdown

        SelectStrokeStyle newStrokeStyle ->
            state
                |> updateStyles (setStrokeStyle newStrokeStyle state.drawingStyles)
                |> closeDropdown

        SelectFontSize newFontSize ->
            state
                |> updateStyles (setFontSize newFontSize state.drawingStyles)
                |> closeDropdown

        WaitForDropdownToOpen dropdown ->
            waitForDropdownOpen dropdown state

        CancelDropdownWait ->
            cancelDelayedDropdown state

        ToggleDropdown dropdown ->
            toggleDropdown dropdown state

        ChangeDrawing newDrawing ->
            state
                |> changeDrawing newDrawing
                |> closeDropdown


onKeyDown : Key -> State -> State
onKeyDown key state =
    case key of
        Character "A" ->
            { state | drawing = DrawLine Arrow }

        Character "C" ->
            { state | drawing = DrawSpotlight RoundedRect, spotlight = DrawSpotlight RoundedRect }

        Character "H" ->
            { state | drawing = DrawFreeHand }

        Character "L" ->
            { state | drawing = DrawLine StraightLine }

        Character "R" ->
            { state | drawing = DrawShape Rect, shape = DrawShape Rect }

        Character "O" ->
            { state | drawing = DrawShape RoundedRect, shape = DrawShape RoundedRect }

        Character "E" ->
            { state | drawing = DrawShape Ellipse, shape = DrawShape Ellipse }

        Character "T" ->
            { state | drawing = DrawTextBox }

        Character "G" ->
            { state | drawing = DrawSpotlight Rect, spotlight = DrawSpotlight Rect }

        Character "I" ->
            { state | drawing = DrawSpotlight Ellipse, spotlight = DrawSpotlight Ellipse }

        Character "P" ->
            { state | drawing = DrawPixelate }

        Character "N" ->
            toggleDropdown (dropdownFromType Fonts) state

        Character "K" ->
            toggleDropdown (dropdownFromType StrokeColors) state

        Character "F" ->
            toggleDropdown (dropdownFromType Fills) state

        Character "S" ->
            toggleDropdown (dropdownFromType Strokes) state

        _ ->
            state



-- SELECTION


changeDrawing : Drawing -> State -> State
changeDrawing drawing state =
    { state | drawing = drawing }
        |> changeShapeAndSpotlightDropdowns drawing


updateStyles : DrawingStyles -> State -> State
updateStyles styles state =
    { state | drawingStyles = styles }


setFill : Maybe Color -> DrawingStyles -> DrawingStyles
setFill fill styles =
    { styles
        | fill = fill
    }


setFontSize : Int -> DrawingStyles -> DrawingStyles
setFontSize fontSize styles =
    { styles
        | fontSize = fontSize
    }


setStrokeStyle : StrokeStyle -> DrawingStyles -> DrawingStyles
setStrokeStyle strokeStyle styles =
    { styles
        | strokeStyle = strokeStyle
    }


setStrokeColor : Color -> DrawingStyles -> DrawingStyles
setStrokeColor strokeColor styles =
    { styles
        | strokeColor = strokeColor
    }


closeDropdown : State -> State
closeDropdown state =
    { state | currentDropdown = Nothing }


dropdownFromType : DropdownType -> Dropdown
dropdownFromType dropdownType =
    let
        fromTrigger trigger =
            { kind = dropdownType, trigger = trigger }
    in
    case dropdownType of
        ShapesDropdown ->
            fromTrigger (DelayedDropdown { waiting = False })

        SpotlightsDropdown ->
            fromTrigger (DelayedDropdown { waiting = False })

        StrokeColors ->
            fromTrigger ImmediateDropdown

        Fills ->
            fromTrigger ImmediateDropdown

        Strokes ->
            fromTrigger ImmediateDropdown

        Fonts ->
            fromTrigger ImmediateDropdown


toggleDropdown : Dropdown -> State -> State
toggleDropdown dropdown state =
    { state
        | currentDropdown =
            case state.currentDropdown of
                Just selected ->
                    if selected == dropdown then
                        Nothing

                    else
                        Just dropdown

                Nothing ->
                    Just dropdown
        , drawing =
            case dropdown.kind of
                ShapesDropdown ->
                    state.shape

                SpotlightsDropdown ->
                    state.spotlight

                Fonts ->
                    DrawTextBox

                _ ->
                    state.drawing
    }


waitForDropdownOpen : Dropdown -> State -> State
waitForDropdownOpen dropdown state =
    { state
        | currentDropdown = Just (setWaiting True dropdown)
        , drawing =
            case dropdown.kind of
                ShapesDropdown ->
                    state.shape

                SpotlightsDropdown ->
                    state.spotlight

                _ ->
                    state.drawing
    }


setWaiting : Bool -> Dropdown -> Dropdown
setWaiting waiting dropdown =
    { dropdown
        | trigger =
            case dropdown.trigger of
                DelayedDropdown _ ->
                    DelayedDropdown { waiting = waiting }

                ImmediateDropdown ->
                    dropdown.trigger
    }


cancelDelayedDropdown : State -> State
cancelDelayedDropdown state =
    { state
        | currentDropdown =
            case state.currentDropdown of
                Just dropdown ->
                    case dropdown.trigger of
                        DelayedDropdown { waiting } ->
                            Nothing

                        ImmediateDropdown ->
                            state.currentDropdown

                Nothing ->
                    state.currentDropdown
    }


changeShapeAndSpotlightDropdowns : Drawing -> State -> State
changeShapeAndSpotlightDropdowns drawing state =
    case drawing of
        DrawLine lineType ->
            case lineType of
                Arrow ->
                    state

                StraightLine ->
                    { state | shape = drawing }

        DrawFreeHand ->
            state

        DrawShape _ ->
            { state | shape = drawing }

        DrawTextBox ->
            state

        DrawSpotlight _ ->
            { state | spotlight = drawing }

        DrawPixelate ->
            state


viewDropdownMenu : Config -> State -> DropdownType -> Html Msg
viewDropdownMenu config { currentDropdown, drawing } selection =
    case currentDropdown of
        Just dropdown ->
            if selection == dropdown.kind then
                viewDropdown config.styles drawing dropdown.kind

            else
                text ""

        Nothing ->
            text ""



-- VIEW


dropdownEvents : Drawing -> Dropdown -> List (Html.Attribute Msg)
dropdownEvents drawing dropdown =
    case dropdown.trigger of
        DelayedDropdown _ ->
            [ stopPropagationAndDefault "contextmenu" (Decode.succeed (ChangeDrawing drawing))
            , onMouseDown (WaitForDropdownToOpen dropdown)
            , onMouseUp CancelDropdownWait
            ]

        ImmediateDropdown ->
            [ Html.Events.onClick (ToggleDropdown dropdown)
            ]


shapesDropdown : DropdownConfig Drawing -> Drawing -> Html Msg
shapesDropdown config drawing =
    div [ class "dropdown-things" ]
        [ Html.button
            ([ classList
                [ ( "drawing-button", True )
                , ( "drawing-button--selected", Drawing.equal config.selected drawing )
                ]
             , title (config.toTitle drawing)
             ]
                ++ dropdownEvents drawing (dropdownFromType ShapesDropdown)
            )
            [ Drawing.icon drawing
            , Icons.viewCornerArrow
            ]
        , config.render ShapesDropdown
        ]


spotlightsDropdown : DropdownConfig Drawing -> Drawing -> Html Msg
spotlightsDropdown config drawing =
    div [ class "dropdown-things" ]
        [ Html.button
            ([ classList
                [ ( "drawing-button", True )
                , ( "drawing-button--selected", Drawing.equal config.selected drawing )
                , ( "drawing-button--spotlight", True )
                ]
             , title (config.toTitle drawing)
             ]
                ++ dropdownEvents drawing (dropdownFromType SpotlightsDropdown)
            )
            [ Drawing.icon drawing
            , Icons.viewCornerArrow
            ]
        , config.render SpotlightsDropdown
        ]


placeholderDrawing : Drawing
placeholderDrawing =
    -- TODO: make this type better so we don't need this
    DrawPixelate


strokeColorDropdown : DropdownConfig () -> Color -> Html Msg
strokeColorDropdown config strokeColor =
    div [ class "dropdown-things" ]
        [ Html.button
            ([ class "drawing-button"
             , title (config.toTitle ())
             ]
                ++ dropdownEvents placeholderDrawing (dropdownFromType StrokeColors)
            )
            [ Icons.viewStrokeColor strokeColor
            ]
        , config.render StrokeColors
        ]


strokeStylesDropdown : DropdownConfig () -> StrokeStyle -> Html Msg
strokeStylesDropdown config strokeStyle =
    div [ class "dropdown-things" ]
        [ Html.button
            ([ class "drawing-button"
             , title (config.toTitle ())
             ]
                ++ dropdownEvents placeholderDrawing (dropdownFromType Strokes)
            )
            [ Icons.viewStrokeStyle strokeStyle
            ]
        , config.render Strokes
        ]


fillsDropdown : DropdownConfig () -> Maybe Color -> Html Msg
fillsDropdown config fill =
    div [ class "dropdown-things" ]
        [ Html.button
            ([ class "drawing-button"
             , title (config.toTitle ())
             ]
                ++ dropdownEvents placeholderDrawing (dropdownFromType Fills)
            )
            [ Icons.viewFill fill
            ]
        , config.render Fills
        ]


fontSizesDropdown : DropdownConfig () -> Html Msg
fontSizesDropdown config =
    div [ class "dropdown-things" ]
        [ Html.button
            ([ class "drawing-button"
             , title (config.toTitle ())
             ]
                ++ dropdownEvents placeholderDrawing (dropdownFromType Fonts)
            )
            [ Icons.viewFontSize
            ]
        , config.render Fonts
        ]


button : { toTitle : Drawing -> String, selected : Drawing } -> Drawing -> Html Msg
button config drawing =
    Html.button
        [ classList
            [ ( "drawing-button", True )
            , ( "drawing-button--selected", Drawing.equal config.selected drawing )
            , ( "drawing-button--spotlight", Drawing.isSpotlight drawing )
            ]
        , title (config.toTitle drawing)
        , onClick (ChangeDrawing drawing)
        ]
        [ Drawing.icon drawing ]


viewFontSizeOptions : FontSize -> Html Msg
viewFontSizeOptions fontSize =
    Drawing.fontSizes
        |> List.map (viewFontSizeOption fontSize)
        |> div [ class "dropdown-options" ]


viewFillOptions : Fill -> Html Msg
viewFillOptions fill =
    Drawing.fills
        |> List.map (viewFillOption fill)
        |> div [ class "dropdown-options" ]


viewStrokeColorOptions : StrokeColor -> Html Msg
viewStrokeColorOptions strokeColor =
    Drawing.strokeColors
        |> List.map (viewStrokeColorOption strokeColor)
        |> div [ class "dropdown-options" ]


viewFillOption : Fill -> Fill -> Html Msg
viewFillOption selectedFill fill =
    Html.button
        [ classList
            [ ( "dropdown-button", True )
            , ( "dropdown-button--selected", selectedFill == fill )
            ]
        , onClick (SelectFill fill)
        ]
        [ Icons.viewFill fill ]


viewStrokeColorOption : StrokeColor -> StrokeColor -> Html Msg
viewStrokeColorOption selectedColor color =
    Html.button
        [ classList
            [ ( "dropdown-button", True )
            , ( "dropdown-button--selected", selectedColor == color )
            ]
        , onClick (SelectStrokeColor color)
        ]
        [ Icons.viewStrokeColor color ]


viewFontSizeOption : FontSize -> FontSize -> Html Msg
viewFontSizeOption selectedFontSize fontSize =
    Html.button
        [ classList
            [ ( "dropdown-button", True )
            , ( "dropdown-button--selected", selectedFontSize == fontSize )
            ]
        , onClick (SelectFontSize fontSize)
        ]
        [ text <| String.fromInt <| fontSize ]


viewStrokeStyleOptions : StrokeStyle -> Html Msg
viewStrokeStyleOptions strokeStyle =
    Drawing.strokeStyles
        |> List.map (viewStrokeStyleOption strokeStyle)
        |> div [ class "dropdown-options" ]


viewStrokeStyleOption : StrokeStyle -> StrokeStyle -> Html Msg
viewStrokeStyleOption selectedStrokeStyle strokeStyle =
    Html.button
        [ classList
            [ ( "dropdown-button", True )
            , ( "dropdown-button--selected", selectedStrokeStyle == strokeStyle )
            ]
        , onClick (SelectStrokeStyle strokeStyle)
        ]
        [ Icons.viewStrokeStyle strokeStyle ]


viewDropdown : DrawingStyles -> Drawing -> DropdownType -> Html Msg
viewDropdown { fill, strokeColor, strokeStyle, fontSize } drawing dropdown =
    case dropdown of
        ShapesDropdown ->
            viewShapesOptions drawing

        SpotlightsDropdown ->
            viewSpotlightsOptions drawing

        Fonts ->
            viewFontSizeOptions fontSize

        Fills ->
            viewFillOptions fill

        StrokeColors ->
            viewStrokeColorOptions strokeColor

        Strokes ->
            viewStrokeStyleOptions strokeStyle


viewShapesOptions : Drawing -> Html Msg
viewShapesOptions drawing =
    Drawing.shapes
        |> List.map (viewDrawingOption drawing)
        |> div [ class "dropdown-options" ]


viewDrawingOption : Drawing -> Drawing -> Html Msg
viewDrawingOption selectedDrawing drawing =
    Html.button
        [ classList
            [ ( "dropdown-button", True )
            , ( "dropdown-button--selected", selectedDrawing == drawing )
            , ( "dropdown-button--spotlight", List.member drawing Drawing.spotlights )
            ]
        , onClick (ChangeDrawing drawing)
        ]
        [ Drawing.icon drawing ]


viewSpotlightsOptions : Drawing -> Html Msg
viewSpotlightsOptions drawing =
    Drawing.spotlights
        |> List.map (viewDrawingOption drawing)
        |> div [ class "dropdown-options" ]


view : Config -> State -> Html Msg
view config ({ drawing, shape, spotlight } as state) =
    let
        { strokeColor, fill, strokeStyle } =
            config.styles

        toTitle =
            Drawing.toString config.os

        toDropdown =
            viewDropdownMenu config state

        buttonConfig =
            { toTitle = toTitle, selected = drawing }

        drawingDropdownConfig =
            { toTitle = toTitle
            , selected = drawing
            , render = toDropdown
            }

        toDropdownConfig title =
            { toTitle = always title
            , selected = ()
            , render = toDropdown
            }

        strokeColorsConfig =
            toDropdownConfig (toStrokeColorsTitle config.os)

        fillConfig =
            toDropdownConfig (toFillsTitle config.os)

        strokeStyleConfig =
            toDropdownConfig (toStrokeStylesTitle config.os)

        fontSizeConfig =
            toDropdownConfig (toFontSizeTitle config.os)
    in
    div [ class "columns" ]
        [ button buttonConfig (DrawLine Arrow)
        , button buttonConfig DrawFreeHand
        , button buttonConfig DrawTextBox
        , shapesDropdown drawingDropdownConfig shape
        , spotlightsDropdown drawingDropdownConfig spotlight
        , button buttonConfig DrawPixelate
        , strokeColorDropdown strokeColorsConfig strokeColor
        , fillsDropdown fillConfig fill
        , strokeStylesDropdown strokeStyleConfig strokeStyle
        , fontSizesDropdown fontSizeConfig
        ]


subscriptions : State -> Sub Msg
subscriptions state =
    case state.currentDropdown of
        Nothing ->
            Sub.none

        Just dropdown ->
            case dropdown.trigger of
                DelayedDropdown { waiting } ->
                    if waiting then
                        Time.every 200 (\_ -> ToggleDropdown dropdown)

                    else
                        Sub.none

                ImmediateDropdown ->
                    Sub.none
