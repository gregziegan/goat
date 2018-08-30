module Controls exposing (Config, Msg, State, closeDropdown, getDrawing, initialState, onKeyDown, styles, subscriptions, update, view)

import Color exposing (Color)
import Drawing exposing (AttributeDropdown(..), Drawing(..), LineType(..), ShapeType(..))
import Drawing.Options as Drawing exposing (DrawingStyles, Fill, FontSize, StrokeColor, StrokeStyle(..))
import Environment exposing (OperatingSystem(..))
import EventUtils exposing (stopPropagationAndDefault)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList, title)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Icons
import Json.Decode as Decode
import Keyboard exposing (Key(..))
import Time


type DropdownTrigger
    = DelayedDropdown
    | ImmediateDropdown


type alias State =
    { currentDropdown : Maybe AttributeDropdown
    , drawing : Drawing
    , shape : Drawing
    , spotlight : Drawing
    , fill : Maybe Color
    , strokeColor : Color
    , strokeStyle : StrokeStyle
    , fontSize : Int
    , waitingForDropdownToggle : Maybe AttributeDropdown
    }


type alias Config =
    { styles : DrawingStyles
    , os : OperatingSystem
    }


type alias DropdownConfig selection =
    { toTitle : selection -> String
    , selected : selection
    , trigger : DropdownTrigger
    , render : AttributeDropdown -> Html Msg
    }


type alias DrawOptions =
    { drawing : Drawing
    , styles : DrawingStyles
    }


initialState : State
initialState =
    { drawing = Drawing.default
    , shape = Drawing.defaultShape
    , spotlight = Drawing.defaultSpotlight
    , waitingForDropdownToggle = Nothing
    , fill = Drawing.defaults.fill
    , strokeColor = Drawing.defaults.strokeColor
    , strokeStyle = Drawing.defaults.strokeStyle
    , fontSize = Drawing.defaults.fontSize
    , currentDropdown = Nothing
    }


getDrawing : State -> Drawing
getDrawing state =
    state.drawing


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


styles : State -> DrawingStyles
styles { strokeColor, fill, strokeStyle, fontSize } =
    DrawingStyles strokeColor fill strokeStyle fontSize



-- UPDATE


type Msg
    = ChangeDrawing Drawing
    | SelectFill (Maybe Color)
    | SelectStrokeColor Color
    | SelectStrokeStyle StrokeStyle
    | SelectFontSize Int
    | WaitForDropdownToggle AttributeDropdown
    | CancelDropdownWait
    | ToggleDropdown AttributeDropdown


update : Msg -> State -> ( State, DrawOptions )
update msg state =
    let
        newState =
            case msg of
                SelectFill newFill ->
                    state
                        |> setFill newFill
                        |> closeDropdown

                SelectStrokeColor newStrokeColor ->
                    state
                        |> setStrokeColor newStrokeColor
                        |> closeDropdown

                SelectStrokeStyle newStrokeStyle ->
                    state
                        |> setStrokeStyle newStrokeStyle
                        |> closeDropdown

                SelectFontSize newFontSize ->
                    state
                        |> setFontSize newFontSize
                        |> closeDropdown

                WaitForDropdownToggle attributeDropdown ->
                    state
                        |> waitForDropdownToggle attributeDropdown
                        |> closeDropdown

                CancelDropdownWait ->
                    state
                        |> cancelWaitForDropdownToggle

                ToggleDropdown editOption ->
                    state
                        |> toggleDropdown editOption
                        |> cancelWaitForDropdownToggle

                ChangeDrawing newDrawing ->
                    state
                        |> changeDrawing newDrawing
                        |> closeDropdown
    in
    ( newState, DrawOptions newState.drawing (styles newState) )


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
            toggleDropdown Fonts state

        Character "K" ->
            toggleDropdown StrokeColors state

        Character "F" ->
            toggleDropdown Fills state

        Character "S" ->
            toggleDropdown Strokes state

        _ ->
            state



-- SELECTION


changeDrawing : Drawing -> State -> State
changeDrawing drawing state =
    { state | drawing = drawing }
        |> changeShapeAndSpotlightDropdowns drawing


setFill : Maybe Color -> State -> State
setFill fill state =
    { state
        | fill = fill
    }


setFontSize : Int -> State -> State
setFontSize fontSize state =
    { state
        | fontSize = fontSize
    }


setStrokeStyle : StrokeStyle -> State -> State
setStrokeStyle strokeStyle state =
    { state
        | strokeStyle = strokeStyle
    }


setStrokeColor : Color -> State -> State
setStrokeColor strokeColor state =
    { state
        | strokeColor = strokeColor
    }


closeDropdown : State -> State
closeDropdown state =
    { state | currentDropdown = Nothing }


toggleDropdown : AttributeDropdown -> State -> State
toggleDropdown attributeDropdown state =
    { state
        | currentDropdown =
            case state.currentDropdown of
                Just dropdown ->
                    if dropdown == attributeDropdown then
                        Nothing

                    else
                        Just attributeDropdown

                Nothing ->
                    Just attributeDropdown
        , drawing =
            case attributeDropdown of
                ShapesDropdown ->
                    state.shape

                SpotlightsDropdown ->
                    state.spotlight

                Fonts ->
                    DrawTextBox

                _ ->
                    state.drawing
    }


waitForDropdownToggle : AttributeDropdown -> State -> State
waitForDropdownToggle attributeDropdown state =
    { state
        | waitingForDropdownToggle = Just attributeDropdown
        , drawing =
            case attributeDropdown of
                ShapesDropdown ->
                    state.shape

                SpotlightsDropdown ->
                    state.spotlight

                _ ->
                    state.drawing
    }


cancelWaitForDropdownToggle : State -> State
cancelWaitForDropdownToggle state =
    { state | waitingForDropdownToggle = Nothing }


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


viewDropdownMenu : Config -> State -> AttributeDropdown -> Html Msg
viewDropdownMenu config { currentDropdown, drawing } selection =
    case currentDropdown of
        Just dropdown ->
            if selection == dropdown then
                viewDropdown config.styles drawing dropdown

            else
                text ""

        Nothing ->
            text ""



-- VIEW


dropdownEvents : Drawing -> AttributeDropdown -> DropdownTrigger -> List (Html.Attribute Msg)
dropdownEvents drawing attributeDropdown dropdownTrigger =
    case dropdownTrigger of
        DelayedDropdown ->
            [ stopPropagationAndDefault "contextmenu" (Decode.succeed (ChangeDrawing drawing))
            , onMouseDown (WaitForDropdownToggle attributeDropdown)
            , onMouseUp CancelDropdownWait
            ]

        ImmediateDropdown ->
            [ Html.Events.onClick (ToggleDropdown attributeDropdown)
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
                ++ dropdownEvents drawing ShapesDropdown DelayedDropdown
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
                ++ dropdownEvents drawing SpotlightsDropdown DelayedDropdown
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
                ++ dropdownEvents placeholderDrawing StrokeColors ImmediateDropdown
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
                ++ dropdownEvents placeholderDrawing Strokes ImmediateDropdown
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
                ++ dropdownEvents placeholderDrawing Fills ImmediateDropdown
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
                ++ dropdownEvents placeholderDrawing Fonts ImmediateDropdown
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


viewDropdown : DrawingStyles -> Drawing -> AttributeDropdown -> Html Msg
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
            , trigger = DelayedDropdown
            , render = toDropdown
            }

        toAttributeDropdownConfig title =
            { toTitle = always title
            , selected = ()
            , trigger = ImmediateDropdown
            , render = toDropdown
            }

        strokeColorsConfig =
            toAttributeDropdownConfig (toStrokeColorsTitle config.os)

        fillConfig =
            toAttributeDropdownConfig (toFillsTitle config.os)

        strokeStyleConfig =
            toAttributeDropdownConfig (toStrokeStylesTitle config.os)

        fontSizeConfig =
            toAttributeDropdownConfig (toFontSizeTitle config.os)
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
    case state.waitingForDropdownToggle of
        Nothing ->
            Sub.none

        Just attributeDropdown ->
            Time.every 200 (\_ -> ToggleDropdown attributeDropdown)
