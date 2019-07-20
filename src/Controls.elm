module Controls exposing (Config, Dropdown, DropdownTrigger(..), DropdownType(..), Msg(..), State, closeDropdown, initialState, onKeyDown, subscriptions, update, view)

import Annotation exposing (Annotation, Choice(..))
import Annotation.Options as Annotation exposing (AnnotationStyles, Fill, FontSize, StrokeColor, StrokeStyle(..))
import Color exposing (Color)
import Environment exposing (OperatingSystem(..))
import EventUtils exposing (stopPropagationAndDefault)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList, title)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Icons
import Json.Decode as Decode
import Keyboard exposing (Key(..))
import Palette
import Svg exposing (Svg)
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
    , annotation : Annotation.Choice
    , shape : Annotation.Choice
    , spotlight : Annotation.Choice
    , annotationStyles : AnnotationStyles
    }


type alias Config =
    { styles : AnnotationStyles
    , os : OperatingSystem
    }


type alias DropdownConfig selection =
    { toTitle : selection -> String
    , selected : selection
    , render : DropdownType -> Html Msg
    }


shapes : List Annotation.Choice
shapes =
    [ RoundedRectangle
    , Rectangle
    , Ellipse
    , StraightLine
    ]


spotlights : List Annotation.Choice
spotlights =
    [ SpotlightRoundedRectangle
    , SpotlightRectangle
    , SpotlightEllipse
    ]


initialState : State
initialState =
    { annotation = Arrow
    , shape = RoundedRectangle
    , spotlight = SpotlightRoundedRectangle
    , annotationStyles = Annotation.defaultStyles
    , currentDropdown = Nothing
    }


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
    = ChangeAnnotation Annotation.Choice
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
                |> updateStyles (setFill newFill state.annotationStyles)
                |> closeDropdown

        SelectStrokeColor newStrokeColor ->
            state
                |> updateStyles (setStrokeColor newStrokeColor state.annotationStyles)
                |> closeDropdown

        SelectStrokeStyle newStrokeStyle ->
            state
                |> updateStyles (setStrokeStyle newStrokeStyle state.annotationStyles)
                |> closeDropdown

        SelectFontSize newFontSize ->
            state
                |> updateStyles (setFontSize newFontSize state.annotationStyles)
                |> closeDropdown

        WaitForDropdownToOpen dropdown ->
            waitForDropdownOpen dropdown state

        CancelDropdownWait ->
            cancelDelayedDropdown state

        ToggleDropdown dropdown ->
            toggleDropdown dropdown state

        ChangeAnnotation newAnnotation ->
            state
                |> changeAnnotation newAnnotation
                |> closeDropdown


onKeyDown : Key -> State -> State
onKeyDown key state =
    case key of
        Character "A" ->
            { state | annotation = Arrow }

        Character "C" ->
            { state | annotation = SpotlightRoundedRectangle, spotlight = SpotlightRoundedRectangle }

        Character "H" ->
            { state | annotation = FreeHand }

        Character "L" ->
            { state | annotation = StraightLine }

        Character "R" ->
            { state | annotation = Rectangle, shape = Rectangle }

        Character "O" ->
            { state | annotation = RoundedRectangle, shape = RoundedRectangle }

        Character "E" ->
            { state | annotation = Ellipse, shape = Ellipse }

        Character "T" ->
            { state | annotation = TextBox }

        Character "G" ->
            { state | annotation = SpotlightRectangle, spotlight = SpotlightRectangle }

        Character "I" ->
            { state | annotation = SpotlightEllipse, spotlight = SpotlightEllipse }

        Character "P" ->
            { state | annotation = Pixelate }

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


changeAnnotation : Annotation.Choice -> State -> State
changeAnnotation annotation state =
    { state | annotation = annotation }
        |> changeShapeAndSpotlightDropdowns annotation


updateStyles : AnnotationStyles -> State -> State
updateStyles styles state =
    { state | annotationStyles = styles }


setFill : Maybe Color -> AnnotationStyles -> AnnotationStyles
setFill fill styles =
    { styles
        | fill = fill
    }


setFontSize : Int -> AnnotationStyles -> AnnotationStyles
setFontSize fontSize styles =
    { styles
        | fontSize = fontSize
    }


setStrokeStyle : StrokeStyle -> AnnotationStyles -> AnnotationStyles
setStrokeStyle strokeStyle styles =
    { styles
        | strokeStyle = strokeStyle
    }


setStrokeColor : Color -> AnnotationStyles -> AnnotationStyles
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
    let
        currentDropdown =
            Just (setWaiting False dropdown)
    in
    { state
        | currentDropdown =
            if state.currentDropdown == currentDropdown then
                Nothing

            else
                currentDropdown
        , annotation =
            case dropdown.kind of
                ShapesDropdown ->
                    state.shape

                SpotlightsDropdown ->
                    state.spotlight

                Fonts ->
                    TextBox

                _ ->
                    state.annotation
    }


waitForDropdownOpen : Dropdown -> State -> State
waitForDropdownOpen dropdown state =
    { state
        | currentDropdown = Just (setWaiting True dropdown)
        , annotation =
            case dropdown.kind of
                ShapesDropdown ->
                    state.shape

                SpotlightsDropdown ->
                    state.spotlight

                _ ->
                    state.annotation
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
                            if waiting then
                                Nothing

                            else
                                state.currentDropdown

                        ImmediateDropdown ->
                            state.currentDropdown

                Nothing ->
                    state.currentDropdown
    }


changeShapeAndSpotlightDropdowns : Annotation.Choice -> State -> State
changeShapeAndSpotlightDropdowns choice state =
    case choice of
        Arrow ->
            state

        StraightLine ->
            { state | shape = choice }

        FreeHand ->
            state

        Rectangle ->
            { state | shape = choice }

        RoundedRectangle ->
            { state | shape = choice }

        Ellipse ->
            { state | shape = choice }

        TextBox ->
            state

        SpotlightRectangle ->
            { state | spotlight = choice }

        SpotlightRoundedRectangle ->
            { state | spotlight = choice }

        SpotlightEllipse ->
            { state | spotlight = choice }

        Pixelate ->
            state


viewDropdownMenu : Config -> State -> DropdownType -> Html Msg
viewDropdownMenu config { currentDropdown, annotation } selection =
    case currentDropdown of
        Just dropdown ->
            case ( dropdown.kind == selection, dropdown.trigger ) of
                ( False, _ ) ->
                    text ""

                ( True, DelayedDropdown { waiting } ) ->
                    if waiting then
                        text ""

                    else
                        viewDropdown config.styles annotation dropdown.kind

                ( True, ImmediateDropdown ) ->
                    viewDropdown config.styles annotation dropdown.kind

        Nothing ->
            text ""



-- VIEW


dropdownEvents : Annotation.Choice -> Dropdown -> List (Html.Attribute Msg)
dropdownEvents annotation dropdown =
    case dropdown.trigger of
        DelayedDropdown _ ->
            [ stopPropagationAndDefault "contextmenu" (Decode.succeed (ChangeAnnotation annotation))
            , onMouseDown (WaitForDropdownToOpen dropdown)
            , onMouseUp CancelDropdownWait
            ]

        ImmediateDropdown ->
            [ Html.Events.onClick (ToggleDropdown dropdown)
            ]


shapesDropdown : DropdownConfig Annotation.Choice -> Annotation.Choice -> Html Msg
shapesDropdown config annotation =
    div [ class "dropdown-things" ]
        [ Html.button
            ([ classList
                [ ( "annotation-button", True )
                , ( "annotation-button--selected", config.selected == annotation )
                ]
             , title (config.toTitle annotation)
             ]
                ++ dropdownEvents annotation (dropdownFromType ShapesDropdown)
            )
            [ icon annotation
            , Icons.viewCornerArrow
            ]
        , config.render ShapesDropdown
        ]


spotlightsDropdown : DropdownConfig Annotation.Choice -> Annotation.Choice -> Html Msg
spotlightsDropdown config annotation =
    div [ class "dropdown-things" ]
        [ Html.button
            ([ classList
                [ ( "annotation-button", True )
                , ( "annotation-button--selected", config.selected == annotation )
                , ( "annotation-button--spotlight", True )
                ]
             , title (config.toTitle annotation)
             ]
                ++ dropdownEvents annotation (dropdownFromType SpotlightsDropdown)
            )
            [ icon annotation
            , Icons.viewCornerArrow
            ]
        , config.render SpotlightsDropdown
        ]


placeholderAnnotation : Annotation.Choice
placeholderAnnotation =
    -- TODO: make this type better so we don't need this
    Pixelate


strokeColorDropdown : DropdownConfig () -> Color -> Html Msg
strokeColorDropdown config strokeColor =
    div [ class "dropdown-things" ]
        [ Html.button
            ([ class "annotation-button"
             , title (config.toTitle ())
             ]
                ++ dropdownEvents placeholderAnnotation (dropdownFromType StrokeColors)
            )
            [ Icons.viewStrokeColor strokeColor
            ]
        , config.render StrokeColors
        ]


strokeStylesDropdown : DropdownConfig () -> StrokeStyle -> Html Msg
strokeStylesDropdown config strokeStyle =
    div [ class "dropdown-things" ]
        [ Html.button
            ([ class "annotation-button"
             , title (config.toTitle ())
             ]
                ++ dropdownEvents placeholderAnnotation (dropdownFromType Strokes)
            )
            [ Icons.viewStrokeStyle strokeStyle
            ]
        , config.render Strokes
        ]


fillsDropdown : DropdownConfig () -> Maybe Color -> Html Msg
fillsDropdown config fill =
    div [ class "dropdown-things" ]
        [ Html.button
            ([ class "annotation-button"
             , title (config.toTitle ())
             ]
                ++ dropdownEvents placeholderAnnotation (dropdownFromType Fills)
            )
            [ Icons.viewFill fill
            ]
        , config.render Fills
        ]


fontSizesDropdown : DropdownConfig () -> Html Msg
fontSizesDropdown config =
    div [ class "dropdown-things" ]
        [ Html.button
            ([ class "annotation-button"
             , title (config.toTitle ())
             ]
                ++ dropdownEvents placeholderAnnotation (dropdownFromType Fonts)
            )
            [ Icons.viewFontSize
            ]
        , config.render Fonts
        ]


button : { toTitle : Annotation.Choice -> String, selected : Annotation.Choice } -> Annotation.Choice -> Html Msg
button config annotation =
    Html.button
        [ classList
            [ ( "annotation-button", True )
            , ( "annotation-button--selected", config.selected == annotation )
            , ( "annotation-button--spotlight", Annotation.isSpotlight annotation )
            ]
        , title (config.toTitle annotation)
        , onClick (ChangeAnnotation annotation)
        ]
        [ icon annotation ]


viewFontSizeOptions : FontSize -> Html Msg
viewFontSizeOptions fontSize =
    Annotation.fontSizes
        |> List.map (viewFontSizeOption fontSize)
        |> div [ class "dropdown-options" ]


viewFillOptions : Fill -> Html Msg
viewFillOptions fill =
    Annotation.fills
        |> List.map (viewFillOption fill)
        |> div [ class "dropdown-options" ]


viewStrokeColorOptions : StrokeColor -> Html Msg
viewStrokeColorOptions strokeColor =
    Annotation.strokeColors
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
    Annotation.strokeStyles
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


viewDropdown : AnnotationStyles -> Annotation.Choice -> DropdownType -> Html Msg
viewDropdown { fill, strokeColor, strokeStyle, fontSize } annotation dropdown =
    case dropdown of
        ShapesDropdown ->
            viewShapesOptions annotation

        SpotlightsDropdown ->
            viewSpotlightsOptions annotation

        Fonts ->
            viewFontSizeOptions fontSize

        Fills ->
            viewFillOptions fill

        StrokeColors ->
            viewStrokeColorOptions strokeColor

        Strokes ->
            viewStrokeStyleOptions strokeStyle


viewShapesOptions : Annotation.Choice -> Html Msg
viewShapesOptions annotation =
    shapes
        |> List.map (viewAnnotationOption annotation)
        |> div [ class "dropdown-options" ]


viewAnnotationOption : Annotation.Choice -> Annotation.Choice -> Html Msg
viewAnnotationOption selectedAnnotation annotation =
    Html.button
        [ classList
            [ ( "dropdown-button", True )
            , ( "dropdown-button--selected", selectedAnnotation == annotation )
            , ( "dropdown-button--spotlight", List.member annotation spotlights )
            ]
        , onClick (ChangeAnnotation annotation)
        ]
        [ icon annotation ]


viewSpotlightsOptions : Annotation.Choice -> Html Msg
viewSpotlightsOptions annotation =
    spotlights
        |> List.map (viewAnnotationOption annotation)
        |> div [ class "dropdown-options" ]


icon : Annotation.Choice -> Svg msg
icon choice =
    case choice of
        Arrow ->
            Icons.viewArrow

        StraightLine ->
            Icons.viewLine

        FreeHand ->
            Icons.freeHand

        Rectangle ->
            Icons.viewRectangle

        RoundedRectangle ->
            Icons.viewRoundedRectangle

        Ellipse ->
            Icons.viewEllipse

        TextBox ->
            Icons.viewText

        SpotlightRectangle ->
            Icons.viewSpotlightRect

        SpotlightRoundedRectangle ->
            Icons.viewSpotlightRoundedRect

        SpotlightEllipse ->
            Icons.viewSpotlightEllipse

        Pixelate ->
            Icons.viewPixelate


choiceToTitle : OperatingSystem -> Annotation.Choice -> String
choiceToTitle os choice =
    case os of
        MacOS ->
            macAnnotationToTitle choice

        Windows ->
            windowsAnnotationToTitle choice


windowsAnnotationToTitle : Annotation.Choice -> String
windowsAnnotationToTitle drawing =
    case drawing of
        Arrow ->
            "A̲rrow"

        StraightLine ->
            "L̲ine"

        FreeHand ->
            "Free H̲and"

        Rectangle ->
            "R̲ectangle"

        RoundedRectangle ->
            "Ro̲unded Rectangle"

        Ellipse ->
            "E̲llipse"

        TextBox ->
            "T̲ext"

        SpotlightRectangle ->
            "Spotlig̲ht Rectangle"

        SpotlightRoundedRectangle ->
            "Spotlight Rounded Rec̲tangle"

        SpotlightEllipse ->
            "Spotlight Elli̲pse"

        Pixelate ->
            "P̲ixelate"


macAnnotationToTitle : Annotation.Choice -> String
macAnnotationToTitle choice =
    case choice of
        Arrow ->
            "Arrow (A)"

        StraightLine ->
            "Line (L)"

        FreeHand ->
            "Free Hand (H)"

        Rectangle ->
            "Rectangle (R)"

        RoundedRectangle ->
            "Rounded Rectangle (O)"

        Ellipse ->
            "Ellipse (E)"

        TextBox ->
            "Text (T)"

        SpotlightRectangle ->
            "Spotlight Rectangle (G)"

        SpotlightRoundedRectangle ->
            "Spotlight Rounded Rectangle (C)"

        SpotlightEllipse ->
            "Spotlight Ellipse (I)"

        Pixelate ->
            "Pixelate (P)"


view : Config -> State -> Html Msg
view config ({ annotation, shape, spotlight } as state) =
    let
        { strokeColor, fill, strokeStyle } =
            config.styles

        toTitle =
            choiceToTitle config.os

        toDropdown =
            viewDropdownMenu config state

        buttonConfig =
            { toTitle = toTitle, selected = annotation }

        annotationDropdownConfig =
            { toTitle = toTitle
            , selected = annotation
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
        [ button buttonConfig Arrow
        , button buttonConfig FreeHand
        , button buttonConfig TextBox
        , shapesDropdown annotationDropdownConfig shape
        , spotlightsDropdown annotationDropdownConfig spotlight
        , button buttonConfig Pixelate
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
                        Time.every 200 (always (ToggleDropdown dropdown))

                    else
                        Sub.none

                ImmediateDropdown ->
                    Sub.none
