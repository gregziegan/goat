module Goat.View.Controls exposing (viewControls, viewDropdownMenu)

import Array.Hamt exposing (Array)
import Color exposing (Color)
import Goat.ControlOptions as ControlOptions exposing (fontSizes)
import Goat.Helpers exposing (..)
import Goat.Icons as Icons
import Goat.Model exposing (..)
import Goat.Update exposing (Msg(..), autoExpandConfig)
import Html exposing (Attribute, Html, button, div, h2, h3, img, li, p, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, src, style)
import Html.Events exposing (onClick, onWithOptions)
import Rocket exposing ((=>))
import UndoList exposing (UndoList)


viewControls : Model -> AnnotationAttributes -> (AttributeDropdown -> Html Msg) -> Html Msg
viewControls ({ edits, keyboardState, drawing, annotationState } as model) { strokeColor, fill, strokeStyle, fontSize } toDropdownMenu =
    div
        [ class "controls" ]
        [ div [ class "columns" ]
            [ button [ onClick ReturnToImageSelection, class "cancel-button" ] [ text "Cancel" ]
            , button [ onClick Save, class "save-button" ] [ text "Save" ]
            ]
        , viewHistoryControls edits
        , div [ class "columns" ]
            (List.map (viewDrawingButton drawing) ControlOptions.drawings
                ++ [ viewStrokeColorDropdown toDropdownMenu strokeColor
                   , viewFillDropdown toDropdownMenu fill
                   , viewLineStrokeDropdown toDropdownMenu strokeStyle
                   , viewFontSizeDropdown toDropdownMenu
                   ]
            )
        ]


viewHistoryControls : UndoList (Array Annotation) -> Html Msg
viewHistoryControls edits =
    div [ class "history-controls" ]
        [ button [ onClick Undo, class "history-button", disabled <| not <| UndoList.hasPast edits ] [ Icons.viewUndoArrow ]
        , button [ onClick Redo, class "history-button flip", disabled <| not <| UndoList.hasFuture edits ] [ Icons.viewUndoArrow ]
        ]


viewFontSizeDropdown : (AttributeDropdown -> Html Msg) -> Html Msg
viewFontSizeDropdown toDropdownMenu =
    div [ class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown Fonts
            , class "drawing-button"
            ]
            [ Icons.viewFontSize
            , Icons.viewCornerArrow
            ]
        , toDropdownMenu Fonts
        ]


viewFontSizeOptions : Int -> Html Msg
viewFontSizeOptions fontSize =
    ControlOptions.fontSizes
        |> List.map (viewFontSizeOption fontSize)
        |> div [ class "dropdown-options" ]


viewFillOptions : Maybe Color -> Html Msg
viewFillOptions fill =
    ControlOptions.fills
        |> List.map (viewFillOption fill)
        |> div [ class "dropdown-options" ]


viewStrokeColorOptions : Color -> Html Msg
viewStrokeColorOptions strokeColor =
    ControlOptions.strokeColors
        |> List.map (viewStrokeColorOption strokeColor)
        |> div [ class "dropdown-options" ]


viewFillOption : Maybe Color -> Maybe Color -> Html Msg
viewFillOption selectedFill fill =
    button
        [ classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedFill == fill
            ]
        , onClick (SelectFill fill)
        ]
        [ Icons.viewFill fill ]


viewStrokeColorOption : Color -> Color -> Html Msg
viewStrokeColorOption selectedColor color =
    button
        [ classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedColor == color
            ]
        , onClick (SelectStrokeColor color)
        ]
        [ Icons.viewStrokeColor color ]


viewFontSizeOption : Int -> Int -> Html Msg
viewFontSizeOption selectedFontSize fontSize =
    button
        [ classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedFontSize == fontSize
            ]
        , onClick (SelectFontSize fontSize)
        ]
        [ text <| toString <| fontSize ]


viewLineStrokeDropdown : (AttributeDropdown -> Html Msg) -> StrokeStyle -> Html Msg
viewLineStrokeDropdown toDropdownMenu strokeStyle =
    div
        [ class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown Strokes
            , class "drawing-button"
            ]
            [ Icons.viewStrokeStyle strokeStyle
            , Icons.viewCornerArrow
            ]
        , toDropdownMenu Strokes
        ]


viewFillDropdown : (AttributeDropdown -> Html Msg) -> Maybe Color -> Html Msg
viewFillDropdown toDropdownMenu fill =
    div
        [ class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown Fills
            , class "drawing-button"
            ]
            [ Icons.viewFill fill
            , Icons.viewCornerArrow
            ]
        , toDropdownMenu Fills
        ]


viewStrokeColorDropdown : (AttributeDropdown -> Html Msg) -> Color -> Html Msg
viewStrokeColorDropdown toDropdownMenu strokeColor =
    div
        [ class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown StrokeColors
            , class "drawing-button"
            ]
            [ Icons.viewStrokeColor strokeColor
            , Icons.viewCornerArrow
            ]
        , toDropdownMenu StrokeColors
        ]


viewLineStrokeOptions : StrokeStyle -> Html Msg
viewLineStrokeOptions strokeStyle =
    ControlOptions.strokeStyles
        |> List.map (viewStrokeStyleOption strokeStyle)
        |> div [ class "dropdown-options" ]


viewStrokeStyleOption : StrokeStyle -> StrokeStyle -> Html Msg
viewStrokeStyleOption selectedStrokeStyle strokeStyle =
    button
        [ classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedStrokeStyle == strokeStyle
            ]
        , onClick (SelectStrokeStyle strokeStyle)
        ]
        [ Icons.viewStrokeStyle strokeStyle ]


viewDropdownMenu : Maybe AttributeDropdown -> AnnotationAttributes -> Model -> AttributeDropdown -> Html Msg
viewDropdownMenu maybeDropdown annotationAttrs model selectedOption =
    Maybe.map (viewDropdownOptions model annotationAttrs selectedOption) maybeDropdown
        |> Maybe.withDefault (text "")


viewDropdownOptions : Model -> AnnotationAttributes -> AttributeDropdown -> AttributeDropdown -> Html Msg
viewDropdownOptions model { fill, strokeColor, strokeStyle, fontSize } selectedOption editOption =
    if selectedOption /= editOption then
        text ""
    else
        case editOption of
            Fonts ->
                viewFontSizeOptions fontSize

            Fills ->
                viewFillOptions fill

            StrokeColors ->
                viewStrokeColorOptions strokeColor

            Strokes ->
                viewLineStrokeOptions strokeStyle


viewDrawingButton : Drawing -> Drawing -> Html Msg
viewDrawingButton selectedDrawing drawing =
    button
        [ classList
            [ "drawing-button" => True
            , "drawing-button--selected" => drawingsAreEqual selectedDrawing drawing
            , "drawing-button--spotlight" => isSpotlightDrawing drawing
            ]
        , onClick <| ChangeDrawing drawing
        ]
        [ viewShapeSvg drawing ]


viewShapeSvg : Drawing -> Html Msg
viewShapeSvg drawing =
    case drawing of
        DrawLine lineType ->
            case lineType of
                StraightLine ->
                    Icons.viewLine

                Arrow ->
                    Icons.viewArrow

        DrawShape shapeType ->
            case shapeType of
                Rect ->
                    Icons.viewRectangle

                RoundedRect ->
                    Icons.viewRoundedRectangle

                Ellipse ->
                    Icons.viewEllipse

        DrawTextBox ->
            Icons.viewText

        DrawSpotlight shapeType ->
            case shapeType of
                Rect ->
                    Icons.viewSpotlightRect

                RoundedRect ->
                    Icons.viewSpotlightRoundedRect

                Ellipse ->
                    Icons.viewSpotlightEllipse

        DrawBlur ->
            Icons.viewBlur
