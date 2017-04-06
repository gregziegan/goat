module Goat.View exposing (..)

import Array.Hamt as Array exposing (Array)
import AutoExpand
import Color.Convert
import Color exposing (Color)
import Goat.Model exposing (..)
import Goat.Helpers exposing (..)
import Goat.Update exposing (..)
import Html exposing (Attribute, Html, button, div, p, text)
import Html.Attributes as Html exposing (class, classList, disabled, height, id, readonly, src, start, style, type_, width)
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput, onMouseEnter, onMouseLeave, onWithOptions)
import Json.Decode as Json
import Keyboard.Extra as Keyboard exposing (Key(Shift), KeyChange, KeyChange(..), isPressed)
import List.Extra
import List.Zipper exposing (Zipper)
import Mouse exposing (Position)
import Rocket exposing ((=>))
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import Svg.Events as SE
import UndoList exposing (UndoList)


view : Model -> Html Msg
view model =
    case model.images of
        Nothing ->
            viewInfoScreen

        Just images ->
            if model.imageSelected then
                viewImageAnnotator model <| List.Zipper.current images
            else
                viewImageSelector images


viewImageSelector : Zipper Image -> Html Msg
viewImageSelector images =
    images
        |> List.Zipper.toList
        |> List.map (viewImageOption images)
        |> div [ Html.class "image-selector" ]


viewImageOption : Zipper Image -> Image -> Html Msg
viewImageOption zipper image =
    button
        [ Html.class "image-option"
        , Html.width <| round image.width
        , Html.height <| round image.height
        , onClick <| SelectImage image
        ]
        [ Html.img [ src image.url, Html.height <| round image.height, Html.width <| round image.width ] []
        , Html.div [ onClick <| SelectImage image, Html.class "image-edit-pencil" ]
            [ viewPencilIcon
            ]
        ]


viewPencilIcon : Html msg
viewPencilIcon =
    svg [ Attr.width "20", Attr.height "20", viewBox "0 0 500 500" ]
        [ Svg.path [ fill "#555", d "M492.8,58L442,7.2c-9.6-9.6-25.3-9.6-34.8,0l-17.6,17.6l-1.5,1.5L377.4,37l85.5,85.5l10.8-10.8l1.5-1.5l17.6-17.6C502.4,83.2,502.4,67.6,492.8,58z" ] []
        , Svg.path [ fill "#555", d "M51.7,362.4l85.5,85.5l308.5-308.5l-85.5-85.5L51.7,362.4z M395.2,148.7L146.4,397.3l-9.3-9.3l248.8-248.8L395.2,148.7z M111.7,362.6l-9.3-9.3l248.7-248.8l9.3,9.3L111.7,362.6z" ] []
        , Svg.polygon [ fill "#555", points "36.4,377.9 14.1,452.9 47.1,485.9 122.1,463.6 79.3,420.7" ] []
        , Svg.polygon [ fill "#555", points "0,500 36,489.2 10.8,464" ] []
        ]


viewInfoScreen : Html Msg
viewInfoScreen =
    div []
        [ Html.text "please upload an image!" ]


viewImageAnnotator : Model -> Image -> Html Msg
viewImageAnnotator ({ edits, fill, strokeColor, mouse, keyboardState, currentDropdown, drawing } as model) selectedImage =
    let
        toDropdownMenu =
            viewDropdownMenu currentDropdown drawing model

        shiftPressed =
            isPressed Shift keyboardState
    in
        div
            [ Html.class "annotation-app" ]
            [ div [ Html.class "controls" ]
                [ div [ Html.class "columns" ]
                    [ button [ onClick Cancel, Html.class "cancel-button" ] [ Html.text "Cancel" ]
                    , button [ onClick Save, Html.class "save-button" ] [ Html.text "Save" ]
                    ]
                , viewHistoryControls edits
                , div [ Html.class "columns" ]
                    (List.map (viewDrawingButton keyboardState drawing toDropdownMenu) (drawingOptions shiftPressed)
                        ++ [ viewStrokeColorDropdown toDropdownMenu strokeColor
                           , viewFillDropdown toDropdownMenu fill
                           , viewLineStrokeDropdown toDropdownMenu
                           ]
                    )
                ]
            , viewCanvas model selectedImage
            ]


drawingsAreEqual : Drawing -> Drawing -> Bool
drawingsAreEqual drawing drawing2 =
    case drawing of
        DrawLine lineType _ ->
            case drawing2 of
                DrawLine lineType2 _ ->
                    lineType == lineType2

                _ ->
                    False

        DrawShape shapeType _ ->
            case drawing2 of
                DrawShape shapeType2 _ ->
                    shapeType == shapeType2

                _ ->
                    False

        DrawTextBox ->
            case drawing2 of
                DrawTextBox ->
                    True

                _ ->
                    False

        DrawSpotlight shapeType shapeMode ->
            case drawing2 of
                DrawSpotlight shapeType2 _ ->
                    shapeType == shapeType2

                _ ->
                    False


viewVanillaDrawingButton : Keyboard.State -> Drawing -> Drawing -> Html Msg
viewVanillaDrawingButton keyboardState selectedDrawing drawing =
    button
        [ Html.classList
            [ "drawing-button" => True
            , "drawing-button--selected" => drawingsAreEqual selectedDrawing drawing
            , "drawing-button--alternate" => drawingsAreEqual selectedDrawing drawing && isPressed Shift keyboardState
            ]
        , onClick <| ChangeDrawing drawing
        ]
        [ viewShapeSvg drawing ]


viewDrawingButton : Keyboard.State -> Drawing -> (AttributeDropdown -> Html Msg) -> Drawing -> Html Msg
viewDrawingButton keyboardState selectedDrawing toDropdownMenu drawing =
    case drawing of
        DrawLine _ _ ->
            viewVanillaDrawingButton keyboardState selectedDrawing drawing

        DrawShape _ _ ->
            viewVanillaDrawingButton keyboardState selectedDrawing drawing

        DrawTextBox ->
            viewTextSizeDropdown selectedDrawing toDropdownMenu

        DrawSpotlight _ _ ->
            viewVanillaDrawingButton keyboardState selectedDrawing drawing


viewHistoryControls : UndoList (Array Annotation) -> Html Msg
viewHistoryControls edits =
    div [ Html.class "history-controls" ]
        [ button [ onClick Undo, Html.class "history-button", disabled <| not <| UndoList.hasPast edits ] [ viewUndoArrow ]
        , button [ onClick Redo, Html.class "history-button flip", disabled <| not <| UndoList.hasFuture edits ] [ viewUndoArrow ]
        ]


viewTextSizeDropdown : Drawing -> (AttributeDropdown -> Html Msg) -> Html Msg
viewTextSizeDropdown drawing toDropdownMenu =
    div [ Html.class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown Fonts
            , Html.classList [ "drawing-button" => True, "drawing-button--selected" => drawingsAreEqual drawing DrawTextBox ]
            ]
            [ viewTextIcon
            , viewCornerArrow
            ]
        , toDropdownMenu Fonts
        ]


viewFontSizeOptions : Float -> Html Msg
viewFontSizeOptions fontSize =
    fontSizes
        |> List.map (viewFontSizeOption fontSize)
        |> div [ Html.class "dropdown-options" ]


viewFillOptions : Fill -> Html Msg
viewFillOptions fill =
    fillOptions
        |> List.map (viewFillOption fill)
        |> div [ Html.class "dropdown-options" ]


viewStrokeColorOptions : Color -> Html Msg
viewStrokeColorOptions strokeColor =
    strokeColorOptions
        |> List.map (viewStrokeColorOption strokeColor)
        |> div [ Html.class "dropdown-options" ]


viewFillOption : Fill -> Fill -> Html Msg
viewFillOption selectedFill fill =
    button
        [ Html.classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedFill == fill
            ]
        , onClick (SelectFill fill)
        ]
        [ viewFillIcon fill ]


viewStrokeColorOption : Color -> Color -> Html Msg
viewStrokeColorOption selectedColor color =
    button
        [ Html.classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedColor == color
            ]
        , onClick (SelectStrokeColor color)
        ]
        [ viewStrokeColorIcon color ]


viewFontSizeOption : Float -> Float -> Html Msg
viewFontSizeOption selectedFontSize fontSize =
    button
        [ Html.classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedFontSize == fontSize
            ]
        , onClick (SelectFontSize fontSize)
        ]
        [ Html.text <| toString <| fontSize ]


viewLineStrokeDropdown : (AttributeDropdown -> Html Msg) -> Html Msg
viewLineStrokeDropdown toDropdownMenu =
    div
        [ Html.class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown Strokes
            , Html.class "drawing-button"
            ]
            [ viewLineStrokeDropdownIcon
            , viewCornerArrow
            ]
        , toDropdownMenu Strokes
        ]


viewFillDropdown : (AttributeDropdown -> Html Msg) -> Fill -> Html Msg
viewFillDropdown toDropdownMenu fill =
    div
        [ Html.class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown Fills
            , Html.class "drawing-button"
            ]
            [ viewFillIcon fill
            , viewCornerArrow
            ]
        , toDropdownMenu Fills
        ]


viewStrokeColorDropdown : (AttributeDropdown -> Html Msg) -> Color -> Html Msg
viewStrokeColorDropdown toDropdownMenu strokeColor =
    div
        [ Html.class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown StrokeColors
            , Html.class "drawing-button"
            ]
            [ viewStrokeColorIcon strokeColor
            , viewCornerArrow
            ]
        , toDropdownMenu StrokeColors
        ]


viewCornerArrow : Html msg
viewCornerArrow =
    svg [ Attr.width "5", Attr.height "5", viewBox "0 0 5 5", Attr.class "corner-arrow" ]
        [ Svg.path [ d "M5 0L0 5h5", fill "#555", fillRule "evenodd" ] [] ]


viewDropdownMenu : Maybe AttributeDropdown -> Drawing -> Model -> AttributeDropdown -> Html Msg
viewDropdownMenu maybeDropdown drawing model selectedOption =
    Maybe.map (viewDropdownOptions drawing model selectedOption) maybeDropdown
        |> Maybe.withDefault (Html.text "")


viewDropdownOptions : Drawing -> Model -> AttributeDropdown -> AttributeDropdown -> Html Msg
viewDropdownOptions curEditMode model selectedOption editOption =
    if selectedOption /= editOption then
        Html.text ""
    else
        case editOption of
            Fonts ->
                viewFontSizeOptions model.fontSize

            Fills ->
                viewFillOptions model.fill

            StrokeColors ->
                viewStrokeColorOptions model.strokeColor

            Strokes ->
                viewLineStrokeOptions model.strokeStyle


viewShapeSvg : Drawing -> Html Msg
viewShapeSvg drawing =
    case drawing of
        DrawLine lineType _ ->
            case lineType of
                StraightLine ->
                    viewLineIcon

                Arrow ->
                    viewArrowIcon

        DrawShape shapeType _ ->
            case shapeType of
                Rect ->
                    viewRectangleIcon

                RoundedRect ->
                    viewRoundedRectangleIcon

                Ellipse ->
                    viewEllipseIcon

        DrawTextBox ->
            viewTextIcon

        DrawSpotlight shapeType _ ->
            case shapeType of
                Rect ->
                    viewSpotlightRectIcon

                RoundedRect ->
                    viewSpotlightRoundedRectIcon

                Ellipse ->
                    viewSpotlightEllipseIcon


viewLineStrokeOptions : StrokeStyle -> Html Msg
viewLineStrokeOptions strokeStyle =
    strokeStyleOptions
        |> List.map (viewStrokeStyleOption strokeStyle)
        |> div [ Html.class "dropdown-options" ]


viewStrokeStyleOption : StrokeStyle -> StrokeStyle -> Html Msg
viewStrokeStyleOption selectedStrokeStyle strokeStyle =
    button
        [ Html.classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedStrokeStyle == strokeStyle
            ]
        , onClick (SelectStrokeStyle strokeStyle)
        ]
        [ case strokeStyle of
            SolidThin ->
                svg [ Attr.width "14", Attr.height "2", viewBox "0 0 14 2" ]
                    [ Svg.path [ d "M1 .5h12", stroke "#555", fill "none", fillRule "evenodd", strokeLinecap "square" ] [] ]

            SolidMedium ->
                viewNormalLineIcon

            SolidThick ->
                svg [ Attr.width "14", Attr.height "4", viewBox "0 0 14 4" ]
                    [ Svg.path [ d "M0 4h16V0H0z", fillRule "nonzero", fill "#555" ] [] ]

            SolidVeryThick ->
                svg [ Attr.width "14", Attr.height "6", viewBox "0 0 14 6" ]
                    [ Svg.path [ d "M0 6h16V0H0z", fillRule "nonzero", fill "#555" ] [] ]

            DashedThin ->
                svg [ Attr.width "14", Attr.height "1", viewBox "0 0 14 1" ]
                    [ Svg.path [ d "M0 2h4V0H0v2zm5 0h4V0H5v2zm5 0h4V0h-4v2z", fillRule "nonzero", fill "#555" ] [] ]

            DashedMedium ->
                svg [ Attr.width "14", Attr.height "2", viewBox "0 0 14 2" ]
                    [ Svg.path [ d "M0 2h4V0H0v2zm5 0h4V0H5v2zm5 0h4V0h-4v2z", fillRule "nonzero", fill "#555" ] [] ]

            DashedThick ->
                svg [ Attr.width "14", Attr.height "2", viewBox "0 0 14 2" ]
                    [ Svg.path [ d "M0 2h4V0H0v2zm5 0h4V0H5v2zm5 0h4V0h-4v2z", fillRule "nonzero", fill "#555" ] [] ]

            DashedVeryThick ->
                svg [ Attr.width "14", Attr.height "2", viewBox "0 0 14 2" ]
                    [ Svg.path [ d "M0 4h6V0H0v4zm9 0h6V0H9v4z", fillRule "nonzero", fill "#555" ] [] ]
        ]


drawingStateEvents : Drawing -> AnnotationState -> List (Html.Attribute Msg)
drawingStateEvents drawing annotationState =
    case annotationState of
        ReadyToDraw ->
            [ onMouseDown <| Json.map (StartDrawing << toDrawingPosition) Mouse.position
            ]

        DrawingAnnotation startPos ->
            onMouseUpOrLeave <| Json.map (FinishDrawing startPos << toDrawingPosition) Mouse.position

        MovingAnnotation index annotation startPos ->
            [ Html.Events.onMouseLeave ResetToReadyToDraw
            , SE.on "mouseup" <| Json.map (FinishMovingAnnotation index annotation startPos << toDrawingPosition) Mouse.position
            ]

        ResizingAnnotation index annotation startPos vertex ->
            [ Html.Events.onMouseLeave ResetToReadyToDraw
            , SE.on "mouseup" <| Json.map (FinishResizingAnnotation index annotation vertex startPos << toDrawingPosition) Mouse.position
            ]

        SelectedAnnotation index annotation ->
            [ onMouseDown <| Json.map (StartDrawing << toDrawingPosition) Mouse.position ]

        EditingATextBox index ->
            [ SE.onMouseDown <| FinishEditingText index
            ]


viewMask : Float -> Float -> Svg msg
viewMask width height =
    rect
        [ x "0"
        , y "0"
        , Attr.height <| toString height
        , Attr.width <| toString width
        , Attr.mask "url(#Mask)"
        , Attr.style "pointer-events: none;"
        ]
        []


viewSpotlights : AnnotationState -> Array Annotation -> List (Svg Msg)
viewSpotlights annotationState annotations =
    annotations
        |> Array.filter isSpotlightShape
        |> Array.map spotlightFillToMaskFill
        |> Array.toList
        |> List.indexedMap (viewAnnotation annotationState)
        |> List.concat


canvasAttributes : Image -> Drawing -> AnnotationState -> List (Svg.Attribute Msg)
canvasAttributes image drawing annotationState =
    [ Html.id "canvas"
    , Html.class "image-edit"
    , Html.style
        [ "width" => toString (round image.width) ++ "px"
        , "height" => toString (round image.height) ++ "px"
        , "cursor" => annotationStateToCursor annotationState
        ]
    ]
        ++ drawingStateEvents drawing annotationState


viewNonSpotlightAnnotations : AnnotationState -> Array Annotation -> List (Svg Msg)
viewNonSpotlightAnnotations annotationState annotations =
    annotations
        |> Array.toList
        |> List.indexedMap (viewAnnotation annotationState)
        |> List.concat


viewDefinitions : Float -> Float -> AnnotationState -> List (Svg Msg) -> List (Svg Msg)
viewDefinitions width height annotationState cutOuts =
    List.map viewArrowHeadDefinition strokeColorOptions
        |> (::) (maskDefinition annotationState width height cutOuts)
        |> (::) viewSvgFilters
        |> defs []
        |> List.singleton


getFirstSpotlightIndex : Array Annotation -> Int
getFirstSpotlightIndex annotations =
    annotations
        |> Array.toList
        |> List.Extra.findIndex isSpotlightShape
        |> Maybe.withDefault 0


getAnnotations : Image -> Array Annotation -> List (Svg Msg) -> List (Svg Msg) -> Bool -> List (Svg Msg)
getAnnotations image annotations spotlights nonSpotlights isDrawingSpotlight =
    let
        firstSpotlightIndex =
            getFirstSpotlightIndex annotations
    in
        if isDrawingSpotlight && List.isEmpty spotlights then
            nonSpotlights ++ [ viewMask image.width image.height ]
        else if List.isEmpty spotlights then
            nonSpotlights
        else
            List.take (firstSpotlightIndex) nonSpotlights
                ++ [ viewMask image.width image.height ]
                ++ List.drop firstSpotlightIndex nonSpotlights


viewDrawingAndAnnotations :
    (List (Svg Msg) -> List (Svg Msg))
    -> List (Svg Msg)
    -> (Bool -> List (Svg Msg))
    -> (StartPosition -> Bool -> Svg Msg)
    -> Drawing
    -> AnnotationState
    -> List (Svg Msg)
viewDrawingAndAnnotations definitions spotlights toAnnotations toDrawing drawing annotationState =
    let
        justAnnotations =
            definitions spotlights ++ toAnnotations False

        nonSpotlightDrawingAndAnnotations start =
            definitions spotlights ++ toAnnotations False ++ [ toDrawing start False ]

        spotlightDrawingAndAnnotations start =
            definitions (spotlights ++ [ toDrawing start True ]) ++ toAnnotations True ++ [ toDrawing start False ]
    in
        case annotationState of
            DrawingAnnotation start ->
                case drawing of
                    DrawShape shapeType _ ->
                        nonSpotlightDrawingAndAnnotations start

                    DrawSpotlight _ _ ->
                        spotlightDrawingAndAnnotations start

                    _ ->
                        nonSpotlightDrawingAndAnnotations start

            _ ->
                justAnnotations


viewCanvas : Model -> Image -> Html Msg
viewCanvas model image =
    let
        annotations =
            model.edits.present

        toDrawing start isInMask =
            viewDrawing model start isInMask

        spotlights =
            viewSpotlights model.annotationState annotations

        nonSpotlights =
            viewNonSpotlightAnnotations model.annotationState annotations

        definitions =
            viewDefinitions image.width image.height model.annotationState

        toAnnotations =
            getAnnotations image annotations spotlights nonSpotlights
    in
        div (canvasAttributes image model.drawing model.annotationState)
            (viewImage image
                :: [ svg
                        [ Attr.id "drawing"
                        , Attr.class "drawing"
                        , Attr.width <| toString <| round image.width
                        , Attr.height <| toString <| round image.height
                        , Html.attribute "xmlns" "http://www.w3.org/2000/svg"
                        ]
                        (viewDrawingAndAnnotations definitions spotlights toAnnotations toDrawing model.drawing model.annotationState)
                   ]
            )


viewSvgFilters : Svg Msg
viewSvgFilters =
    Svg.filter [ Attr.id "dropShadow", Attr.x "-20%", Attr.y "-20%", Attr.width "200%", Attr.height "200%" ]
        [ Svg.feGaussianBlur [ Attr.in_ "SourceAlpha", Attr.stdDeviation "2.2" ] []
        , Svg.feOffset [ Attr.dx "2", Attr.dy "2", Attr.result "offsetblur" ] []
        , Svg.feComponentTransfer []
            [ Svg.feFuncA [ Attr.type_ "linear", Attr.slope "0.2" ] []
            ]
        , Svg.feMerge []
            [ Svg.feMergeNode [] []
            , Svg.feMergeNode [ Attr.in_ "SourceGraphic" ] []
            ]
        ]


viewArrowHeadDefinition : Color -> Svg Msg
viewArrowHeadDefinition color =
    marker
        [ Attr.id <| "arrow-head--" ++ Color.Convert.colorToHex color
        , orient "auto"
        , markerWidth "2"
        , markerHeight "4"
        , refX "0.1"
        , refY "2"
        , Attr.class "pointerCursor"
        ]
        [ Svg.path [ d "M0,0 V4 L2,2 Z", Attr.fill <| Color.Convert.colorToHex color ] []
        ]


isSpotlightShape : Annotation -> Bool
isSpotlightShape annotation =
    case annotation of
        Spotlight _ _ ->
            True

        _ ->
            False


spotlightFillToMaskFill : Annotation -> Annotation
spotlightFillToMaskFill annotation =
    case annotation of
        Spotlight shapeType shape ->
            Spotlight shapeType { shape | fill = MaskFill }

        _ ->
            annotation


annotationStateEvents : Int -> Annotation -> AnnotationState -> List (Svg.Attribute Msg)
annotationStateEvents index annotation annotationState =
    case annotationState of
        ReadyToDraw ->
            [ SE.on "mousedown" <| Json.map (SelectAnnotation index annotation << toDrawingPosition) Mouse.position
            , Attr.class "pointerCursor"
            , Html.attribute "onmousedown" "event.stopPropagation();"
            ]

        DrawingAnnotation start ->
            [ Attr.class "crosshairCursor" ]

        SelectedAnnotation start _ ->
            [ Attr.class "moveCursor"
            , SE.on "mousedown" <| Json.map (StartMovingAnnotation index annotation << toDrawingPosition) Mouse.position
            , Html.attribute "onmousedown" "event.stopPropagation();"
            ]

        MovingAnnotation index _ startPos ->
            [ SE.on "mouseup" <| Json.map (FinishMovingAnnotation index annotation startPos << toDrawingPosition) Mouse.position
            , Attr.class "moveCursor"
            ]

        ResizingAnnotation _ _ _ _ ->
            [ Attr.class "resizeCursor" ]

        EditingATextBox index ->
            [ Attr.class "crosshairCursor" ]


getSelectState : Int -> AnnotationState -> SelectState
getSelectState index annotationState =
    case annotationState of
        SelectedAnnotation int annotation ->
            if index == int then
                SelectedWithVertices
            else
                NotSelected

        MovingAnnotation int annotation startPosition ->
            if index == int then
                SelectedWithVertices
            else
                NotSelected

        ResizingAnnotation int annotation startPosition vertex ->
            if index == int then
                SelectedWithVertices
            else
                NotSelected

        EditingATextBox int ->
            if index == int then
                Selected
            else
                NotSelected

        _ ->
            NotSelected


viewAnnotation : AnnotationState -> Int -> Annotation -> List (Svg Msg)
viewAnnotation annotationState index annotation =
    let
        selectState =
            getSelectState index annotationState

        movementEvents =
            annotationStateEvents index annotation annotationState

        toVertexEvents =
            annotationStateVertexEvents index annotation annotationState

        vertices verticesType { start, end } =
            viewVertices verticesType start end toVertexEvents selectState
    in
        case annotation of
            Lines lineType line ->
                viewLine movementEvents (vertices Linear line) lineType line

            Shapes shapeType shape ->
                case shapeType of
                    Ellipse ->
                        viewShape movementEvents (vertices Elliptical shape) shapeType shape

                    _ ->
                        viewShape movementEvents (vertices Rectangular shape) shapeType shape

            TextBox textBox ->
                viewTextBox movementEvents (vertices Rectangular textBox) annotationState selectState index textBox

            Spotlight shapeType shape ->
                case shapeType of
                    Ellipse ->
                        viewShape movementEvents (vertices Elliptical shape) shapeType shape

                    _ ->
                        viewShape movementEvents (vertices Rectangular shape) shapeType shape


annotationStateVertexEvents : Int -> Annotation -> AnnotationState -> Vertex -> List (Svg.Attribute Msg)
annotationStateVertexEvents index annotation annotationState vertex =
    [ SE.on "mousedown" <| Json.map (StartResizingAnnotation index annotation vertex << toDrawingPosition) Mouse.position
    , Attr.class "resizeCursor"
    , Html.attribute "onmousedown" "event.stopPropagation();"
    ]
        ++ case annotationState of
            ResizingAnnotation int annotation start vertex ->
                [ SE.on "mouseup" <| Json.map (FinishResizingAnnotation index annotation vertex start << toDrawingPosition) Mouse.position ]

            _ ->
                []


maskDefinition : AnnotationState -> Float -> Float -> List (Svg Msg) -> Svg Msg
maskDefinition annotationState width height shapes =
    rect
        ([ x "0"
         , y "0"
         , Attr.width <| toString width
         , Attr.height <| toString height
         , opacity "0.5"
         , fill "white"
         ]
        )
        []
        :: shapes
        |> Svg.mask [ Attr.id "Mask" ]


viewDrawing : Model -> StartPosition -> Bool -> Svg Msg
viewDrawing { drawing, fill, strokeColor, strokeStyle, fontSize, mouse, keyboardState } pos isInMask =
    let
        lineAttrs lineType lineMode =
            lineAttributes lineType <| Line pos (calcLinePos pos mouse lineMode) strokeColor strokeStyle

        shapeAttrs shapeType shapeMode =
            shapeAttributes shapeType <| Shape pos (calcShapePos pos mouse shapeMode) fill strokeColor strokeStyle

        spotlightAttrs shapeType shapeMode spotlightFill spotlightColor =
            shapeAttributes shapeType <| Shape pos (calcShapePos pos mouse shapeMode) spotlightFill spotlightColor strokeStyle
    in
        case drawing of
            DrawLine lineType lineMode ->
                case lineType of
                    Arrow ->
                        Svg.path (lineAttrs lineType lineMode) []

                    StraightLine ->
                        Svg.path (lineAttrs lineType lineMode) []

            DrawShape shapeType shapeMode ->
                case shapeType of
                    Rect ->
                        Svg.rect (shapeAttrs shapeType shapeMode) []

                    RoundedRect ->
                        Svg.rect (shapeAttrs shapeType shapeMode) []

                    Ellipse ->
                        Svg.ellipse (shapeAttrs shapeType shapeMode) []

            DrawTextBox ->
                Svg.rect ((shapeAttributes Rect <| Shape pos mouse EmptyFill (Color.rgb 230 230 230) SolidThin) ++ [ Attr.strokeWidth "1" ]) []

            DrawSpotlight shapeType shapeMode ->
                let
                    ( fillDependentOnMask, strokeDependentOnMask ) =
                        if isInMask then
                            MaskFill => Color.white
                        else
                            EmptyFill => strokeColor
                in
                    case shapeType of
                        Rect ->
                            Svg.rect (spotlightAttrs shapeType shapeMode fillDependentOnMask strokeDependentOnMask) []

                        RoundedRect ->
                            Svg.rect (spotlightAttrs shapeType shapeMode fillDependentOnMask strokeDependentOnMask) []

                        Ellipse ->
                            Svg.ellipse (spotlightAttrs shapeType shapeMode fillDependentOnMask strokeDependentOnMask) []


fillStyle : Fill -> ( String, Bool )
fillStyle fill =
    case fill of
        SolidFill color ->
            Color.Convert.colorToHex color => True

        SpotlightFill ->
            "white" => False

        MaskFill ->
            "black" => True

        EmptyFill ->
            "white" => False


pointerEvents : Fill -> String
pointerEvents fill =
    case fill of
        EmptyFill ->
            "pointer-events: visibleStroke;"

        MaskFill ->
            "pointer-events: none;"

        SolidFill _ ->
            "pointer-events: auto;"

        SpotlightFill ->
            "pointer-events: visibleStroke;"


viewShape : List (Svg.Attribute Msg) -> List (Svg Msg) -> ShapeType -> Shape -> List (Svg Msg)
viewShape attrs vertices shapeType shape =
    let
        allAttrs =
            shapeAttributes shapeType shape ++ attrs
    in
        flip List.append vertices <|
            case shapeType of
                Rect ->
                    [ Svg.rect allAttrs [] ]

                RoundedRect ->
                    [ Svg.rect allAttrs [] ]

                Ellipse ->
                    [ Svg.ellipse allAttrs [] ]


viewVertices : Vertices -> StartPosition -> EndPosition -> (Vertex -> List (Svg.Attribute Msg)) -> SelectState -> List (Svg Msg)
viewVertices vertices start end toVertexEvents selectState =
    let
        toVertices =
            case vertices of
                Rectangular ->
                    shapeVertices

                Elliptical ->
                    ellipseVertices

                Linear ->
                    lineVertices
    in
        if selectState == SelectedWithVertices then
            toVertices toVertexEvents start end
        else
            []


shapeAttrs : Shape -> List (Svg.Attribute Msg)
shapeAttrs ({ strokeStyle, strokeColor, fill } as shape) =
    (Attr.style <| pointerEvents fill) :: strokeAttrs strokeStyle strokeColor


strokeAttrs : StrokeStyle -> Color -> List (Svg.Attribute Msg)
strokeAttrs strokeStyle strokeColor =
    let
        ( strokeWidth, dashArray ) =
            toLineStyle strokeStyle
    in
        [ Attr.stroke <| Color.Convert.colorToHex strokeColor
        , Attr.strokeWidth strokeWidth
        , Attr.strokeDasharray dashArray
        ]


rectAttrs : Shape -> List (Svg.Attribute Msg)
rectAttrs { start, end } =
    [ Attr.width <| toString <| abs <| end.x - start.x
    , Attr.height <| toString <| abs <| end.y - start.y
    , x <| toString <| Basics.min start.x end.x
    , y <| toString <| Basics.min start.y end.y
    , Attr.filter "url(#dropShadow)"
    ]


ellipseAttributes : Shape -> List (Svg.Attribute Msg)
ellipseAttributes { start, end } =
    [ rx <| toString <| abs <| end.x - start.x
    , ry <| toString <| abs <| end.y - start.y
    , cx <| toString start.x
    , cy <| toString start.y
    , Attr.filter "url(#dropShadow)"
    ]


shapeAttributes : ShapeType -> Shape -> List (Svg.Attribute Msg)
shapeAttributes shapeType shape =
    let
        ( fillColor, isVisible ) =
            fillStyle shape.fill

        fillStyles =
            if isVisible then
                [ Attr.fill fillColor ]
            else
                [ Attr.fill fillColor, fillOpacity "0" ]
    in
        List.append fillStyles <|
            case shapeType of
                Rect ->
                    shapeAttrs shape ++ rectAttrs shape

                RoundedRect ->
                    shapeAttrs shape ++ rectAttrs shape ++ [ rx "15", ry "15" ]

                Ellipse ->
                    shapeAttrs shape ++ ellipseAttributes shape


shapeVertices : (Vertex -> List (Svg.Attribute Msg)) -> StartPosition -> EndPosition -> List (Svg Msg)
shapeVertices toVertexEvents start end =
    [ viewVertex (toVertexEvents Start) start.x start.y
    , viewVertex (toVertexEvents StartPlusX) end.x start.y
    , viewVertex (toVertexEvents StartPlusY) start.x end.y
    , viewVertex (toVertexEvents End) end.x end.y
    ]


lineVertices : (Vertex -> List (Svg.Attribute Msg)) -> StartPosition -> EndPosition -> List (Svg Msg)
lineVertices toVertexEvents start end =
    [ viewVertex (toVertexEvents Start) start.x start.y
    , viewVertex (toVertexEvents End) end.x end.y
    ]


viewVertex : List (Svg.Attribute Msg) -> Int -> Int -> Svg Msg
viewVertex vertexEvents x y =
    circle
        ([ cx <| toString x
         , cy <| toString y
         , r "5"
         , fill <| Color.Convert.colorToHex Color.blue
         , Attr.stroke "white"
         , Attr.strokeWidth "2"
         , Attr.filter "url(#dropShadow)"
         ]
            ++ vertexEvents
        )
        []


arrowVertices : (Vertex -> List (Svg.Attribute Msg)) -> StartPosition -> EndPosition -> List (Svg Msg)
arrowVertices toVertexEvents start end =
    [ viewVertex (toVertexEvents Start) start.x start.y
    , viewVertex (toVertexEvents End) end.x end.y
    ]


ellipseVertices : (Vertex -> List (Svg.Attribute Msg)) -> StartPosition -> EndPosition -> List (Svg Msg)
ellipseVertices toVertexEvents start end =
    let
        dX =
            end.x - start.x

        dY =
            end.y - start.y

        rectStart =
            Position (start.x - dX) (end.y - 2 * dY)
    in
        shapeVertices toVertexEvents rectStart end


viewTextArea : Int -> SelectState -> TextArea -> Svg Msg
viewTextArea index selectState { start, end, text, fill, fontSize, angle, autoexpand } =
    foreignObject
        []
        [ div
            [ Html.class "text-box-container"
            , Html.style
                [ "top" => toPx (Basics.min start.y end.y)
                , "left" => toPx (Basics.min start.x end.x)
                , "width" => toPx (abs (end.x - start.x))
                , "font-size" => toPx fontSize
                , "color" => Color.Convert.colorToHex fill
                ]
            , Html.attribute "onclick" "event.stopPropagation();"
            ]
            [ AutoExpand.view (Goat.Update.config index fontSize) autoexpand text
            ]
        ]


viewTextBox : List (Svg.Attribute Msg) -> List (Svg Msg) -> AnnotationState -> SelectState -> Int -> TextArea -> List (Svg Msg)
viewTextBox attrs vertices annotationState selectState index ({ start, end, text, fill, fontSize, angle, autoexpand } as textBox) =
    case selectState of
        Selected ->
            (viewTextArea index selectState textBox)
                |> List.singleton
                |> flip List.append (viewShape ([ Attr.style "opacity: 0;" ] ++ attrs) vertices Rect (Shape start end EmptyFill Color.black SolidThin))

        NotSelected ->
            textBox.text
                |> String.split "\n"
                |> List.map (Svg.tspan [ dy <| toString fontSize, x <| toString <| Basics.min start.x end.x, Attr.fill <| Color.Convert.colorToHex fill ] << List.singleton << Svg.text)
                |> Svg.text_ [ y <| toString <| Basics.min start.y end.y, Attr.style "pointer-events: none; user-select: none;" ]
                |> List.singleton
                |> flip List.append (viewShape ([ Attr.style "stroke: transparent; pointer-events: auto; cursor: pointer;" ] ++ attrs) vertices Rect (Shape start end EmptyFill Color.black SolidThin))

        SelectedWithVertices ->
            textBox.text
                |> String.split "\n"
                |> List.map (Svg.tspan [ dy <| toString fontSize, x <| toString <| Basics.min start.x end.x, Attr.fill <| Color.Convert.colorToHex fill ] << List.singleton << Svg.text)
                |> Svg.text_ [ y <| toString <| Basics.min start.y end.y, Attr.style "pointer-events: none; user-select: none;" ]
                |> List.singleton
                |> flip List.append
                    (viewShape (Attr.strokeWidth "0.5" :: attrs) vertices Rect (Shape start end EmptyFill Color.white SolidThin)
                        ++ viewShape (attrs ++ [ Attr.strokeWidth "0.5", SE.onMouseDown <| StartEditingText index textBox, Attr.style "pointer-events: fill; cursor: pointer;" ]) vertices Rect (Shape start end EmptyFill (Color.rgb 230 230 230) SolidThin)
                    )


viewLine : List (Svg.Attribute Msg) -> List (Svg Msg) -> LineType -> Line -> List (Svg Msg)
viewLine attrs vertices lineType line =
    let
        allAttrs =
            lineAttributes lineType line ++ attrs
    in
        flip List.append vertices <|
            case lineType of
                StraightLine ->
                    [ Svg.path allAttrs [] ]

                Arrow ->
                    [ Svg.path allAttrs [] ]


simpleLineAttrs : Line -> List (Svg.Attribute Msg)
simpleLineAttrs ({ start, end } as line) =
    []
        ++ [ Attr.fill "none"
           , d <| linePath start end
             --  , Attr.filter "url(#dropShadow)"
           ]


linePath : StartPosition -> EndPosition -> String
linePath start end =
    "M" ++ toString start.x ++ "," ++ toString start.y ++ " l" ++ toString (end.x - start.x) ++ "," ++ toString (end.y - start.y)


arrowAttributes : Line -> List (Svg.Attribute Msg)
arrowAttributes line =
    [ markerEnd <| "url(#arrow-head--" ++ Color.Convert.colorToHex line.strokeColor ++ ")" ]


lineAttributes : LineType -> Line -> List (Svg.Attribute Msg)
lineAttributes lineType line =
    case lineType of
        Arrow ->
            arrowAttributes line ++ simpleLineAttrs line ++ strokeAttrs line.strokeStyle line.strokeColor

        StraightLine ->
            simpleLineAttrs line ++ strokeAttrs line.strokeStyle line.strokeColor


viewImage : Image -> Html Msg
viewImage { width, height, url } =
    Html.img
        [ Html.class "image-to-annotate"
        , Html.width (round width)
        , Html.height (round height)
        , src url
        ]
        []


viewResetArrow : Html msg
viewResetArrow =
    svg [ Attr.width "20", Attr.height "20", viewBox "0 0 14.155 14.155" ]
        [ g [ fill "grey" ]
            [ Svg.path [ d "M12.083,1.887c-0.795-0.794-1.73-1.359-2.727-1.697v2.135c0.48,0.239,0.935,0.55,1.334,0.95c1.993,1.994,1.993,5.236,0,7.229c-1.993,1.99-5.233,1.99-7.229,0c-1.991-1.995-1.991-5.235,0-7.229C3.466,3.269,3.482,3.259,3.489,3.25h0.002l1.181,1.179L4.665,0.685L0.923,0.68l1.176,1.176C2.092,1.868,2.081,1.88,2.072,1.887c-2.763,2.762-2.763,7.243,0,10.005c2.767,2.765,7.245,2.765,10.011,0C14.844,9.13,14.847,4.649,12.083,1.887z" ] [] ]
        ]


viewUndoArrow : Html msg
viewUndoArrow =
    svg [ Attr.width "15", Attr.height "8", viewBox "0 0 15 8" ]
        [ Svg.path [ d "M4.036 2.572c3.44-2.342 6.622-1.915 9.262.275C14.11 3.52 14.682 4.2 15 4.682L13.45 6c-.044-.067-.15-.21-.31-.402-.28-.33-.61-.665-.985-.976-1.978-1.64-4.246-2.003-6.866-.335L8 8l-8-.94L2.158 0l1.878 2.572z", fill "currentColor", fillRule "nonzero" ] []
        ]


viewArrowIcon : Html msg
viewArrowIcon =
    svg [ Attr.width "20", Attr.height "20", viewBox "0 0 347.341 347.341" ]
        [ polygon [ points "347.341,107.783 347.339,0 239.559,0.002 282.843,43.285 0,326.128 21.213,347.341 304.056,64.498", fill "#555" ] []
        ]


viewRectangleIcon : Html msg
viewRectangleIcon =
    svg [ Attr.width "14", Attr.height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M2 12h10V2H2v10zM0 0h14v14H0V0z", fillRule "nonzero", fill "#555" ] [] ]


viewSpotlightRectIcon : Html msg
viewSpotlightRectIcon =
    svg [ Attr.width "14", Attr.height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M4 10h6V4H4v6zM0 0h14v14H0V0z", fillRule "nonzero", fill "#555" ] [] ]


viewSpotlightRoundedRectIcon : Html msg
viewSpotlightRoundedRectIcon =
    svg [ Attr.width "14", Attr.height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M4 10h6V4H4v6zM0 0h14v14H0V0z", fillRule "nonzero", fill "#555" ] [] ]


viewSpotlightEllipseIcon : Html msg
viewSpotlightEllipseIcon =
    svg [ Attr.width "14", Attr.height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M4 10h6V4H4v6zM0 0h14v14H0V0z", fillRule "nonzero", fill "#555" ] [] ]


viewRoundedRectangleIcon : Html msg
viewRoundedRectangleIcon =
    svg [ Attr.width "14", Attr.height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M2 3v8c0 .552.448 1 1 1h8c.552 0 1-.448 1-1V3c0-.552-.448-1-1-1H3c-.552 0-1 .448-1 1zM0 3c0-1.655 1.342-3 3-3h8c1.655 0 3 1.342 3 3v8c0 1.655-1.342 3-3 3H3c-1.655 0-3-1.342-3-3V3z", fillRule "nonzero", fill "#555" ] [] ]


viewEllipseIcon : Html msg
viewEllipseIcon =
    svg [ Attr.width "14", Attr.height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M2 7c0 2.757 2.242 5 5 5 2.757 0 5-2.242 5-5 0-2.757-2.242-5-5-5-2.757 0-5 2.242-5 5zM0 7c0-3.866 3.142-7 7-7 3.866 0 7 3.142 7 7 0 3.866-3.142 7-7 7-3.866 0-7-3.142-7-7z", fillRule "nonzero", fill "#555" ] [] ]


viewFillIcon : Fill -> Html msg
viewFillIcon fill =
    svg [ Attr.width "14", Attr.height "14", viewBox "0 0 14 14" ]
        [ case fill of
            SolidFill color ->
                circle
                    [ cx "7"
                    , cy "7"
                    , r "7"
                    , Attr.fill <| Color.Convert.colorToHex color
                    ]
                    []

            SpotlightFill ->
                Svg.text ""

            MaskFill ->
                Svg.text ""

            EmptyFill ->
                Svg.path [ d "M0 7c0-3.866 3.142-7 7-7 3.866 0 7 3.142 7 7 0 3.866-3.142 7-7 7-3.866 0-7-3.142-7-7zm9.793-4.207l-7.07 7.07 1.413 1.415 7.07-7.07-1.413-1.415z", Attr.fill "#555", fillRule "evenodd" ] []
        ]


viewStrokeColorIcon : Color -> Html msg
viewStrokeColorIcon strokeColor =
    svg [ Attr.width "14", Attr.height "14", viewBox "0 0 14 14" ]
        [ Svg.path
            [ d "M2 7c0 2.757 2.242 5 5 5 2.757 0 5-2.242 5-5 0-2.757-2.242-5-5-5-2.757 0-5 2.242-5 5zM0 7c0-3.866 3.142-7 7-7 3.866 0 7 3.142 7 7 0 3.866-3.142 7-7 7-3.866 0-7-3.142-7-7z", fillRule "nonzero", fill <| Color.Convert.colorToHex strokeColor ]
            []
        ]


viewLineStrokeDropdownIcon : Html msg
viewLineStrokeDropdownIcon =
    svg [ Attr.width "20", Attr.height "20", viewBox "0 0 20 20" ]
        [ g [ stroke "#555" ]
            [ line [ x1 "0", x2 "20", y1 "10", y2 "10", strokeWidth "6" ] []
            ]
        ]


viewLineIcon : Html msg
viewLineIcon =
    svg [ Attr.width "20", Attr.height "20", viewBox "0 0 12 12" ]
        [ Svg.path [ d "M11 0L0 11l1 1L12 1z", fillRule "nonzero", fill "#555" ] [] ]


viewNormalLineIcon : Html msg
viewNormalLineIcon =
    svg [ Attr.width "14", Attr.height "2", viewBox "0 0 14 2" ]
        [ Svg.path [ d "M0 2h14V0H0z", fillRule "nonzero", fill "#555" ] [] ]


viewDownArrow : Html msg
viewDownArrow =
    svg [ Attr.height "20", Attr.width "20", viewBox "0 0 48 48" ]
        [ Svg.path [ d "M14 20l10 10 10-10z", fill "grey" ] []
        , Svg.path [ d "M0 0h48v48h-48z", fill "none" ] []
        ]


viewTextIcon : Html msg
viewTextIcon =
    svg [ viewBox "0 0 12 15", Attr.height "12", Attr.width "15" ]
        [ Svg.path [ d "M0 0v4l2-2h3v10.03H3l-1 2h8l-1-2H7V2h3l2 2V0z", fillRule "evenodd" ] []
        ]



-- HELPERS


onMouseDown : Json.Decoder msg -> Html.Attribute msg
onMouseDown decodeToMsg =
    on "mousedown" decodeToMsg


onMouseUp : Json.Decoder msg -> Html.Attribute msg
onMouseUp decodeToMsg =
    on "mouseup" decodeToMsg


onMouseUpOrLeave : Json.Decoder msg -> List (Html.Attribute msg)
onMouseUpOrLeave decodeToMsg =
    [ on "mouseleave" decodeToMsg, onMouseUp decodeToMsg ]
