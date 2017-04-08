module Goat.View exposing (..)

import Array.Hamt as Array exposing (Array)
import AutoExpand
import Color exposing (Color)
import Color.Convert
import Goat.Helpers exposing (..)
import Goat.Icons as Icons
import Goat.Model exposing (..)
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
import SingleTouch as ST
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import Svg.Events as SE
import Touch as T
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
            [ Icons.viewPencil
            ]
        ]


viewInfoScreen : Html Msg
viewInfoScreen =
    div [ Html.class "droparea" ]
        [ div [ Html.class "info-text" ]
            [ Html.h2 [] [ Html.text "Please drag and drop an image onto the page" ]
            , div [ Html.class "goat-time" ]
                [ p [] [ Html.text "Or, show me the goats!" ]
                , button [ Html.class "goat-button", onClick ShowMeTheGoats ] [ Html.text "🐐" ]
                ]
            ]
        ]


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
        [ button [ onClick Undo, Html.class "history-button", disabled <| not <| UndoList.hasPast edits ] [ Icons.viewUndoArrow ]
        , button [ onClick Redo, Html.class "history-button flip", disabled <| not <| UndoList.hasFuture edits ] [ Icons.viewUndoArrow ]
        ]


viewTextSizeDropdown : Drawing -> (AttributeDropdown -> Html Msg) -> Html Msg
viewTextSizeDropdown drawing toDropdownMenu =
    div [ Html.class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown Fonts
            , Html.classList [ "drawing-button" => True, "drawing-button--selected" => drawingsAreEqual drawing DrawTextBox ]
            ]
            [ Icons.viewText
            , Icons.viewCornerArrow
            ]
        , toDropdownMenu Fonts
        ]


viewFontSizeOptions : Int -> Html Msg
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
        [ Icons.viewFill fill ]


viewStrokeColorOption : Color -> Color -> Html Msg
viewStrokeColorOption selectedColor color =
    button
        [ Html.classList
            [ "dropdown-button" => True
            , "dropdown-button--selected" => selectedColor == color
            ]
        , onClick (SelectStrokeColor color)
        ]
        [ Icons.viewStrokeColor color ]


viewFontSizeOption : Int -> Int -> Html Msg
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
            [ Icons.viewLineStrokeDropdown
            , Icons.viewCornerArrow
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
            [ Icons.viewFill fill
            , Icons.viewCornerArrow
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
            [ Icons.viewStrokeColor strokeColor
            , Icons.viewCornerArrow
            ]
        , toDropdownMenu StrokeColors
        ]


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
                    Icons.viewLine

                Arrow ->
                    Icons.viewArrow

        DrawShape shapeType _ ->
            case shapeType of
                Rect ->
                    Icons.viewRectangle

                RoundedRect ->
                    Icons.viewRoundedRectangle

                Ellipse ->
                    Icons.viewEllipse

        DrawTextBox ->
            Icons.viewText

        DrawSpotlight shapeType _ ->
            case shapeType of
                Rect ->
                    Icons.viewSpotlightRect

                RoundedRect ->
                    Icons.viewSpotlightRoundedRect

                Ellipse ->
                    Icons.viewSpotlightEllipse


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
                Icons.viewNormalLine

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
            , ST.onSingleTouch T.TouchStart T.preventAndStop <| (StartDrawing << toDrawingPosition << toPosition)
            ]

        DrawingAnnotation start ->
            onMouseUpOrLeave (Json.map (FinishDrawing start << toDrawingPosition) Mouse.position)
                ++ [ ST.onSingleTouch T.TouchEnd T.preventAndStop (FinishDrawing start << toDrawingPosition << toPosition)
                   , ST.onSingleTouch T.TouchMove T.preventAndStop (ContinueDrawing << toDrawingPosition << toPosition)
                   ]

        MovingAnnotation index annotation start ->
            [ Html.Events.onMouseLeave ResetToReadyToDraw
            , onMouseUp <| Json.map (FinishMovingAnnotation index annotation start << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchMove T.preventAndStop (MoveAnnotation index annotation start << toDrawingPosition << toPosition)
            , ST.onSingleTouch T.TouchEnd T.preventAndStop (FinishMovingAnnotation index annotation start << toDrawingPosition << toPosition)
            ]

        ResizingAnnotation index annotation start vertex ->
            [ Html.Events.onMouseLeave ResetToReadyToDraw
            , onMouseUp <| Json.map (FinishResizingAnnotation index annotation vertex start << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchMove T.preventAndStop (ResizeAnnotation index annotation vertex start << toDrawingPosition << toPosition)
            , ST.onSingleTouch T.TouchEnd T.preventAndStop (FinishResizingAnnotation index annotation vertex start << toDrawingPosition << toPosition)
            ]

        SelectedAnnotation index annotation ->
            [ onMouseDown <| Json.map (StartDrawing << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchStart T.preventAndStop <| (StartDrawing << toDrawingPosition << toPosition)
            ]

        EditingATextBox index ->
            [ SE.onMouseDown <| FinishEditingText index
            , ST.onSingleTouch T.TouchStart T.preventAndStop <| (\_ -> FinishEditingText index)
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
            [ onMouseDown <| Json.map (SelectAnnotation index annotation << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchStart T.preventAndStop (SelectAnnotation index annotation << toDrawingPosition << toPosition)
            , Attr.class "pointerCursor"
            , Html.attribute "onmousedown" "event.stopPropagation();"
            ]

        DrawingAnnotation start ->
            [ Attr.class "crosshairCursor" ]

        SelectedAnnotation start _ ->
            [ Attr.class "moveCursor"
            , onMouseDown <| Json.map (StartMovingAnnotation index annotation << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchStart T.preventAndStop (StartMovingAnnotation index annotation << toDrawingPosition << toPosition)
            , Html.attribute "onmousedown" "event.stopPropagation();"
            ]

        MovingAnnotation index _ start ->
            [ onMouseUp <| Json.map (FinishMovingAnnotation index annotation start << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchEnd T.preventAndStop (FinishMovingAnnotation index annotation start << toDrawingPosition << toPosition)
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

        MovingAnnotation int annotation startition ->
            if index == int then
                SelectedWithVertices
            else
                NotSelected

        ResizingAnnotation int annotation startition vertex ->
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
    [ onMouseDown <| Json.map (StartResizingAnnotation index annotation vertex << toDrawingPosition) Mouse.position
    , ST.onSingleTouch T.TouchStart T.preventAndStop (StartResizingAnnotation index annotation vertex << toDrawingPosition << toPosition)
    , Attr.class "resizeCursor"
    , Html.attribute "onmousedown" "event.stopPropagation();"
    ]
        ++ case annotationState of
            ResizingAnnotation int annotation start vertex ->
                [ onMouseUp <| Json.map (FinishResizingAnnotation index annotation vertex start << toDrawingPosition) Mouse.position
                , ST.onSingleTouch T.TouchEnd T.preventAndStop (FinishResizingAnnotation index annotation vertex start << toDrawingPosition << toPosition)
                ]

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


viewTextArea : Int -> TextArea -> Svg Msg
viewTextArea index { start, end, text, fill, fontSize, angle, autoexpand } =
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
            [ AutoExpand.view (Goat.Update.config index) (toFloat fontSize * 1.2) autoexpand text
            ]
        ]


viewTextBox : List (Svg.Attribute Msg) -> List (Svg Msg) -> AnnotationState -> SelectState -> Int -> TextArea -> List (Svg Msg)
viewTextBox attrs vertices annotationState selectState index ({ start, end, text, fill, fontSize, angle, autoexpand } as textBox) =
    case selectState of
        Selected ->
            (viewTextArea index textBox)
                |> List.singleton

        NotSelected ->
            textBox.text
                |> String.split "\n"
                |> List.map (Svg.tspan [ dy <| toString fontSize, x <| toString <| Basics.min start.x end.x, Attr.fill <| Color.Convert.colorToHex fill, Attr.fontSize <| toString fontSize ] << List.singleton << Svg.text)
                |> Svg.text_ ([ y <| toString <| Basics.min start.y end.y ] ++ attrs)
                |> List.singleton

        SelectedWithVertices ->
            textBox.text
                |> String.split "\n"
                |> List.map (Svg.tspan [ dy <| toString fontSize, x <| toString <| Basics.min start.x end.x, Attr.fill <| Color.Convert.colorToHex fill ] << List.singleton << Svg.text)
                |> Svg.text_
                    ([ y <| toString <| Basics.min start.y end.y
                     , Html.Events.onDoubleClick <| StartEditingText index textBox
                     , ST.onSingleTouch T.TouchStart T.preventAndStop (\_ -> StartEditingText index textBox)
                     , Attr.stroke <|
                        if fill == Color.black then
                            "white"
                        else
                            "black"
                     , Attr.strokeWidth "0.5px"
                     , Attr.fontSize <| toString fontSize
                     ]
                        ++ attrs
                    )
                |> List.singleton


svgTextHeight : Int -> String -> Int
svgTextHeight fontSize text =
    text
        |> String.split "\n"
        |> List.length
        |> (*) fontSize


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
