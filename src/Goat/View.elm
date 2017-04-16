module Goat.View exposing (view, viewAnnotation)

import Array.Hamt as Array exposing (Array)
import AutoExpand
import Color exposing (Color)
import Color.Convert
import Goat.ControlOptions as ControlOptions exposing (fontSizes)
import Goat.Helpers exposing (..)
import Goat.Icons as Icons
import Goat.Model exposing (..)
import Goat.Update exposing (Msg(..), autoExpandConfig)
import Html exposing (Attribute, Html, button, div, h2, img, li, p, text, ul)
import Html.Attributes exposing (class, classList, disabled, id, src, style)
import Html.Events exposing (onClick, onWithOptions)
import Json.Decode as Json
import Keyboard.Extra exposing (Key(Shift), KeyChange, isPressed)
import List.Zipper exposing (Zipper)
import Mouse exposing (Position)
import Rocket exposing ((=>))
import SingleTouch as ST
import Svg exposing (Svg, circle, defs, foreignObject, marker, rect, svg)
import Svg.Attributes as Attr
import Svg.Lazy
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
        |> List.map viewImageOption
        |> div [ class "image-selector" ]


viewImageOption : Image -> Html Msg
viewImageOption image =
    button
        [ class "image-option"
        , Html.Attributes.width <| round image.width
        , Html.Attributes.height <| round image.height
        , onClick <| SelectImage image
        ]
        [ img [ src image.url, Html.Attributes.height <| round image.height, Html.Attributes.width <| round image.width ] []
        , div [ onClick <| SelectImage image, class "image-edit-pencil" ]
            [ Icons.viewPencil
            ]
        ]


viewInfoScreen : Html Msg
viewInfoScreen =
    div [ class "droparea" ]
        [ div [ class "info-text" ]
            [ h2 [] [ text "Please drag and drop an image onto the page" ]
            , div [ class "goat-time" ]
                [ p [] [ text "Or, show me the goats!" ]
                , button [ class "goat-button", onClick ShowMeTheGoats ] [ text "🐐" ]
                ]
            ]
        ]


annotationStateAttributes : Model -> AnnotationAttributes
annotationStateAttributes model =
    case model.annotationState of
        SelectedAnnotation _ annotationAttrs ->
            annotationAttrs

        MovingAnnotation _ _ _ annotationAttrs ->
            annotationAttrs

        ResizingAnnotation _ annotationAttrs ->
            annotationAttrs

        EditingATextBox _ annotationAttrs ->
            annotationAttrs

        _ ->
            currentAnnotationAttributes model


viewImageAnnotator : Model -> Image -> Html Msg
viewImageAnnotator model selectedImage =
    let
        annotationAttrs =
            (annotationStateAttributes model)
    in
        div
            [ class "annotation-app"
            ]
            [ viewModals model
            , viewModalMask model.showingAnyMenu
            , viewControls model annotationAttrs (viewDropdownMenu model.currentDropdown annotationAttrs model)
            , viewDrawingArea model annotationAttrs selectedImage
            ]


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
            (List.map (viewDrawingButton drawing) (ControlOptions.drawings (isPressed Shift keyboardState))
                ++ [ viewStrokeColorDropdown toDropdownMenu strokeColor
                   , viewFillDropdown toDropdownMenu fill
                   , viewLineStrokeDropdown toDropdownMenu strokeStyle
                   , viewFontSizeDropdown toDropdownMenu
                   ]
            )
        ]


viewModals : Model -> Html Msg
viewModals model =
    case model.annotationMenu of
        Just { index, position } ->
            viewAnnotationMenu position index

        Nothing ->
            text ""


viewModalMask : Bool -> Html Msg
viewModalMask showingAnyMenu =
    div
        [ classList [ ( "modal-mask", True ), ( "hidden", not showingAnyMenu ) ]
        , onClick CloseAllMenus
        ]
        []


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

        DrawBlur _ ->
            Icons.viewBlur


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


drawingStateEvents : AnnotationState -> List (Attribute Msg)
drawingStateEvents annotationState =
    case annotationState of
        ReadyToDraw ->
            [ onMouseDown <| Json.map (StartDrawing << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchStart T.preventAndStop <| (StartDrawing << toDrawingPosition << toPosition)
            , onWithOptions "contextmenu" defaultPrevented (Json.map ToggleAnnotationMenu Mouse.position)
            ]

        DrawingAnnotation _ _ ->
            [ onMouseUp (Json.map (FinishDrawing << toDrawingPosition) Mouse.position)
            , ST.onSingleTouch T.TouchEnd T.preventAndStop (FinishDrawing << toDrawingPosition << toPosition)
            , ST.onSingleTouch T.TouchMove T.preventAndStop (ContinueDrawing << toDrawingPosition << toPosition)
            , onWithOptions "contextmenu" defaultPrevented (Json.map ToggleAnnotationMenu Mouse.position)
            ]

        MovingAnnotation _ _ _ _ ->
            [ onMouseUp <| Json.map (FinishMovingAnnotation << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchMove T.preventAndStop (MoveAnnotation << toDrawingPosition << toPosition)
            , ST.onSingleTouch T.TouchEnd T.preventAndStop (FinishMovingAnnotation << toDrawingPosition << toPosition)
            , onWithOptions "contextmenu" defaultPrevented (Json.map ToggleAnnotationMenu Mouse.position)
            ]

        ResizingAnnotation _ _ ->
            [ onMouseUp <| Json.map (FinishResizingAnnotation << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchMove T.preventAndStop (ResizeAnnotation << toDrawingPosition << toPosition)
            , ST.onSingleTouch T.TouchEnd T.preventAndStop (FinishResizingAnnotation << toDrawingPosition << toPosition)
            , onWithOptions "contextmenu" defaultPrevented (Json.map ToggleAnnotationMenu Mouse.position)
            ]

        SelectedAnnotation _ _ ->
            [ onMouseDown <| Json.map (StartDrawing << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchStart T.preventAndStop <| (StartDrawing << toDrawingPosition << toPosition)
            ]

        EditingATextBox index _ ->
            [ Html.Events.onMouseDown <| FinishEditingText index
            , ST.onSingleTouch T.TouchStart T.preventAndStop (\_ -> FinishEditingText index)
            , onWithOptions "contextmenu" defaultPrevented (Json.map ToggleAnnotationMenu Mouse.position)
            ]


viewMask : Float -> Float -> Svg msg
viewMask width height =
    rect
        [ Attr.x "0"
        , Attr.y "0"
        , Attr.height <| toString height
        , Attr.width <| toString width
        , Attr.mask "url(#Mask)"
        , Attr.style "pointer-events: none;"
        ]
        []


viewSpotlights : AnnotationState -> Array Annotation -> List (Svg Msg)
viewSpotlights annotationState annotations =
    annotations
        |> Array.toIndexedList
        |> List.filterMap (Maybe.map (viewMaskCutOut annotationState) << spotlightToMaskCutout)
        |> List.concat


viewBlurs : AnnotationState -> Array Annotation -> List (Svg Msg)
viewBlurs annotationState annotations =
    annotations
        |> Array.toIndexedList
        |> List.filterMap (uncurry (viewBlur annotationState))
        |> List.concat


canvasAttributes : Image -> Drawing -> AnnotationState -> List (Svg.Attribute Msg)
canvasAttributes image drawing annotationState =
    [ id "canvas"
    , class "image-edit"
    , style
        [ "width" => toString (round image.width) ++ "px"
        , "height" => toString (round image.height) ++ "px"
        , "cursor" => annotationStateToCursor annotationState
        ]
    , Html.Attributes.contextmenu "annotation-menu"
    ]
        ++ drawingStateEvents annotationState


viewNonSpotlightAnnotations : AnnotationState -> Array Annotation -> List (Svg Msg)
viewNonSpotlightAnnotations annotationState annotations =
    annotations
        |> Array.toList
        |> List.indexedMap (viewAnnotation annotationState)
        |> List.concat


viewDefinitions : Float -> Float -> List (Svg Msg) -> List (Svg Msg) -> List (Svg Msg)
viewDefinitions width height spotlightCutOuts blurCutOuts =
    ControlOptions.strokeColors
        |> List.map viewArrowHeadDefinition
        |> (::) (maskDefinition width height spotlightCutOuts)
        |> (::) (pixelateMaskDefinition blurCutOuts)
        |> flip List.append viewSvgFilters
        |> defs []
        |> List.singleton


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
            List.take firstSpotlightIndex nonSpotlights
                ++ (viewMask image.width image.height
                        :: List.drop firstSpotlightIndex nonSpotlights
                   )


viewDrawingAndAnnotations :
    Image
    -> (List (Svg Msg) -> List (Svg Msg) -> List (Svg Msg))
    -> List (Svg Msg)
    -> List (Svg Msg)
    -> (Bool -> List (Svg Msg))
    -> (StartPosition -> Position -> Bool -> Svg Msg)
    -> Drawing
    -> AnnotationState
    -> List (Svg Msg)
viewDrawingAndAnnotations image definitions spotlights blurs toAnnotations toDrawing drawing annotationState =
    case annotationState of
        DrawingAnnotation start curPos ->
            let
                nonSpotlightDrawingAndAnnotations =
                    definitions spotlights blurs ++ (Svg.Lazy.lazy viewBlurredImage image :: viewImage image :: toAnnotations False) ++ [ toDrawing start curPos False ]

                spotlightDrawingAndAnnotations =
                    definitions (spotlights ++ [ toDrawing start curPos True ]) blurs ++ (Svg.Lazy.lazy viewBlurredImage image :: Svg.Lazy.lazy viewImage image :: toAnnotations True) ++ [ toDrawing start curPos False ]
            in
                case drawing of
                    DrawShape _ _ ->
                        nonSpotlightDrawingAndAnnotations

                    DrawSpotlight _ _ ->
                        spotlightDrawingAndAnnotations

                    _ ->
                        nonSpotlightDrawingAndAnnotations

        _ ->
            definitions spotlights blurs ++ (Svg.Lazy.lazy viewBlurredImage image :: viewImage image :: toAnnotations False)


viewDrawingArea : Model -> AnnotationAttributes -> Image -> Html Msg
viewDrawingArea model annotationAttrs image =
    let
        annotations =
            model.edits.present

        toDrawing =
            viewDrawing model annotationAttrs

        spotlights =
            viewSpotlights model.annotationState annotations

        blurs =
            viewBlurs model.annotationState <|
                case model.annotationState of
                    DrawingAnnotation start curPos ->
                        case model.drawing of
                            DrawBlur _ ->
                                Array.push (Blur start curPos) annotations

                            _ ->
                                annotations

                    _ ->
                        annotations

        nonSpotlights =
            viewNonSpotlightAnnotations model.annotationState annotations

        definitions =
            viewDefinitions image.width image.height

        toAnnotations =
            getAnnotations image annotations spotlights nonSpotlights
    in
        div
            (canvasAttributes image model.drawing model.annotationState)
            [ svg
                [ Attr.id "drawing"
                , Attr.class "drawing"
                , Attr.width <| toString <| round image.width
                , Attr.height <| toString <| round image.height
                , Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg"
                ]
                (viewDrawingAndAnnotations image definitions spotlights blurs toAnnotations toDrawing model.drawing model.annotationState)
            ]


{-| TODO: fix these filters for lines. Lines/Arrows render very strangely with this filter.
-}
viewSvgFilters : List (Svg Msg)
viewSvgFilters =
    [ Svg.filter [ id "pixelate", Attr.x "0", Attr.y "0" ]
        [ Svg.feFlood [ Attr.height "2", Attr.width "2", Attr.x "4", Attr.y "4" ] []
        , Svg.feComposite [ Attr.height "10", Attr.width "10" ] []
        , Svg.feTile [ Attr.result "a" ] []
        , Svg.feComposite [ Attr.in_ "SourceGraphic", Attr.in2 "a", Attr.operator "in" ] []
        , Svg.feMorphology [ Attr.operator "dilate", Attr.radius "5" ] []
        ]
    , Svg.filter [ Attr.id "dropShadow", Attr.x "-20%", Attr.y "-20%", Attr.width "200%", Attr.height "200%" ]
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
    ]


viewArrowHeadDefinition : Color -> Svg Msg
viewArrowHeadDefinition color =
    marker
        [ Attr.id <| "arrow-head--" ++ Color.Convert.colorToHex color
        , Attr.orient "auto"
        , Attr.markerWidth "2"
        , Attr.markerHeight "4"
        , Attr.refX "0.1"
        , Attr.refY "2"
        , Attr.class "pointerCursor"
        ]
        [ Svg.path [ Attr.d "M0,0 V4 L2,2 Z", Attr.fill <| Color.Convert.colorToHex color ] []
        ]


annotationStateEvents : Int -> AnnotationState -> List (Svg.Attribute Msg)
annotationStateEvents annIndex annotationState =
    case annotationState of
        ReadyToDraw ->
            [ Html.Events.onWithOptions "mousedown" stopPropagation <| Json.map (SelectAndMoveAnnotation annIndex << toDrawingPosition) Mouse.position
            , Attr.class "pointerCursor"
            , onWithOptions "contextmenu" (Html.Events.Options True True) (Json.map (ToggleSelectedAnnotationMenu annIndex) Mouse.position)
            ]

        DrawingAnnotation _ _ ->
            [ Attr.class "crosshairCursor" ]

        SelectedAnnotation _ _ ->
            [ Attr.class "moveCursor"
            , Html.Events.onWithOptions "mousedown" stopPropagation <| Json.map (StartMovingAnnotation annIndex << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchStart T.preventAndStop (StartMovingAnnotation annIndex << toDrawingPosition << toPosition)
            , onWithOptions "contextmenu" defaultPrevented (Json.map (ToggleSelectedAnnotationMenu annIndex) Mouse.position)
            ]

        MovingAnnotation index _ ( dx, dy ) _ ->
            [ onMouseUp <| Json.map (FinishMovingAnnotation << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchEnd T.preventAndStop (FinishMovingAnnotation << toDrawingPosition << toPosition)
            , Attr.class "moveCursor"
            ]
                ++ if index == annIndex then
                    [ Attr.transform <| "translate(" ++ toString dx ++ "," ++ toString dy ++ ")" ]
                   else
                    []

        ResizingAnnotation _ _ ->
            [ Attr.class "resizeCursor"
            ]

        EditingATextBox index _ ->
            [ Attr.class "crosshairCursor" ]


getSelectState : Int -> AnnotationState -> SelectState
getSelectState annIndex annotationState =
    case annotationState of
        SelectedAnnotation index _ ->
            if index == annIndex then
                SelectedWithVertices
            else
                NotSelected

        MovingAnnotation index _ _ _ ->
            if index == annIndex then
                SelectedWithVertices
            else
                NotSelected

        ResizingAnnotation { index } _ ->
            if index == annIndex then
                SelectedWithVertices
            else
                NotSelected

        EditingATextBox index _ ->
            if index == annIndex then
                Selected
            else
                NotSelected

        _ ->
            NotSelected


viewMaskCutOut : AnnotationState -> ( Int, ShapeType, Shape ) -> List (Svg Msg)
viewMaskCutOut annotationState ( index, shapeType, shape ) =
    viewShape (annotationStateEvents index annotationState) shapeType (Just Color.black) shape


viewAnnotation : AnnotationState -> Int -> Annotation -> List (Svg Msg)
viewAnnotation annotationState index annotation =
    let
        selectState =
            getSelectState index annotationState

        annotationStateAttrs =
            annotationStateEvents index annotationState

        toVertexEvents =
            annotationStateVertexEvents index annotationState

        vertices verticesType { start, end } =
            viewVertices verticesType start end toVertexEvents selectState
    in
        case annotation of
            Lines lineType shape ->
                viewLine annotationStateAttrs lineType shape
                    |> flip List.append (vertices Linear shape)

            Shapes shapeType fill shape ->
                case shapeType of
                    Ellipse ->
                        viewShape annotationStateAttrs shapeType fill shape
                            |> flip List.append (vertices Elliptical shape)

                    _ ->
                        viewShape annotationStateAttrs shapeType fill shape
                            |> flip List.append (vertices Rectangular shape)

            TextBox textBox ->
                viewTextBox annotationStateAttrs selectState index textBox

            Spotlight shapeType shape ->
                case shapeType of
                    Ellipse ->
                        viewShape annotationStateAttrs shapeType Nothing shape
                            |> flip List.append (vertices Elliptical shape)

                    _ ->
                        viewShape annotationStateAttrs shapeType Nothing shape
                            |> flip List.append (vertices Rectangular shape)

            Blur start end ->
                [ Svg.rect (rectAttrs start end ++ [ Attr.fill "none", Attr.style "pointer-events: all;" ] ++ annotationStateAttrs) [] ]
                    |> flip List.append (vertices Rectangular { start = start, end = end })


viewBlur : AnnotationState -> Int -> Annotation -> Maybe (List (Svg Msg))
viewBlur annotationState index annotation =
    case annotation of
        Blur start end ->
            Just [ Svg.rect (rectAttrs start end ++ [ Attr.fill "black", Attr.style "all" ] ++ (annotationStateEvents index annotationState)) [] ]

        _ ->
            Nothing


annotationStateVertexEvents : Int -> AnnotationState -> Vertex -> ResizeDirection -> List (Svg.Attribute Msg)
annotationStateVertexEvents index annotationState vertex direction =
    [ Html.Events.onWithOptions "mousedown" stopPropagation <| Json.map (StartResizingAnnotation index vertex << toDrawingPosition) Mouse.position
    , ST.onSingleTouch T.TouchStart T.preventAndStop (StartResizingAnnotation index vertex << toDrawingPosition << toPosition)
    , Attr.class (directionToCursor direction)
    ]
        ++ case annotationState of
            MovingAnnotation _ _ ( dx, dy ) _ ->
                [ Attr.transform <| "translate(" ++ toString dx ++ "," ++ toString dy ++ ")" ]

            ResizingAnnotation _ _ ->
                [ onMouseUp <| Json.map (FinishResizingAnnotation << toDrawingPosition) Mouse.position
                , ST.onSingleTouch T.TouchEnd T.preventAndStop (FinishResizingAnnotation << toDrawingPosition << toPosition)
                ]

            _ ->
                []


maskDefinition : Float -> Float -> List (Svg Msg) -> Svg Msg
maskDefinition width height shapes =
    rect
        ([ Attr.x "0"
         , Attr.y "0"
         , Attr.width <| toString width
         , Attr.height <| toString height
         , Attr.opacity "0.5"
         , Attr.fill "white"
         ]
        )
        []
        :: shapes
        |> Svg.mask [ Attr.id "Mask" ]


pixelateMaskDefinition : List (Svg Msg) -> Svg Msg
pixelateMaskDefinition shapes =
    rect
        ([ Attr.x "0"
         , Attr.y "0"
         , Attr.width "100%"
         , Attr.height "100%"
         , Attr.fill "white"
         ]
        )
        []
        :: shapes
        |> Svg.mask [ Attr.id "pixelateMask" ]


viewDrawing : Model -> AnnotationAttributes -> StartPosition -> Position -> Bool -> Svg Msg
viewDrawing { drawing, keyboardState } { strokeColor, fill, strokeStyle, fontSize } start curPos isInMask =
    let
        lineAttrs lineType lineMode =
            lineAttributes lineType <| Shape start (calcLinePos start curPos lineMode) strokeColor strokeStyle

        shapeAttrs shapeType shapeMode =
            shapeAttributes shapeType (Shape start (calcShapePos start curPos shapeMode) strokeColor strokeStyle) fill

        spotlightAttrs shapeType shapeMode =
            if isInMask then
                shapeAttributes shapeType (Shape start (calcShapePos start curPos shapeMode) strokeColor strokeStyle) (Just Color.black)
            else
                shapeAttributes shapeType (Shape start (calcShapePos start curPos shapeMode) strokeColor strokeStyle) Nothing
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
                Svg.rect ((shapeAttributes Rect <| Shape start curPos (Color.rgb 230 230 230) SolidThin) Nothing ++ [ Attr.strokeWidth "1" ]) []

            DrawSpotlight shapeType shapeMode ->
                case shapeType of
                    Rect ->
                        Svg.rect (spotlightAttrs shapeType shapeMode) []

                    RoundedRect ->
                        Svg.rect (spotlightAttrs shapeType shapeMode) []

                    Ellipse ->
                        Svg.ellipse (spotlightAttrs shapeType shapeMode) []

            DrawBlur _ ->
                Svg.text ""


viewShape : List (Svg.Attribute Msg) -> ShapeType -> Maybe Color -> Shape -> List (Svg Msg)
viewShape attrs shapeType fill shape =
    case shapeType of
        Rect ->
            [ Svg.rect (shapeAttributes shapeType shape fill ++ attrs) [] ]

        RoundedRect ->
            [ Svg.rect (shapeAttributes shapeType shape fill ++ attrs) [] ]

        Ellipse ->
            [ Svg.ellipse (shapeAttributes shapeType shape fill ++ attrs) [] ]


viewVertices : Vertices -> StartPosition -> EndPosition -> (Vertex -> ResizeDirection -> List (Svg.Attribute Msg)) -> SelectState -> List (Svg Msg)
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


rectAttrs : StartPosition -> EndPosition -> List (Svg.Attribute Msg)
rectAttrs start end =
    [ Attr.width <| toString <| abs <| end.x - start.x
    , Attr.height <| toString <| abs <| end.y - start.y
    , Attr.x <| toString <| Basics.min start.x end.x
    , Attr.y <| toString <| Basics.min start.y end.y
    , Attr.filter "url(#dropShadow)"
    ]


ellipseAttributes : Shape -> List (Svg.Attribute Msg)
ellipseAttributes { start, end } =
    [ Attr.rx <| toString <| abs <| (end.x - start.x) // 2
    , Attr.ry <| toString <| abs <| (end.y - start.y) // 2
    , Attr.cx <| toString <| start.x + ((end.x - start.x) // 2)
    , Attr.cy <| toString <| start.y + ((end.y - start.y) // 2)
    , Attr.filter "url(#dropShadow)"
    ]


fillAttrs : Maybe Color -> List (Svg.Attribute Msg)
fillAttrs fill =
    case fill of
        Just color ->
            [ Attr.fill <| Color.Convert.colorToHex color
            , Attr.pointerEvents "auto"
            ]

        Nothing ->
            [ Attr.fillOpacity "0"
            , Attr.pointerEvents "visibleStroke"
            ]


shapeAttributes : ShapeType -> Shape -> Maybe Color -> List (Svg.Attribute Msg)
shapeAttributes shapeType shape fill =
    fillAttrs fill
        ++ strokeAttrs shape.strokeStyle shape.strokeColor
        ++ case shapeType of
            Rect ->
                rectAttrs shape.start shape.end

            RoundedRect ->
                rectAttrs shape.start shape.end ++ [ Attr.rx "15", Attr.ry "15" ]

            Ellipse ->
                ellipseAttributes shape


shapeVertices : (Vertex -> ResizeDirection -> List (Svg.Attribute Msg)) -> StartPosition -> EndPosition -> List (Svg Msg)
shapeVertices toVertexEvents start end =
    let
        ( resizeDir1, resizeDir2, resizeDir3, resizeDir4 ) =
            if start.x < end.x && start.y > end.y then
                ( NESW, NWSE, NWSE, NESW )
            else if start.x < end.x && start.y < end.y then
                ( NWSE, NESW, NESW, NWSE )
            else if start.x > end.x && start.y > end.y then
                ( NWSE, NESW, NESW, NWSE )
            else
                ( NESW, NWSE, NWSE, NESW )
    in
        [ viewVertex (toVertexEvents Start resizeDir1) start.x start.y
        , viewVertex (toVertexEvents StartPlusX resizeDir2) end.x start.y
        , viewVertex (toVertexEvents StartPlusY resizeDir3) start.x end.y
        , viewVertex (toVertexEvents End resizeDir4) end.x end.y
        ]


lineVertices : (Vertex -> ResizeDirection -> List (Svg.Attribute Msg)) -> StartPosition -> EndPosition -> List (Svg Msg)
lineVertices toVertexEvents start end =
    [ viewVertex (toVertexEvents Start Move) start.x start.y
    , viewVertex (toVertexEvents End Move) end.x end.y
    ]


viewVertex : List (Svg.Attribute Msg) -> Int -> Int -> Svg Msg
viewVertex vertexEvents x y =
    circle
        ([ Attr.cx <| toString x
         , Attr.cy <| toString y
         , Attr.r "5"
         , Attr.fill <| Color.Convert.colorToHex Color.blue
         , Attr.stroke "white"
         , Attr.strokeWidth "2"
         , Attr.filter "url(#dropShadow)"
         ]
            ++ vertexEvents
        )
        []


ellipseVertices : (Vertex -> ResizeDirection -> List (Svg.Attribute Msg)) -> StartPosition -> EndPosition -> List (Svg Msg)
ellipseVertices toVertexEvents start end =
    shapeVertices toVertexEvents start end


viewTextArea : Int -> TextArea -> Svg Msg
viewTextArea index ({ start, end, fill, fontSize, autoexpand } as textArea) =
    foreignObject
        []
        [ div
            [ class "text-box-container"
            , style
                [ "top" => toPx (Basics.min start.y end.y)
                , "left" => toPx (Basics.min start.x end.x)
                , "width" => toPx (abs (end.x - start.x))
                , "font-size" => toPx fontSize
                , "color" => Color.Convert.colorToHex fill
                ]
            , Html.Events.onWithOptions "mousedown" stopPropagation (Json.succeed PreventTextMouseDown)
            ]
            [ AutoExpand.view (autoExpandConfig index) (fontSizeToLineHeight fontSize) autoexpand textArea.text
            ]
        ]


viewTextBox : List (Svg.Attribute Msg) -> SelectState -> Int -> TextArea -> List (Svg Msg)
viewTextBox attrs selectState index ({ start, end, fill, fontSize } as textBox) =
    case selectState of
        Selected ->
            viewTextArea index textBox
                |> List.singleton

        NotSelected ->
            textBox.text
                |> String.split "\n"
                |> List.map (Svg.tspan [ Attr.dy <| toString <| fontSizeToLineHeight fontSize, Attr.x <| toString <| Basics.min start.x end.x, Attr.fill <| Color.Convert.colorToHex fill, Attr.fontSize <| toString fontSize ] << List.singleton << Svg.text)
                |> Svg.text_ ([ Attr.y <| toString <| Basics.min start.y end.y ] ++ attrs)
                |> List.singleton

        SelectedWithVertices ->
            textBox.text
                |> String.split "\n"
                |> List.map (Svg.tspan [ Attr.dy <| toString <| fontSizeToLineHeight fontSize, Attr.x <| toString <| Basics.min start.x end.x, Attr.fill <| Color.Convert.colorToHex fill ] << List.singleton << Svg.text)
                |> Svg.text_
                    ([ Attr.y <| toString <| Basics.min start.y end.y
                     , Html.Events.onDoubleClick <| FocusTextArea index
                     , ST.onSingleTouch T.TouchStart T.preventAndStop (\_ -> FocusTextArea index)
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


viewLine : List (Svg.Attribute Msg) -> LineType -> Shape -> List (Svg Msg)
viewLine attrs lineType shape =
    case lineType of
        StraightLine ->
            [ Svg.path (lineAttributes lineType shape ++ attrs) [] ]

        Arrow ->
            [ Svg.path (lineAttributes lineType shape ++ attrs) [] ]


simpleLineAttrs : Shape -> List (Svg.Attribute Msg)
simpleLineAttrs { start, end } =
    [ Attr.fill "none"
    , Attr.d <| linePath start end
      --  , Attr.filter "url(#dropShadow)"
    ]


arrowAttributes : Shape -> List (Svg.Attribute Msg)
arrowAttributes shape =
    [ Attr.markerEnd <| "url(#arrow-head--" ++ Color.Convert.colorToHex shape.strokeColor ++ ")" ]


lineAttributes : LineType -> Shape -> List (Svg.Attribute Msg)
lineAttributes lineType shape =
    case lineType of
        Arrow ->
            arrowAttributes shape ++ simpleLineAttrs shape ++ strokeAttrs shape.strokeStyle shape.strokeColor

        StraightLine ->
            simpleLineAttrs shape ++ strokeAttrs shape.strokeStyle shape.strokeColor


viewBlurredImage : Image -> Svg Msg
viewBlurredImage { width, height, url } =
    Svg.image
        [ Attr.width (toString (round width))
        , Attr.height (toString (round height))
        , Attr.xlinkHref url
        , Attr.filter "url(#pixelate)"
        ]
        []



-- Svg.text ""


viewImage : Image -> Svg Msg
viewImage { width, height, url } =
    Svg.image
        [ Attr.class "image-to-annotate"
        , Attr.width (toString (round width))
        , Attr.height (toString (round height))
        , Attr.xlinkHref url
        , Attr.mask "url(#pixelateMask)"
        ]
        []


viewAnnotationMenu : Position -> Maybe Int -> Html Msg
viewAnnotationMenu pos selectedIndex =
    div
        [ id "annotation-menu"
        , class "annotation-menu"
        , style
            [ ( "top", toPx pos.y )
            , ( "left", toPx pos.x )
            ]
        ]
        [ ul [ class "annotation-menu__list" ]
            (case selectedIndex of
                Just index ->
                    [ viewAnnotationMenuItem (BringAnnotationToFront index) "Bring to Front"
                    , viewAnnotationMenuItem (SendAnnotationToBack index) "Send to Back"
                    ]

                Nothing ->
                    [ viewDisabledAnnotationMenuItem "Bring to Front"
                    , viewDisabledAnnotationMenuItem "Send to Back"
                    ]
            )
        ]


viewDisabledAnnotationMenuItem : String -> Html Msg
viewDisabledAnnotationMenuItem buttonText =
    li [ class "annotation-menu__item" ]
        [ button
            [ class "annotation-menu__button"
            , disabled True
            ]
            [ text buttonText ]
        ]


viewAnnotationMenuItem : Msg -> String -> Html Msg
viewAnnotationMenuItem msg buttonText =
    li [ class "annotation-menu__item" ]
        [ button
            [ class "annotation-menu__button"
            , onClick msg
            ]
            [ text buttonText ]
        ]
