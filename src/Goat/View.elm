module Goat.View exposing (..)

import Array.Hamt as Array exposing (Array)
import AutoExpand
import Color exposing (Color)
import Color.Convert
import Goat.Helpers exposing (..)
import Goat.Icons as Icons
import Goat.Model exposing (..)
import Goat.Update exposing (Msg(..))
import Html exposing (Attribute, Html, button, div, p, text)
import Html.Attributes as Html exposing (class, classList, disabled, height, id, readonly, src, start, style, type_, width)
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput, onMouseEnter, onMouseLeave, onWithOptions)
import Json.Decode as Json
import Keyboard.Extra as Keyboard exposing (Key(Shift), KeyChange, KeyChange(..), isPressed)
import List.Zipper exposing (Zipper)
import Mouse exposing (Position)
import Rocket exposing ((=>))
import SingleTouch as ST
import Svg exposing (..)
import Svg.Attributes as Attr
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
viewImageAnnotator model selectedImage =
    div
        [ Html.class "annotation-app"
        ]
        [ viewModals model
        , viewModalMask model.showingAnyMenu
        , viewControls model (viewDropdownMenu model.currentDropdown model.drawing model)
        , viewDrawingArea model selectedImage
        ]


viewControls : Model -> (AttributeDropdown -> Html Msg) -> Html Msg
viewControls { edits, keyboardState, drawing, fill, strokeColor, strokeStyle } toDropdownMenu =
    div
        [ Html.class "controls" ]
        [ div [ Html.class "columns" ]
            [ button [ onClick ReturnToImageSelection, Html.class "cancel-button" ] [ Html.text "Cancel" ]
            , button [ onClick Save, Html.class "save-button" ] [ Html.text "Save" ]
            ]
        , viewHistoryControls edits
        , div [ Html.class "columns" ]
            (List.map (viewDrawingButton drawing) (drawingOptions (isPressed Shift keyboardState))
                ++ [ viewStrokeColorDropdown toDropdownMenu strokeColor
                   , viewFillDropdown toDropdownMenu fill
                   , viewLineStrokeDropdown toDropdownMenu strokeStyle
                   , viewFontSizeDropdown drawing toDropdownMenu
                   ]
            )
        ]


viewModals : Model -> Html Msg
viewModals model =
    case model.annotationMenu of
        Just { index, position } ->
            viewAnnotationMenu position index

        Nothing ->
            Html.text ""


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
        [ Html.classList
            [ "drawing-button" => True
            , "drawing-button--selected" => drawingsAreEqual selectedDrawing drawing
              -- , "drawing-button--spotlight"
              --     => (case drawing of
              --             DrawSpotlight shapeType shapeMode ->
              --                 True
              --
              --             _ ->
              --                 False
              --        )
            ]
        , onClick <| ChangeDrawing drawing
        ]
        [ viewShapeSvg drawing ]


viewHistoryControls : UndoList (Array Annotation) -> Html Msg
viewHistoryControls edits =
    div [ Html.class "history-controls" ]
        [ button [ onClick Undo, Html.class "history-button", disabled <| not <| UndoList.hasPast edits ] [ Icons.viewUndoArrow ]
        , button [ onClick Redo, Html.class "history-button flip", disabled <| not <| UndoList.hasFuture edits ] [ Icons.viewUndoArrow ]
        ]


viewFontSizeDropdown : Drawing -> (AttributeDropdown -> Html Msg) -> Html Msg
viewFontSizeDropdown drawing toDropdownMenu =
    div [ Html.class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown Fonts
            , Html.class "drawing-button"
            ]
            [ Icons.viewFontSize
            , Icons.viewCornerArrow
            ]
        , toDropdownMenu Fonts
        ]


viewFontSizeOptions : Int -> Html Msg
viewFontSizeOptions fontSize =
    fontSizes
        |> List.map (viewFontSizeOption fontSize)
        |> div [ Html.class "dropdown-options" ]


viewFillOptions : Maybe Color -> Html Msg
viewFillOptions fill =
    fillOptions
        |> List.map (viewFillOption fill)
        |> div [ Html.class "dropdown-options" ]


viewStrokeColorOptions : Color -> Html Msg
viewStrokeColorOptions strokeColor =
    strokeColorOptions
        |> List.map (viewStrokeColorOption strokeColor)
        |> div [ Html.class "dropdown-options" ]


viewFillOption : Maybe Color -> Maybe Color -> Html Msg
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


viewLineStrokeDropdown : (AttributeDropdown -> Html Msg) -> StrokeStyle -> Html Msg
viewLineStrokeDropdown toDropdownMenu strokeStyle =
    div
        [ Html.class "dropdown-things" ]
        [ button
            [ onClick <| ToggleDropdown Strokes
            , Html.class "drawing-button"
            ]
            [ Icons.viewStrokeStyle strokeStyle
            , Icons.viewCornerArrow
            ]
        , toDropdownMenu Strokes
        ]


viewFillDropdown : (AttributeDropdown -> Html Msg) -> Maybe Color -> Html Msg
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
        [ Icons.viewStrokeStyle strokeStyle ]


drawingStateEvents : Drawing -> AnnotationState -> List (Html.Attribute Msg)
drawingStateEvents drawing annotationState =
    case annotationState of
        ReadyToDraw ->
            [ onMouseDown <| Json.map (StartDrawing << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchStart T.preventAndStop <| (StartDrawing << toDrawingPosition << toPosition)
            ]

        DrawingAnnotation _ _ ->
            [ onMouseUp (Json.map (FinishDrawing << toDrawingPosition) Mouse.position)
            , ST.onSingleTouch T.TouchEnd T.preventAndStop (FinishDrawing << toDrawingPosition << toPosition)
            , ST.onSingleTouch T.TouchMove T.preventAndStop (ContinueDrawing << toDrawingPosition << toPosition)
            ]

        MovingAnnotation index start _ ->
            [ onMouseUp <| Json.map (FinishMovingAnnotation << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchMove T.preventAndStop (MoveAnnotation << toDrawingPosition << toPosition)
            , ST.onSingleTouch T.TouchEnd T.preventAndStop (FinishMovingAnnotation << toDrawingPosition << toPosition)
            ]

        ResizingAnnotation _ ->
            [ onMouseUp <| Json.map (FinishResizingAnnotation << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchMove T.preventAndStop (ResizeAnnotation << toDrawingPosition << toPosition)
            , ST.onSingleTouch T.TouchEnd T.preventAndStop (FinishResizingAnnotation << toDrawingPosition << toPosition)
            ]

        SelectedAnnotation index ->
            [ onMouseDown <| Json.map (StartDrawing << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchStart T.preventAndStop <| (StartDrawing << toDrawingPosition << toPosition)
            , Html.contextmenu "annotation-menu"
            , onWithOptions "contextmenu" defaultPrevented (Json.map (ToggleAnnotationMenu index) Mouse.position)
            ]

        EditingATextBox index ->
            [ Html.Events.onMouseDown <| FinishEditingText index
            , ST.onSingleTouch T.TouchStart T.preventAndStop <| (\_ -> FinishEditingText index)
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
    -> (StartPosition -> Position -> Bool -> Svg Msg)
    -> Drawing
    -> AnnotationState
    -> List (Svg Msg)
viewDrawingAndAnnotations definitions spotlights toAnnotations toDrawing drawing annotationState =
    case annotationState of
        DrawingAnnotation start curPos ->
            let
                nonSpotlightDrawingAndAnnotations start =
                    definitions spotlights ++ toAnnotations False ++ [ toDrawing start curPos False ]

                spotlightDrawingAndAnnotations start =
                    definitions (spotlights ++ [ toDrawing start curPos True ]) ++ toAnnotations True ++ [ toDrawing start curPos False ]
            in
                case drawing of
                    DrawShape shapeType _ ->
                        nonSpotlightDrawingAndAnnotations start

                    DrawSpotlight _ _ ->
                        spotlightDrawingAndAnnotations start

                    _ ->
                        nonSpotlightDrawingAndAnnotations start

        _ ->
            definitions spotlights ++ toAnnotations False


viewDrawingArea : Model -> Image -> Html Msg
viewDrawingArea model image =
    let
        annotations =
            model.edits.present

        toDrawing =
            viewDrawing model

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


{-|
  TODO: fix these filters for lines. Lines/Arrows render very strangely with this filter.
-}
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
            , onWithOptions "contextmenu" defaultPrevented (Json.map (ToggleAnnotationMenu annIndex) Mouse.position)
            ]

        DrawingAnnotation _ _ ->
            [ Attr.class "crosshairCursor" ]

        SelectedAnnotation _ ->
            [ Attr.class "moveCursor"
            , Html.Events.onWithOptions "mousedown" stopPropagation <| Json.map (StartMovingAnnotation annIndex << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchStart T.preventAndStop (StartMovingAnnotation annIndex << toDrawingPosition << toPosition)
            , onWithOptions "contextmenu" defaultPrevented (Json.map (ToggleAnnotationMenu annIndex) Mouse.position)
            ]

        MovingAnnotation index _ ( dx, dy ) ->
            [ onMouseUp <| Json.map (FinishMovingAnnotation << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchEnd T.preventAndStop (FinishMovingAnnotation << toDrawingPosition << toPosition)
            , Attr.class "moveCursor"
            ]
                ++ if index == annIndex then
                    [ Attr.transform <| "translate(" ++ toString dx ++ "," ++ toString dy ++ ")" ]
                   else
                    []

        ResizingAnnotation _ ->
            [ Attr.class "resizeCursor"
            ]

        EditingATextBox index ->
            [ Attr.class "crosshairCursor" ]


getSelectState : Int -> AnnotationState -> SelectState
getSelectState annIndex annotationState =
    case annotationState of
        SelectedAnnotation index ->
            if index == annIndex then
                SelectedWithVertices
            else
                NotSelected

        MovingAnnotation index _ _ ->
            if index == annIndex then
                SelectedWithVertices
            else
                NotSelected

        ResizingAnnotation { index } ->
            if index == annIndex then
                SelectedWithVertices
            else
                NotSelected

        EditingATextBox index ->
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
                viewTextBox annotationStateAttrs annotationState selectState index textBox

            Spotlight shapeType shape ->
                case shapeType of
                    Ellipse ->
                        viewShape annotationStateAttrs shapeType Nothing shape
                            |> flip List.append (vertices Elliptical shape)

                    _ ->
                        viewShape annotationStateAttrs shapeType Nothing shape
                            |> flip List.append (vertices Rectangular shape)


annotationStateVertexEvents : Int -> AnnotationState -> Vertex -> List (Svg.Attribute Msg)
annotationStateVertexEvents index annotationState vertex =
    [ Html.Events.onWithOptions "mousedown" stopPropagation <| Json.map (StartResizingAnnotation index vertex << toDrawingPosition) Mouse.position
    , ST.onSingleTouch T.TouchStart T.preventAndStop (StartResizingAnnotation index vertex << toDrawingPosition << toPosition)
    , Attr.class "resizeCursor"
    ]
        ++ case annotationState of
            MovingAnnotation int start ( dx, dy ) ->
                [ Attr.transform <| "translate(" ++ toString dx ++ "," ++ toString dy ++ ")" ]

            ResizingAnnotation _ ->
                [ onMouseUp <| Json.map (FinishResizingAnnotation << toDrawingPosition) Mouse.position
                , ST.onSingleTouch T.TouchEnd T.preventAndStop (FinishResizingAnnotation << toDrawingPosition << toPosition)
                ]

            _ ->
                []


maskDefinition : AnnotationState -> Float -> Float -> List (Svg Msg) -> Svg Msg
maskDefinition annotationState width height shapes =
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


viewDrawing : Model -> StartPosition -> Position -> Bool -> Svg Msg
viewDrawing { drawing, fill, strokeColor, strokeStyle, fontSize, keyboardState } start curPos isInMask =
    let
        lineAttrs lineType lineMode =
            lineAttributes lineType <| Shape start (calcLinePos start curPos lineMode) strokeColor strokeStyle

        shapeAttrs shapeType shapeMode =
            shapeAttributes shapeType (Shape start (calcShapePos start curPos shapeMode) strokeColor strokeStyle) fill

        spotlightAttrs shapeType shapeMode =
            if isInMask then
                -- maskAttributes shapeType (Shape start (calcShapePos start curPos shapeMode))
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


maskAttributes : ShapeType -> Shape -> List (Svg.Attribute Msg)
maskAttributes shapeType shape =
    shapeAttributes shapeType shape (Just Color.white)


viewShape : List (Svg.Attribute Msg) -> ShapeType -> Maybe Color -> Shape -> List (Svg Msg)
viewShape attrs shapeType fill shape =
    case shapeType of
        Rect ->
            [ Svg.rect (shapeAttributes shapeType shape fill ++ attrs) [] ]

        RoundedRect ->
            [ Svg.rect (shapeAttributes shapeType shape fill ++ attrs) [] ]

        Ellipse ->
            [ Svg.ellipse (shapeAttributes shapeType shape fill ++ attrs) [] ]


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
    , Attr.x <| toString <| Basics.min start.x end.x
    , Attr.y <| toString <| Basics.min start.y end.y
    , Attr.filter "url(#dropShadow)"
    ]


ellipseAttributes : Shape -> List (Svg.Attribute Msg)
ellipseAttributes { start, end } =
    [ Attr.rx <| toString <| abs <| end.x - start.x
    , Attr.ry <| toString <| abs <| end.y - start.y
    , Attr.cx <| toString start.x
    , Attr.cy <| toString start.y
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
                rectAttrs shape

            RoundedRect ->
                rectAttrs shape ++ [ Attr.rx "15", Attr.ry "15" ]

            Ellipse ->
                ellipseAttributes shape


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
            , Html.attribute "onmousedown" "evt ? evt.stopPropagation() : event.stopPropagation();"
            ]
            [ AutoExpand.view (Goat.Update.config index) (fontSizeToLineHeight fontSize) autoexpand text
            ]
        ]


viewTextBox : List (Svg.Attribute Msg) -> AnnotationState -> SelectState -> Int -> TextArea -> List (Svg Msg)
viewTextBox attrs annotationState selectState index ({ start, end, text, fill, fontSize, angle, autoexpand } as textBox) =
    case selectState of
        Selected ->
            (viewTextArea index textBox)
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


viewImage : Image -> Html Msg
viewImage { width, height, url } =
    Html.img
        [ Html.class "image-to-annotate"
        , Html.width (round width)
        , Html.height (round height)
        , src url
        ]
        []


viewAnnotationMenu : Position -> Int -> Html Msg
viewAnnotationMenu pos index =
    div
        [ id "annotation-menu"
        , class "annotation-menu"
        , Html.style
            [ ( "top", toPx pos.y )
            , ( "left", toPx pos.x )
            ]
        , Html.Events.onMouseEnter (SelectAnnotation index)
        ]
        [ Html.ul [ class "annotation-menu__list" ]
            [ viewAnnotationMenuItem (BringAnnotationToFront index) "Bring to Front"
            , viewAnnotationMenuItem (SendAnnotationToBack index) "Send to Back"
            ]
        ]


viewAnnotationMenuItem : Msg -> String -> Html Msg
viewAnnotationMenuItem msg buttonText =
    Html.li [ class "annotation-menu__item" ]
        [ button
            [ Html.class "annotation-menu__button"
            , onClick msg
            ]
            [ Html.text buttonText ]
        ]
