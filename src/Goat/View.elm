module Goat.View exposing (view, viewImage, viewPixelatedImage)

import Array.Hamt as Array exposing (Array)
import Goat.Flags exposing (Image)
import Goat.Helpers exposing (..)
import Goat.Icons as Icons
import Goat.Model exposing (..)
import Goat.Update exposing (Msg(..), autoExpandConfig)
import Goat.View.Annotation as Annotation
import Goat.View.Controls as Controls
import Goat.View.Definitions as Definitions
import Html exposing (Attribute, Html, button, div, h2, h3, img, li, p, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, src, style)
import Html.Events exposing (onClick, onWithOptions)
import Json.Decode as Json
import List.Zipper exposing (Zipper)
import Mouse exposing (Position)
import Rocket exposing ((=>))
import SingleTouch as ST
import Svg exposing (Svg, circle, defs, foreignObject, marker, rect, svg)
import Svg.Attributes as Attr
import Svg.Lazy
import Touch as T


view : Model -> Html Msg
view model =
    case model.images of
        Nothing ->
            case model.context of
                Zendesk ->
                    viewLoadingScreen

                Web ->
                    viewInfoScreen

        Just images ->
            if model.imageSelected then
                viewImageAnnotator model <| List.Zipper.current images
            else
                div [] [ h3 [] [ text "Please select an image to annotate:" ], viewImageSelector images ]


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


viewLoadingScreen : Html Msg
viewLoadingScreen =
    div
        [ class "loader" ]
        [ svg
            [ Attr.width "40px", Attr.height "40px", Attr.viewBox "0 0 50 50" ]
            [ Svg.path
                [ Attr.fill "currentColor", Attr.d "M25.251,6.461c-10.318,0-18.683,8.365-18.683,18.683h4.068c0-8.071,6.543-14.615,14.615-14.615V6.461z" ]
                []
            , Svg.animateTransform
                [ Attr.attributeType "xml", Attr.attributeName "transform", Attr.type_ "rotate", Attr.from "0 25 25", Attr.to "360 25 25", Attr.dur "0.6s", Attr.repeatCount "indefinite" ]
                []
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
            , Controls.viewControls model annotationAttrs (Controls.viewDropdownMenu model.currentDropdown annotationAttrs model)
            , viewDrawingArea model annotationAttrs selectedImage
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
                    definitions spotlights blurs ++ (Svg.Lazy.lazy viewPixelatedImage image :: viewImage image :: toAnnotations False) ++ [ toDrawing start curPos False ]

                spotlightDrawingAndAnnotations =
                    definitions (spotlights ++ [ toDrawing start curPos True ]) blurs ++ (Svg.Lazy.lazy viewPixelatedImage image :: Svg.Lazy.lazy viewImage image :: toAnnotations True) ++ [ toDrawing start curPos False ]
            in
                case drawing of
                    DrawShape _ ->
                        nonSpotlightDrawingAndAnnotations

                    DrawSpotlight _ ->
                        spotlightDrawingAndAnnotations

                    _ ->
                        nonSpotlightDrawingAndAnnotations

        _ ->
            definitions spotlights blurs ++ (Svg.Lazy.lazy viewPixelatedImage image :: viewImage image :: toAnnotations False)


viewDrawingArea : Model -> AnnotationAttributes -> Image -> Html Msg
viewDrawingArea model annotationAttrs image =
    let
        annotations =
            model.edits.present

        toDrawing =
            Annotation.viewDrawing model annotationAttrs

        spotlights =
            Definitions.viewSpotlights model.annotationState annotations

        blurs =
            Definitions.viewBlurs model.annotationState <|
                case model.annotationState of
                    DrawingAnnotation start curPos ->
                        case model.drawing of
                            DrawBlur ->
                                Array.push (Blur start curPos) annotations

                            _ ->
                                annotations

                    _ ->
                        annotations

        nonSpotlights =
            Definitions.viewNonSpotlightAnnotations model.annotationState annotations

        definitions =
            Definitions.viewDefinitions image.width image.height

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


viewPixelatedImage : Image -> Svg Msg
viewPixelatedImage { width, height, url } =
    Svg.image
        [ Attr.width (toString (round width))
        , Attr.height (toString (round height))
        , Attr.xlinkHref url
        , Attr.filter "url(#pixelate)"
        ]
        []


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
