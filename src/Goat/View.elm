module Goat.View exposing (view)

import Goat.Flags exposing (Image)
import Goat.Helpers exposing (..)
import Goat.Model exposing (..)
import Goat.Update exposing (Msg(..), autoExpandConfig)
import Goat.View.Controls as Controls
import Goat.View.DrawingArea as DrawingArea
import Goat.View.ImageSelector as ImageSelector
import Html exposing (Attribute, Html, button, div, h2, h3, img, li, p, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, src, style)
import Html.Events exposing (onClick, onWithOptions)
import List.Zipper exposing (Zipper)
import Svg exposing (Svg, circle, defs, foreignObject, marker, rect, svg)
import Svg.Attributes as Attr


{-
   Current Structure:

   Context: Zendesk
     1. Loading Icon while waiting for images from the editor
     2. Receive Selected Image -> Annotator
     3. Receive list of images -> Image Selector

   Context: Web
     1. Wait for image upload or select default goats
     2. Upload brings you to Image Selector (Should probably just bring you directly to annotator)

   Image Annotator:
      Controls | DrawingArea

   DrawingArea:
      Definitions | PixelatedImage | Image | Annotations

-}


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
                ImageSelector.view images


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
            , DrawingArea.viewDrawingArea model annotationAttrs selectedImage
            ]


viewModals : Model -> Html Msg
viewModals model =
    case model.annotationMenu of
        Just { index, position } ->
            DrawingArea.viewAnnotationMenu position index

        Nothing ->
            text ""


viewModalMask : Bool -> Html Msg
viewModalMask showingAnyMenu =
    div
        [ classList [ ( "modal-mask", True ), ( "hidden", not showingAnyMenu ) ]
        , onClick CloseAllMenus
        ]
        []
