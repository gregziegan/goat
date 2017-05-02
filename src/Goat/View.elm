module Goat.View exposing (view)

import Goat.Flags exposing (Image)
import Goat.Model exposing (..)
import Goat.Update exposing (Msg(..), autoExpandConfig)
import Goat.Utils exposing (..)
import Goat.View.Controls as Controls
import Goat.View.DrawingArea as DrawingArea
import Goat.View.ImageSelector as ImageSelector
import Html exposing (Attribute, Html, button, div, h2, h3, img, li, p, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, src, style)
import Html.Events exposing (onClick, onWithOptions)
import List.Zipper exposing (Zipper)


{-
   Current Structure:

   Platform: Zendesk
     1. Welcome screen that asks for images in the comment editor
     2. If image is selected in editor, go to 3. else 4.
     3. Receive Selected Image -> Annotator
     4. Receive list of images -> Image Selector

   Platform: Web
     1. Wait for image upload or select default goats
     2. Upload brings you to the image annotator

   Image Annotator:
      Controls | DrawingArea

   DrawingArea:
      Definitions | PixelatedImage | Image | Annotations


    How views are split up:
      /Views/
        -> Controls = functions for the Drawing controls
        -> DrawingArea = functions that build up a drawing area with its submodules
        /DrawingArea/
           -> Annotation = functions for rendering an individual annotation
           -> Definitions = functions for rendering the svg defs
           -> Vertices = functions for rendering an annotation's vertices on selection
        -> Icons = functions to render svg icons for controls, image selector, etc
        -> ImageSelector = functions to render the image selection gallery
        -> Utils = utility functions for view-specific transformations

-}


view : Model -> Html Msg
view model =
    case model.images of
        Nothing ->
            case model.platform of
                Zendesk ->
                    viewEmptyImagesScreen

                Web ->
                    viewInfoScreen

        Just images ->
            if model.imageSelected then
                viewImageAnnotator model <| List.Zipper.current images
            else
                ImageSelector.view images


viewEmptyImagesScreen : Html Msg
viewEmptyImagesScreen =
    div
        [ class "no-images-page" ]
        [ h2 [] [ text "Welcome to G.O.A.T.!" ]
        , p [] [ text "Please upload an image to the comment editor. The image will show up here for annotating." ]
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
            , Controls.viewControls model annotationAttrs (Controls.viewDropdownMenu model.currentDropdown model.drawing annotationAttrs)
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
