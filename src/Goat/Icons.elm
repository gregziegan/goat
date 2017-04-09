module Goat.Icons exposing (..)

import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Goat.Model exposing (Fill(..), StrokeStyle(..))
import Html exposing (Html)
import Svg exposing (circle, g, line, polygon, svg)
import Svg.Attributes exposing (class, cx, cy, d, fill, fillRule, height, path, points, r, stroke, viewBox, width, strokeLinecap)


viewUndoArrow : Html msg
viewUndoArrow =
    svg [ width "15", height "8", viewBox "0 0 15 8" ]
        [ Svg.path [ d "M4.036 2.572c3.44-2.342 6.622-1.915 9.262.275C14.11 3.52 14.682 4.2 15 4.682L13.45 6c-.044-.067-.15-.21-.31-.402-.28-.33-.61-.665-.985-.976-1.978-1.64-4.246-2.003-6.866-.335L8 8l-8-.94L2.158 0l1.878 2.572z", fill "currentColor", fillRule "nonzero" ] []
        ]


viewArrow : Html msg
viewArrow =
    svg [ width "20", height "20", viewBox "0 0 347.341 347.341" ]
        [ polygon [ points "347.341,107.783 347.339,0 239.559,0.002 282.843,43.285 0,326.128 21.213,347.341 304.056,64.498", fill "#555" ] []
        ]


viewRectangle : Html msg
viewRectangle =
    svg [ width "14", height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M2 12h10V2H2v10zM0 0h14v14H0V0z", fillRule "nonzero", fill "#555" ] [] ]


viewSpotlightRect : Html msg
viewSpotlightRect =
    svg [ width "14", height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M4 10h6V4H4v6zM0 0h14v14H0V0z", fillRule "nonzero", fill "#555" ] [] ]


viewSpotlightEllipse : Html msg
viewSpotlightEllipse =
    svg [ width "14", height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M4 7c0 1.652 1.347 3 3 3 1.652 0 3-1.347 3-3 0-1.652-1.347-3-3-3-1.652 0-3 1.347-3 3zM0 7c0-3.866 3.142-7 7-7 3.866 0 7 3.142 7 7 0 3.866-3.142 7-7 7-3.866 0-7-3.142-7-7z", fillRule "nonzero", fill "#555" ] [] ]


viewSpotlightRoundedRect : Html msg
viewSpotlightRoundedRect =
    svg [ width "14", height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M4 10h6V4H4v6zM0 3.003C0 1.345 1.342 0 3.003 0h7.994C12.655 0 14 1.342 14 3.003v7.994C14 12.655 12.658 14 10.997 14H3.003C1.345 14 0 12.658 0 10.997V3.003z", fillRule "nonzero", fill "#555" ] [] ]


viewRoundedRectangle : Html msg
viewRoundedRectangle =
    svg [ width "14", height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M2 3v8c0 .552.448 1 1 1h8c.552 0 1-.448 1-1V3c0-.552-.448-1-1-1H3c-.552 0-1 .448-1 1zM0 3c0-1.655 1.342-3 3-3h8c1.655 0 3 1.342 3 3v8c0 1.655-1.342 3-3 3H3c-1.655 0-3-1.342-3-3V3z", fillRule "nonzero", fill "#555" ] [] ]


viewEllipse : Html msg
viewEllipse =
    svg [ width "14", height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M2 7c0 2.757 2.242 5 5 5 2.757 0 5-2.242 5-5 0-2.757-2.242-5-5-5-2.757 0-5 2.242-5 5zM0 7c0-3.866 3.142-7 7-7 3.866 0 7 3.142 7 7 0 3.866-3.142 7-7 7-3.866 0-7-3.142-7-7z", fillRule "nonzero", fill "#555" ] [] ]


viewFill : Fill -> Html msg
viewFill aFill =
    svg [ width "14", height "14", viewBox "0 0 14 14" ]
        [ case aFill of
            SolidFill color ->
                if color == Color.white then
                    circle [ cx "7", cy "7", r "6", stroke "#555", fill "white" ] []
                else
                    circle
                        [ cx "7"
                        , cy "7"
                        , r "7"
                        , fill <| colorToHex color
                        ]
                        []

            SpotlightFill ->
                Svg.text ""

            MaskFill ->
                Svg.text ""

            EmptyFill ->
                Svg.path [ d "M0 7c0-3.866 3.142-7 7-7 3.866 0 7 3.142 7 7 0 3.866-3.142 7-7 7-3.866 0-7-3.142-7-7zm9.793-4.207l-7.07 7.07 1.413 1.415 7.07-7.07-1.413-1.415z", fill "#555", fillRule "evenodd" ] []
        ]


viewStrokeColor : Color -> Html msg
viewStrokeColor strokeColor =
    svg [ width "14", height "14", viewBox "0 0 14 14" ]
        (if strokeColor == Color.white then
            [ circle [ cx "7", cy "7", r "7", fill "#555" ] []
            , circle [ cx "7", cy "7", r "5", stroke "white", fill "#555" ] []
            ]
         else
            [ Svg.path
                [ d "M2 7c0 2.757 2.242 5 5 5 2.757 0 5-2.242 5-5 0-2.757-2.242-5-5-5-2.757 0-5 2.242-5 5zM0 7c0-3.866 3.142-7 7-7 3.866 0 7 3.142 7 7 0 3.866-3.142 7-7 7-3.866 0-7-3.142-7-7z", fillRule "nonzero", fill <| Color.Convert.colorToHex strokeColor ]
                []
            ]
        )


viewLine : Html msg
viewLine =
    svg [ width "20", height "20", viewBox "0 0 12 12" ]
        [ Svg.path [ d "M11 0L0 11l1 1L12 1z", fillRule "nonzero", fill "#555" ] [] ]


viewNormalLine : Html msg
viewNormalLine =
    svg [ width "14", height "2", viewBox "0 0 14 2" ]
        [ Svg.path [ d "M0 2h14V0H0z", fillRule "nonzero", fill "#555" ] [] ]


viewDownArrow : Html msg
viewDownArrow =
    svg [ height "20", width "20", viewBox "0 0 48 48" ]
        [ Svg.path [ d "M14 20l10 10 10-10z", fill "grey" ] []
        , Svg.path [ d "M0 0h48v48h-48z", fill "none" ] []
        ]


viewText : Html msg
viewText =
    svg [ viewBox "0 0 12 15", height "12", width "15" ]
        [ Svg.path [ d "M0 0v4l2-2h3v10.03H3l-1 2h8l-1-2H7V2h3l2 2V0z", fillRule "evenodd" ] []
        ]


viewPencil : Html msg
viewPencil =
    svg [ width "20", height "20", viewBox "0 0 500 500" ]
        [ Svg.path [ fill "#555", d "M492.8,58L442,7.2c-9.6-9.6-25.3-9.6-34.8,0l-17.6,17.6l-1.5,1.5L377.4,37l85.5,85.5l10.8-10.8l1.5-1.5l17.6-17.6C502.4,83.2,502.4,67.6,492.8,58z" ] []
        , Svg.path [ fill "#555", d "M51.7,362.4l85.5,85.5l308.5-308.5l-85.5-85.5L51.7,362.4z M395.2,148.7L146.4,397.3l-9.3-9.3l248.8-248.8L395.2,148.7z M111.7,362.6l-9.3-9.3l248.7-248.8l9.3,9.3L111.7,362.6z" ] []
        , Svg.polygon [ fill "#555", points "36.4,377.9 14.1,452.9 47.1,485.9 122.1,463.6 79.3,420.7" ] []
        , Svg.polygon [ fill "#555", points "0,500 36,489.2 10.8,464" ] []
        ]


viewCornerArrow : Html msg
viewCornerArrow =
    svg [ width "5", height "5", viewBox "0 0 5 5", class "corner-arrow" ]
        [ Svg.path [ d "M5 0L0 5h5", fill "#555", fillRule "evenodd" ] [] ]


viewStrokeStyle : StrokeStyle -> Html msg
viewStrokeStyle strokeStyle =
    case strokeStyle of
        SolidThin ->
            svg [ width "14", height "2", viewBox "0 0 14 2" ]
                [ Svg.path [ d "M1 .5h12", stroke "#555", fill "none", fillRule "evenodd", strokeLinecap "square" ] [] ]

        SolidMedium ->
            viewNormalLine

        SolidThick ->
            svg [ width "14", height "4", viewBox "0 0 14 4" ]
                [ Svg.path [ d "M0 4h16V0H0z", fillRule "nonzero", fill "#555" ] [] ]

        SolidVeryThick ->
            svg [ width "14", height "6", viewBox "0 0 14 6" ]
                [ Svg.path [ d "M0 6h16V0H0z", fillRule "nonzero", fill "#555" ] [] ]

        DashedThin ->
            svg [ width "14", height "1", viewBox "0 0 14 1" ]
                [ Svg.path [ d "M0 2h4V0H0v2zm5 0h4V0H5v2zm5 0h4V0h-4v2z", fillRule "nonzero", fill "#555" ] [] ]

        DashedMedium ->
            svg [ width "14", height "2", viewBox "0 0 14 2" ]
                [ Svg.path [ d "M0 2h4V0H0v2zm5 0h4V0H5v2zm5 0h4V0h-4v2z", fillRule "nonzero", fill "#555" ] [] ]

        DashedThick ->
            svg [ width "14", height "2", viewBox "0 0 14 2" ]
                [ Svg.path [ d "M0 2h4V0H0v2zm5 0h4V0H5v2zm5 0h4V0h-4v2z", fillRule "nonzero", fill "#555" ] [] ]

        DashedVeryThick ->
            svg [ width "14", height "2", viewBox "0 0 14 2" ]
                [ Svg.path [ d "M0 4h6V0H0v4zm9 0h6V0H9v4z", fillRule "nonzero", fill "#555" ] [] ]
