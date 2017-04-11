module Goat.Icons exposing (..)

import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Goat.Model exposing (StrokeStyle(..))
import Html exposing (Html)
import Svg exposing (circle, g, line, polygon, svg)
import Svg.Attributes exposing (class, cx, cy, rx, d, fill, fillOpacity, fillRule, height, path, points, r, stroke, viewBox, width, strokeLinecap, x, y)


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
    svg [ width "20", height "20", viewBox "0 0 20 20" ]
        [ Svg.path [ d "M0,0 L20,0 L20,20 L0,20 L0,0 Z M3,4 L3,13 L15,13 L15,4 L3,4 Z", fillOpacity "0.337494339", fill "#000000" ] []
        , Svg.rect [ stroke "#000000", x "3.5", y "4.5", width "11", height "8", fill "white" ] []
        ]


viewSpotlightEllipse : Html msg
viewSpotlightEllipse =
    svg [ width "20", height "20", viewBox "0 0 20 20" ]
        [ Svg.path [ d "M0,0 L20,0 L20,20 L0,20 L0,0 Z M3,7.9999433 L3,9.0000567 C3,11.2003465 4.78899235,13 6.99582624,13 L11.0041738,13 C13.2034788,13 15,11.2091644 15,9.0000567 L15,7.9999433 C15,5.79965349 13.2110077,4 11.0041738,4 L6.99582624,4 C4.79652125,4 3,5.79083562 3,7.9999433 Z", fillOpacity "0.337494339", fill "#000000" ] []
        , Svg.rect [ stroke "#000000", x "3.5", y "4.5", width "11", height "8", rx "4", fill "white" ] []
        ]


viewSpotlightRoundedRect : Html msg
viewSpotlightRoundedRect =
    svg [ width "20", height "20", viewBox "0 0 20 20" ]
        [ Svg.path [ d "M0,0 L20,0 L20,20 L0,20 L0,0 Z M3,7.9999433 L3,9.0000567 C3,11.2003465 4.78899235,13 6.99582624,13 L11.0041738,13 C13.2034788,13 15,11.2091644 15,9.0000567 L15,7.9999433 C15,5.79965349 13.2110077,4 11.0041738,4 L6.99582624,4 C4.79652125,4 3,5.79083562 3,7.9999433 Z", fillOpacity "0.337494339", fill "#000000" ] []
        , Svg.rect [ stroke "#000000", x "3.5", y "4.5", width "11", height "8", rx "3", fill "white" ] []
        ]


viewRoundedRectangle : Html msg
viewRoundedRectangle =
    svg [ width "14", height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M2 3v8c0 .552.448 1 1 1h8c.552 0 1-.448 1-1V3c0-.552-.448-1-1-1H3c-.552 0-1 .448-1 1zM0 3c0-1.655 1.342-3 3-3h8c1.655 0 3 1.342 3 3v8c0 1.655-1.342 3-3 3H3c-1.655 0-3-1.342-3-3V3z", fillRule "nonzero", fill "#555" ] [] ]


viewEllipse : Html msg
viewEllipse =
    svg [ width "14", height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M2 7c0 2.757 2.242 5 5 5 2.757 0 5-2.242 5-5 0-2.757-2.242-5-5-5-2.757 0-5 2.242-5 5zM0 7c0-3.866 3.142-7 7-7 3.866 0 7 3.142 7 7 0 3.866-3.142 7-7 7-3.866 0-7-3.142-7-7z", fillRule "nonzero", fill "#555" ] [] ]


viewFill : Maybe Color -> Html msg
viewFill aFill =
    svg [ width "14", height "14", viewBox "0 0 14 14" ]
        [ case aFill of
            Just color ->
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

            Nothing ->
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


viewFontSize : Html msg
viewFontSize =
    svg [ viewBox "0 0 20 20", height "20", width "20" ]
        [ Svg.path [ d "M4.99902344,13.3544922 L4.01904297,10.5024414 L2.97753906,13.3544922 L4.99902344,13.3544922 Z M3.56201172,9.54443359 L4.55078125,9.54443359 L6.89306641,16 L5.93505859,16 L5.28027344,14.0664062 L2.72705078,14.0664062 L2.02832031,16 L1.13183594,16 L3.56201172,9.54443359 Z M15.0009766,10.7089844 L13.0410156,5.00488281 L10.9580078,10.7089844 L15.0009766,10.7089844 Z M12.1269531,3.08886719 L14.1044922,3.08886719 L18.7890625,16 L16.8730469,16 L15.5634766,12.1328125 L10.4570312,12.1328125 L9.05957031,16 L7.26660156,16 L12.1269531,3.08886719 Z", fill "#555" ] []
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
