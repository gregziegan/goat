module Goat.Icons exposing (viewUndoArrow, viewArrow, viewRectangle, viewSpotlightRect, viewSpotlightEllipse, viewSpotlightRoundedRect, viewRoundedRectangle, viewEllipse, viewFill, viewStrokeColor, viewLine, viewNormalLine, viewDownArrow, viewText, viewFontSize, viewPencil, viewCornerArrow, viewStrokeStyle)

import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Goat.Model exposing (StrokeStyle(..))
import Html exposing (Html)
import Html.Attributes
import Svg exposing (circle, ellipse, defs, g, mask, polygon, rect, svg, path, linearGradient, stop, use)
import Svg.Attributes as Attr exposing (id, class, cx, cy, d, fill, fillOpacity, fillRule, height, points, offset, opacity, r, rx, ry, stroke, strokeLinecap, stopColor, stopOpacity, strokeWidth, viewBox, width, x, x1, x2, xlinkHref, y, y1, y2)


viewUndoArrow : Html msg
viewUndoArrow =
    svg [ width "15", height "8", viewBox "0 0 15 8" ]
        [ Svg.path [ d "M4.036 2.572c3.44-2.342 6.622-1.915 9.262.275C14.11 3.52 14.682 4.2 15 4.682L13.45 6c-.044-.067-.15-.21-.31-.402-.28-.33-.61-.665-.985-.976-1.978-1.64-4.246-2.003-6.866-.335L8 8l-8-.94L2.158 0l1.878 2.572z", fill "currentColor", fillRule "nonzero" ] []
        ]


viewArrow : Html msg
viewArrow =
    svg [ width "20", height "20", viewBox "0 0 347.341 347.341" ]
        [ polygon [ points "347.341,107.783 347.339,0 239.559,0.002 282.843,43.285 0,326.128 21.213,347.341 304.056,64.498", fill "currentColor" ] []
        ]


viewRectangle : Html msg
viewRectangle =
    svg [ width "14", height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M2 12h10V2H2v10zM0 0h14v14H0V0z", fillRule "nonzero", fill "currentColor" ] [] ]


viewSpotlightRect : Html msg
viewSpotlightRect =
    svg
        [ width "28", height "28", viewBox "0 0 28 28", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg", Html.Attributes.attribute "xmlns:xlink" "http://www.w3.org/1999/xlink" ]
        [ defs []
            [ path [ d "M0 0h28v28H0V0zm6 7v14h16V7H6z", id "rect" ]
                []
            , linearGradient [ id "striped", x1 "50%", x2 "50%", y1 "0%", y2 "100%" ]
                [ stop [ offset "0%", stopColor "#FFF", stopOpacity ".5" ] []
                , stop [ offset "100%", stopOpacity ".5" ] []
                ]
            ]
        , g [ fill "none", fillRule "evenodd" ]
            [ mask [ fill "#fff", id "rectMask" ]
                [ use [ xlinkHref "#rect" ] [] ]
            , g [ fill "url(#striped)", Attr.mask "url(#rectMask)", opacity ".504", stroke "currentColor", strokeLinecap "square" ]
                [ path [ d "M-3 5l34-30M-3 8l34-30M-3 11l34-30M-3 14l34-30M-3 17l34-30M-3 20l34-30M-3 23L31-7M-3 32L31 2M-3 26L31-4M-3 29L31-1M-3 38L31 8M-3 41l34-30M-3 44l34-30M-3 47l34-30M-3 50l34-30M-3 35L31 5M-3 53l34-30M-3 56l34-30" ] [] ]
            , path
                [ stroke "currentColor", strokeWidth "2", d "M7 8h14v12H7z" ]
                []
            ]
        ]


viewSpotlightEllipse : Html msg
viewSpotlightEllipse =
    svg
        [ width "28", height "28", viewBox "0 0 28 28", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg", Html.Attributes.attribute "xmlns:xlink" "http://www.w3.org/1999/xlink" ]
        [ defs []
            [ path [ d "M0 0h28v28H0V0zm14 21c4.418 0 8-3.134 8-7s-3.582-7-8-7-8 3.134-8 7 3.582 7 8 7z", id "ellipse" ] []
            ]
        , g [ fill "none", fillRule "evenodd" ]
            [ mask [ fill "#fff", id "ellipseMask" ]
                [ use [ xlinkHref "#ellipse" ] [] ]
            , g [ fill "url(#striped)", Attr.mask "url(#ellipseMask)", opacity ".504", stroke "currentColor", strokeLinecap "square" ]
                [ path [ d "M-3 5l34-30M-3 8l34-30M-3 11l34-30M-3 14l34-30M-3 17l34-30M-3 20l34-30M-3 23L31-7M-3 32L31 2M-3 26L31-4M-3 29L31-1M-3 38L31 8M-3 41l34-30M-3 44l34-30M-3 47l34-30M-3 50l34-30M-3 35L31 5M-3 53l34-30M-3 56l34-30" ] [] ]
            , ellipse [ cx "14", cy "14", rx "7", ry "6", stroke "currentColor", strokeWidth "2" ] []
            ]
        ]


viewSpotlightRoundedRect : Html msg
viewSpotlightRoundedRect =
    svg
        [ width "28", height "28", viewBox "0 0 28 28", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg", Html.Attributes.attribute "xmlns:xlink" "http://www.w3.org/1999/xlink" ]
        [ defs []
            [ path [ d "M0 0h28v28H0V0zm6 10.993v6.014C6 19.213 7.79 21 9.996 21h8.008C20.21 21 22 19.212 22 17.007v-6.014C22 8.787 20.21 7 18.004 7H9.996C7.79 7 6 8.788 6 10.993z", id "roundedRect" ] []
            ]
        , g [ fill "none", fillRule "evenodd" ]
            [ mask [ fill "#fff", id "roundedRectMask" ]
                [ use [ xlinkHref "#roundedRect" ] [] ]
            , g [ fill "url(#striped)", Attr.mask "url(#roundedRectMask)", opacity ".504", stroke "currentColor", strokeLinecap "square" ]
                [ path [ d "M-3 5l34-30M-3 8l34-30M-3 11l34-30M-3 14l34-30M-3 17l34-30M-3 20l34-30M-3 23L31-7M-3 32L31 2M-3 26L31-4M-3 29L31-1M-3 38L31 8M-3 41l34-30M-3 44l34-30M-3 47l34-30M-3 50l34-30M-3 35L31 5M-3 53l34-30M-3 56l34-30" ] [] ]
            , rect [ width "14", height "12", x "7", y "8", stroke "currentColor", strokeWidth "2", rx "4" ] []
            ]
        ]


viewRoundedRectangle : Html msg
viewRoundedRectangle =
    svg [ width "14", height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M2 3v8c0 .552.448 1 1 1h8c.552 0 1-.448 1-1V3c0-.552-.448-1-1-1H3c-.552 0-1 .448-1 1zM0 3c0-1.655 1.342-3 3-3h8c1.655 0 3 1.342 3 3v8c0 1.655-1.342 3-3 3H3c-1.655 0-3-1.342-3-3V3z", fillRule "nonzero", fill "currentColor" ] [] ]


viewEllipse : Html msg
viewEllipse =
    svg [ width "14", height "14", viewBox "0 0 14 14" ]
        [ Svg.path [ d "M2 7c0 2.757 2.242 5 5 5 2.757 0 5-2.242 5-5 0-2.757-2.242-5-5-5-2.757 0-5 2.242-5 5zM0 7c0-3.866 3.142-7 7-7 3.866 0 7 3.142 7 7 0 3.866-3.142 7-7 7-3.866 0-7-3.142-7-7z", fillRule "nonzero", fill "currentColor" ] [] ]


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
                Svg.path [ d "M0 7c0-3.866 3.142-7 7-7 3.866 0 7 3.142 7 7 0 3.866-3.142 7-7 7-3.866 0-7-3.142-7-7zm9.793-4.207l-7.07 7.07 1.413 1.415 7.07-7.07-1.413-1.415z", fill "currentColor", fillRule "evenodd" ] []
        ]


viewStrokeColor : Color -> Html msg
viewStrokeColor strokeColor =
    svg [ width "14", height "14", viewBox "0 0 14 14" ]
        (if strokeColor == Color.white then
            [ circle [ cx "7", cy "7", r "7", fill "currentColor" ] []
            , circle [ cx "7", cy "7", r "5", stroke "white", fill "currentColor" ] []
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
        [ Svg.path [ d "M11 0L0 11l1 1L12 1z", fillRule "nonzero", fill "currentColor" ] [] ]


viewNormalLine : Html msg
viewNormalLine =
    svg [ width "14", height "2", viewBox "0 0 14 2" ]
        [ Svg.path [ d "M0 2h14V0H0z", fillRule "nonzero", fill "currentColor" ] [] ]


viewDownArrow : Html msg
viewDownArrow =
    svg [ height "20", width "20", viewBox "0 0 48 48" ]
        [ Svg.path [ d "M14 20l10 10 10-10z", fill "grey" ] []
        , Svg.path [ d "M0 0h48v48h-48z", fill "none" ] []
        ]


viewText : Html msg
viewText =
    svg [ viewBox "0 0 20 20", height "20", width "20" ]
        [ Svg.path [ d "M6.07421875,18 L6.07421875,17 L7.99609375,17 L7.99609375,3.1171875 L7.15234375,3.1171875 C6.12890113,3.1171875 5.32226857,3.43359059 4.73242188,4.06640625 C4.14257518,4.69922191 3.62109602,5.82421066 3.16796875,7.44140625 L2.546875,7.44140625 L2.734375,2 L17.2773438,2 L17.4765625,7.44140625 L16.8554688,7.44140625 C16.4257791,5.8710859 15.9101593,4.75781578 15.3085937,4.1015625 C14.7070282,3.44530922 13.9101612,3.1171875 12.9179688,3.1171875 L12,3.1171875 L12,17 L13.9492188,17 L13.9492187,18 L6.07421875,18 Z", fillRule "evenodd", fill "currentColor" ] []
        ]


viewFontSize : Html msg
viewFontSize =
    svg [ viewBox "0 0 20 20", height "20", width "20" ]
        [ Svg.path [ d "M4.99902344,13.3544922 L4.01904297,10.5024414 L2.97753906,13.3544922 L4.99902344,13.3544922 Z M3.56201172,9.54443359 L4.55078125,9.54443359 L6.89306641,16 L5.93505859,16 L5.28027344,14.0664062 L2.72705078,14.0664062 L2.02832031,16 L1.13183594,16 L3.56201172,9.54443359 Z M15.0009766,10.7089844 L13.0410156,5.00488281 L10.9580078,10.7089844 L15.0009766,10.7089844 Z M12.1269531,3.08886719 L14.1044922,3.08886719 L18.7890625,16 L16.8730469,16 L15.5634766,12.1328125 L10.4570312,12.1328125 L9.05957031,16 L7.26660156,16 L12.1269531,3.08886719 Z", fill "currentColor" ] []
        ]


viewPencil : Html msg
viewPencil =
    svg [ width "20", height "20", viewBox "0 0 500 500" ]
        [ Svg.path [ fill "currentColor", d "M492.8,58L442,7.2c-9.6-9.6-25.3-9.6-34.8,0l-17.6,17.6l-1.5,1.5L377.4,37l85.5,85.5l10.8-10.8l1.5-1.5l17.6-17.6C502.4,83.2,502.4,67.6,492.8,58z" ] []
        , Svg.path [ fill "currentColor", d "M51.7,362.4l85.5,85.5l308.5-308.5l-85.5-85.5L51.7,362.4z M395.2,148.7L146.4,397.3l-9.3-9.3l248.8-248.8L395.2,148.7z M111.7,362.6l-9.3-9.3l248.7-248.8l9.3,9.3L111.7,362.6z" ] []
        , Svg.polygon [ fill "currentColor", points "36.4,377.9 14.1,452.9 47.1,485.9 122.1,463.6 79.3,420.7" ] []
        , Svg.polygon [ fill "currentColor", points "0,500 36,489.2 10.8,464" ] []
        ]


viewCornerArrow : Html msg
viewCornerArrow =
    svg [ width "5", height "5", viewBox "0 0 5 5", class "corner-arrow" ]
        [ Svg.path [ d "M5 0L0 5h5", fill "currentColor", fillRule "evenodd" ] [] ]


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
                [ Svg.path [ d "M0 4h16V0H0z", fillRule "nonzero", fill "currentColor" ] [] ]

        SolidVeryThick ->
            svg [ width "14", height "6", viewBox "0 0 14 6" ]
                [ Svg.path [ d "M0 6h16V0H0z", fillRule "nonzero", fill "currentColor" ] [] ]

        DashedThin ->
            svg [ width "14", height "1", viewBox "0 0 14 1" ]
                [ Svg.path [ d "M0 2h4V0H0v2zm5 0h4V0H5v2zm5 0h4V0h-4v2z", fillRule "nonzero", fill "currentColor" ] [] ]

        DashedMedium ->
            svg [ width "14", height "2", viewBox "0 0 14 2" ]
                [ Svg.path [ d "M0 2h4V0H0v2zm5 0h4V0H5v2zm5 0h4V0h-4v2z", fillRule "nonzero", fill "currentColor" ] [] ]

        DashedThick ->
            svg [ width "14", height "2", viewBox "0 0 14 2" ]
                [ Svg.path [ d "M0 2h4V0H0v2zm5 0h4V0H5v2zm5 0h4V0h-4v2z", fillRule "nonzero", fill "currentColor" ] [] ]

        DashedVeryThick ->
            svg [ width "14", height "2", viewBox "0 0 14 2" ]
                [ Svg.path [ d "M0 4h6V0H0v4zm9 0h6V0H9v4z", fillRule "nonzero", fill "currentColor" ] [] ]
