module Svgs exposing (..)

import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


viewResetArrow : Html msg
viewResetArrow =
    svg [ width "20", height "20", viewBox "0 0 14.155 14.155" ]
        [ g [ fill "grey" ]
            [ Svg.path [ d "M12.083,1.887c-0.795-0.794-1.73-1.359-2.727-1.697v2.135c0.48,0.239,0.935,0.55,1.334,0.95c1.993,1.994,1.993,5.236,0,7.229c-1.993,1.99-5.233,1.99-7.229,0c-1.991-1.995-1.991-5.235,0-7.229C3.466,3.269,3.482,3.259,3.489,3.25h0.002l1.181,1.179L4.665,0.685L0.923,0.68l1.176,1.176C2.092,1.868,2.081,1.88,2.072,1.887c-2.763,2.762-2.763,7.243,0,10.005c2.767,2.765,7.245,2.765,10.011,0C14.844,9.13,14.847,4.649,12.083,1.887z" ] [] ]
        ]


viewUndoArrow : Html msg
viewUndoArrow =
    svg [ width "20", height "20", viewBox "0 0 26.676 26.676" ]
        [ g [ fill "grey" ]
            [ Svg.path [ d "M26.105,21.891c-0.229,0-0.439-0.131-0.529-0.346l0,0c-0.066-0.156-1.716-3.857-7.885-4.59c-1.285-0.156-2.824-0.236-4.693-0.25v4.613c0,0.213-0.115,0.406-0.304,0.508c-0.188,0.098-0.413,0.084-0.588-0.033L0.254,13.815C0.094,13.708,0,13.528,0,13.339c0-0.191,0.094-0.365,0.254-0.477l11.857-7.979c0.175-0.121,0.398-0.129,0.588-0.029c0.19,0.102,0.303,0.295,0.303,0.502v4.293c2.578,0.336,13.674,2.33,13.674,11.674c0,0.271-0.191,0.508-0.459,0.562C26.18,21.891,26.141,21.891,26.105,21.891z" ] []
            ]
        ]


viewArrowIcon : Html msg
viewArrowIcon =
    svg [ width "20", height "20", viewBox "0 0 347.341 347.341" ]
        [ polygon [ points "347.341,107.783 347.339,0 239.559,0.002 282.843,43.285 0,326.128 21.213,347.341 304.056,64.498", fill "grey" ] []
        ]


viewRectangleIcon : Html msg
viewRectangleIcon =
    svg [ width "20", height "20", viewBox "0 0 40 40" ]
        [ rect [ x "10", y "10", width "20", height "20", fill "white", stroke "grey" ] []
        ]


viewRoundedRectangleIcon : Html msg
viewRoundedRectangleIcon =
    svg [ width "20", height "20", viewBox "0 0 40 40" ]
        [ rect [ x "10", y "10", width "20", height "20", rx "2", ry "2", fill "white", stroke "grey" ] []
        ]


viewEllipseIcon : Html msg
viewEllipseIcon =
    svg [ width "20", height "20", viewBox "0 0 20 20" ]
        [ ellipse [ cx "10", cy "10", rx "8", ry "5", stroke "grey", strokeWidth "2", fill "white" ] []
        ]


viewFillIcon : Color -> Html msg
viewFillIcon fillColor =
    svg [ width "20", height "20", viewBox "0 0 20 20" ]
        [ circle
            ([ cx "10", cy "10", r "10", fill <| colorToHex fillColor ]
                ++ if fillColor == Color.white then
                    [ stroke "black", r "9" ]
                   else
                    []
            )
            []
        ]


viewLineStrokeDropdownIcon : Html msg
viewLineStrokeDropdownIcon =
    svg [ width "20", height "20", viewBox "0 0 20 20" ]
        [ g [ stroke "grey" ]
            [ line [ x1 "0", x2 "20", y1 "10", y2 "10", strokeWidth "6" ] []
            ]
        ]


viewLineStroke : number -> Html msg
viewLineStroke strokeWidth =
    svg [ width "20", height "20", viewBox "0 0 20 20" ]
        [ g [ stroke "grey" ]
            [ line
                [ x1 "0"
                , x2 "20"
                , y1 "10"
                , y2 "10"
                , Svg.Attributes.strokeWidth <| toString <| strokeWidth
                ]
                []
            ]
        ]


viewDownArrow : Html msg
viewDownArrow =
    svg [ height "20", width "20", viewBox "0 0 48 48" ]
        [ Svg.path [ d "M14 20l10 10 10-10z", fill "grey" ] []
        , Svg.path [ d "M0 0h48v48h-48z", fill "none" ] []
        ]


viewTextIcon : Html msg
viewTextIcon =
    svg [ viewBox "0 0 405 405", height "20", width "20" ]
        [ Svg.path [ fill "grey", d "M34.784,0v132.809h43.61v-33.98c0-7.906,3.012-15.808,9.041-21.837c6.037-6.029,13.943-9.041,21.845-9.041h46.323v262.891c0,7.906-3.012,15.804-9.041,21.841c-6.033,6.029-13.943,9.041-21.841,9.041h-32.99v43.61h221.858v-43.61h-32.981c-7.91,0-15.808-3.012-21.833-9.041c-6.042-6.037-9.05-13.939-9.05-21.841V67.951h46.323c7.91,0,15.808,3.012,21.841,9.041c6.025,6.029,9.041,13.935,9.041,21.837v33.98h43.618V0H34.784z" ] []
        ]
