module Svgs exposing (..)

import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


viewArrowIcon : Html msg
viewArrowIcon =
    svg [ width "20", height "20", viewBox "0 0 20 20" ]
        [ g [ stroke "grey" ]
            [ line [ x1 "5", x2 "15", y1 "16", y2 "4", strokeWidth "4" ] [] ]
        ]


viewOvalIcon : Html msg
viewOvalIcon =
    svg [ width "20", height "20", viewBox "0 0 20 20" ]
        [ ellipse [ cx "10", cy "10", rx "8", ry "5", stroke "grey", strokeWidth "2", fill "white" ] []
        ]


viewFillIcon : Color -> Html msg
viewFillIcon fillColor =
    svg [ width "20", height "20", viewBox "0 0 20 20" ]
        [ circle [ cx "10", cy "10", r "10", fill <| colorToHex fillColor ] []
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
