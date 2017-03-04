module Svgs exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)


viewEditIcon : Html msg
viewEditIcon =
    svg [ viewBox "0 0 500 500" ]
        [ g []
            [ Svg.path [ fill "#010101", d "M492.8,58L442,7.2c-9.6-9.6-25.3-9.6-34.8,0l-17.6,17.6l-1.5,1.5L377.4,37l85.5,85.5l10.8-10.8l1.5-1.5l17.6-17.6C502.4,83.2,502.4,67.6,492.8,58z" ] []
            , Svg.path [ fill "#010101", d "M51.7,362.4l85.5,85.5l308.5-308.5l-85.5-85.5L51.7,362.4z M395.2,148.7L146.4,397.3l-9.3-9.3l248.8-248.8L395.2,148.7z M111.7,362.6l-9.3-9.3l248.7-248.8l9.3,9.3L111.7,362.6z" ] []
            , polygon [ fill "#010101", points "36.4,377.9 14.1,452.9 47.1,485.9 122.1,463.6 79.3,420.7" ] []
            , polygon [ fill "#010101", points "0,500 36,489.2 10.8,464" ] []
            ]
        ]


viewLineStrokeDropdownIcon : Html msg
viewLineStrokeDropdownIcon =
    svg [ width "30", height "30", viewBox "0 0 30 30" ]
        [ g [ stroke "grey" ]
            [ line [ x1 "0", x2 "30", y1 "4", y2 "4", strokeWidth "2" ] []
            , line [ x1 "0", x2 "30", y1 "12", y2 "12", strokeWidth "4" ] []
            , line [ x1 "0", x2 "30", y1 "23", y2 "23", strokeWidth "6" ] []
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
    svg [ height "48", viewBox "0 0 48 48", width "48" ]
        [ Svg.path [ d "M14 20l10 10 10-10z", fill "grey" ] []
        , Svg.path [ d "M0 0h48v48h-48z", fill "none" ] []
        ]


viewTextIcon : Html msg
viewTextIcon =
    svg [ viewBox "0 0 405 405", height "48", width "48" ]
        [ Svg.path [ fill "grey", d "M34.784,0v132.809h43.61v-33.98c0-7.906,3.012-15.808,9.041-21.837c6.037-6.029,13.943-9.041,21.845-9.041h46.323v262.891c0,7.906-3.012,15.804-9.041,21.841c-6.033,6.029-13.943,9.041-21.841,9.041h-32.99v43.61h221.858v-43.61h-32.981c-7.91,0-15.808-3.012-21.833-9.041c-6.042-6.037-9.05-13.939-9.05-21.841V67.951h46.323c7.91,0,15.808,3.012,21.841,9.041c6.025,6.029,9.041,13.935,9.041,21.837v33.98h43.618V0H34.784z" ] []
        ]
