module Goat.View.EventUtils exposing (alwaysPreventDefault, onMouseDown, onMouseUp, stopPropagationAndDefault)

import Html exposing (Attribute)
import Html.Events exposing (custom, on)
import Json.Decode as Json


onMouseDown : Json.Decoder msg -> Attribute msg
onMouseDown decodeToMsg =
    on "mousedown" decodeToMsg


onMouseUp : Json.Decoder msg -> Attribute msg
onMouseUp decodeToMsg =
    on "mouseup" decodeToMsg


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


stopPropagationAndDefault event decoder =
    custom event (Json.map (\msg -> { message = msg, stopPropagation = True, preventDefault = True }) decoder)
