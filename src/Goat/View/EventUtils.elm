module Goat.View.EventUtils exposing (stopPropagation, defaultPrevented, stopPropagationAndDefault, onMouseDown, onMouseUp)

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Json


defaultPrevented : Html.Events.Options
defaultPrevented =
    { stopPropagation = False, preventDefault = True }


stopPropagation : Html.Events.Options
stopPropagation =
    { stopPropagation = True, preventDefault = False }


stopPropagationAndDefault : Html.Events.Options
stopPropagationAndDefault =
    { stopPropagation = True, preventDefault = True }


onMouseDown : Json.Decoder msg -> Attribute msg
onMouseDown decodeToMsg =
    on "mousedown" decodeToMsg


onMouseUp : Json.Decoder msg -> Attribute msg
onMouseUp decodeToMsg =
    on "mouseup" decodeToMsg
