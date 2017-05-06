module Goat.View.EventUtils exposing (stopPropagation, defaultPrevented, onMouseDown, onMouseUp)

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Json


defaultPrevented : Html.Events.Options
defaultPrevented =
    Html.Events.Options False True


stopPropagation : Html.Events.Options
stopPropagation =
    Html.Events.Options True False


onMouseDown : Json.Decoder msg -> Attribute msg
onMouseDown decodeToMsg =
    on "mousedown" decodeToMsg


onMouseUp : Json.Decoder msg -> Attribute msg
onMouseUp decodeToMsg =
    on "mouseup" decodeToMsg
