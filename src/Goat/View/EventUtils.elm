module Goat.View.EventUtils exposing (stopPropagation, defaultPrevented)

import Html.Events


defaultPrevented : Html.Events.Options
defaultPrevented =
    Html.Events.Options False True


stopPropagation : Html.Events.Options
stopPropagation =
    Html.Events.Options True False
