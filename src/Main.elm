module Main exposing (main)

import Goat.Flags exposing (Flags)
import Goat.Model exposing (Model, init)
import Goat.Subscriptions exposing (subscriptions)
import Goat.Update exposing (Msg, update)
import Goat.View exposing (view)
import Html
import Rocket


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init >> Rocket.batchInit
        , update = update >> Rocket.batchUpdate
        , view = view
        , subscriptions = subscriptions
        }
