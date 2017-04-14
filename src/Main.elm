module Main exposing (main)

import Goat.Model exposing (Flags, Model, init)
import Goat.Ports as Ports
import Goat.Subscriptions exposing (subscriptions)
import Goat.Update exposing (Msg, update)
import Goat.View exposing (view)
import Html
import Rocket


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init >> flip (,) [ Ports.listenForUpload "droparea" ] >> Rocket.batchInit
        , update = update >> Rocket.batchUpdate
        , view = view
        , subscriptions = subscriptions
        }
