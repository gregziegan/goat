module Main exposing (main)

import Goat.Flags exposing (Flags, decodeFlags)
import Goat.Model as Model exposing (Model, init)
import Goat.Subscriptions exposing (subscriptions)
import Goat.Update exposing (Msg, update)
import Goat.View exposing (view)
import Json.Decode exposing (Value)
import Html
import Rocket


main : Program Value Model Msg
main =
    Html.programWithFlags
        { init = decodeFlags >> init >> Rocket.batchInit
        , update = update >> Rocket.batchUpdate
        , view = view
        , subscriptions = subscriptions
        }
