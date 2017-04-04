module Main exposing (..)

import Html
import Goat.Model exposing (AnnotationState(..), Flags, Model, init)
import Goat.Subscriptions exposing (subscriptions)
import Goat.Update exposing (Msg(..), update)
import Goat.View exposing (view)
import Rocket


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init >> Rocket.batchInit
        , update = update >> Rocket.batchUpdate
        , view = view
        , subscriptions = subscriptions
        }
