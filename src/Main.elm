module Main exposing (..)

import Goat.Model exposing (AnnotationState(..), Flags, BugReportModel, Msg, init)
import Goat.Ports as Ports
import Goat.Subscriptions exposing (subscriptions)
import Goat.Update exposing (update)
import Goat.View exposing (view)
import Html
import Rocket


main : Program Flags BugReportModel Msg
main =
    Html.programWithFlags
        { init = init >> flip (,) [ Ports.listenForUpload "droparea" ] >> Rocket.batchInit
        , update = update >> Rocket.batchUpdate
        , view = .app >> view
        , subscriptions = .app >> subscriptions
        }
