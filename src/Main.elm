module Main exposing (main)

import Browser
import Flags exposing (Flags, decodeFlags)
import Json.Decode exposing (Value)
import Subscriptions exposing (subscriptions)


batchCommands : ( model, List (Cmd msg) ) -> ( model, Cmd msg )
batchCommands ( model, commands ) =
    ( model, Cmd.batch commands )


batchInit : ( model, List (Cmd msg) ) -> ( model, Cmd msg )
batchInit =
    batchCommands


batchUpdate : (model -> ( model, List (Cmd msg) )) -> model -> ( model, Cmd msg )
batchUpdate fn =
    fn >> batchCommands


main : Program Value Model Msg
main =
    Browser.application
        { init = decodeFlags >> init >> batchInit
        , update = update >> batchUpdate
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlUpdate = onUrlUpdate
        }
