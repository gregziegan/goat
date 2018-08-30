module Main exposing (main)

import Browser
import Goat.Flags exposing (Flags, decodeFlags)
import Goat.Model as Model exposing (Model, init)
import Goat.Subscriptions exposing (subscriptions)
import Goat.Update exposing (Msg, update)
import Goat.View exposing (view)
import Json.Decode exposing (Value)


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
    Browser.element
        { init = decodeFlags >> init >> batchInit
        , update = update >> batchUpdate
        , view = view
        , subscriptions = subscriptions
        }
