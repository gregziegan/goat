port module Api exposing (clear, store)

import Json.Decode exposing (Value)


store : Value -> Cmd msg
store val =
    storeCache (Just val)


clear : Cmd msg
clear =
    storeCache Nothing


port storeCache : Maybe Value -> Cmd msg
