module Flags exposing (Flags, decode)

import Environment exposing (Environment, OperatingSystem(..), Platform(..))
import Json.Decode as Json


type alias Flags =
    { environment : Environment
    }


isMacToOS : Bool -> OperatingSystem
isMacToOS isMac =
    if isMac then
        MacOS

    else
        Windows


isZendeskToPlatform : Bool -> Platform
isZendeskToPlatform inZendesk =
    if inZendesk then
        Zendesk

    else
        Web


decodeFlagsHelper : Json.Decoder Flags
decodeFlagsHelper =
    Json.map Flags
        (Json.map2 Environment
            (Json.map isMacToOS (Json.field "isMac" Json.bool))
            (Json.map isZendeskToPlatform (Json.field "inZendesk" Json.bool))
        )


decode : Json.Value -> Result Json.Error Flags
decode =
    Json.decodeValue decodeFlagsHelper
