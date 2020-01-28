module Flags exposing (Flags, decodeFlags)

import Environment exposing (OperatingSystem(..), Platform(..))
import Json.Decode as Json


type alias Flags =
    { os : OperatingSystem
    , platform : Platform
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
    Json.map2 Flags
        (Json.map isMacToOS (Json.field "isMac" Json.bool))
        (Json.map isZendeskToPlatform (Json.field "inZendesk" Json.bool))


decodeFlags : Json.Value -> Result Json.Error Flags
decodeFlags =
    Json.decodeValue decodeFlagsHelper
