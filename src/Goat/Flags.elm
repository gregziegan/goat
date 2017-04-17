module Goat.Flags exposing (Flags, Image)


type alias Flags =
    { isMac : Bool
    , inZendesk : Bool
    }


type alias Image =
    { id : String
    , url : String
    , width : Float
    , height : Float
    , originalWidth : Float
    , originalHeight : Float
    }
