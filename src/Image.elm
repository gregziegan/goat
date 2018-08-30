module Image exposing (Id, Image, decode, encode, goats, store)

import Api
import Json.Decode as Decode exposing (Decoder, field)
import Json.Encode as Encode exposing (Value)


type alias Id =
    String


type alias Image =
    { id : Id
    , url : String
    , width : Float
    , height : Float
    , originalWidth : Float
    , originalHeight : Float
    }


encode : Image -> Value
encode image =
    Encode.object
        [ ( "id", Encode.string image.id )
        , ( "url", Encode.string image.url )
        , ( "width", Encode.float image.width )
        , ( "height", Encode.float image.height )
        , ( "originalWidth", Encode.float image.originalWidth )
        , ( "originalHeight", Encode.float image.originalHeight )
        ]


decode : Decoder Image
decode =
    Decode.map6 Image
        (field "id" Decode.string)
        (field "url" Decode.string)
        (field "width" Decode.float)
        (field "height" Decode.float)
        (field "originalWidth" Decode.float)
        (field "originalHeight" Decode.float)


goats : List Image
goats =
    [ { id = "goat-1"
      , url = "docs/images/goat.jpg"
      , width = 300
      , height = 300
      , originalWidth = 300
      , originalHeight = 300
      }
    , { id = "goat-2"
      , url = "docs/images/goat2.jpg"
      , width = 300
      , height = 300
      , originalWidth = 300
      , originalHeight = 300
      }
    ]


store : Image -> Cmd msg
store image =
    Api.store (encode image)
