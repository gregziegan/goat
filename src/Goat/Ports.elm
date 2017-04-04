port module Goat.Ports exposing (..)

import Goat.Model exposing (Image)


port exportToImage : Image -> Cmd msg


port setImages : (List Image -> msg) -> Sub msg
