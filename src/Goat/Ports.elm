port module Goat.Ports exposing (exportToImage, listenForUpload, setImages, newImage)

import Goat.Model exposing (Image)


-- Talk to Javascript


port exportToImage : Image -> Cmd msg


port listenForUpload : String -> Cmd msg



-- Get data from Javascript


port setImages : (List Image -> msg) -> Sub msg


port newImage : (Image -> msg) -> Sub msg
