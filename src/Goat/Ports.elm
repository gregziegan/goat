port module Goat.Ports exposing (exportToImage, listenForUpload, setImages, newImage, reset, requestImages)

import Goat.Flags exposing (Image)


-- Talk to Javascript


port exportToImage : Image -> Cmd msg


port listenForUpload : () -> Cmd msg


port requestImages : () -> Cmd msg



-- Get data from Javascript


port setImages : (List Image -> msg) -> Sub msg


port newImage : (Image -> msg) -> Sub msg


port reset : (() -> msg) -> Sub msg
