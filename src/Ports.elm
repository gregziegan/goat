port module Ports exposing (exportToImage, listenForUpload, newImage, requestImages, reset, selectText, setImages)

import Json.Decode exposing (Value)



-- Talk to Javascript


{-| Export image by sending the image id to JS. JS will load the b64 image data from the cache using this Id.
-}
port exportToImage : String -> Cmd msg


{-| Tell JS that the app is ready to receive drag n' drop image upload
-}
port listenForUpload : () -> Cmd msg


{-| Try to read in images from Zendesk or "the Goats" when app is loaded
-}
port requestImages : () -> Cmd msg


{-| Select the text of a textarea with provided DOM id
-}
port selectText : String -> Cmd msg



-- Get data from Javascript


{-| Receiving a list of `Image`
-}
port setImages : (Value -> msg) -> Sub msg


{-| Receiving a single `Image`
-}
port newImage : (Value -> msg) -> Sub msg


{-| Resets the app state when working with the G.O.A.T. app in Zendesk platform.
Called in JS when the app pane is closed.
TODO: remove when the "editing previously editing images" feature is implemented
-}
port reset : (() -> msg) -> Sub msg
