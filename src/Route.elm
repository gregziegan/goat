module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Image
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)



-- ROUTING


type Route
    = Upload
    | Root
    | Annotate (Maybe Image.Id)
    | Gallery
    | Goats


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Upload Parser.top
        , Parser.map Upload (s "upload")
        , Parser.map (Annotate << Just) (s "annotate" </> string)
        , Parser.map (Annotate Nothing) (s "annotate")
        , Parser.map Goats (s "gallery" </> s "goats")
        , Parser.map Gallery (s "gallery")
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser



-- INTERNAL


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Upload ->
                    []

                Root ->
                    []

                Annotate (Just id) ->
                    [ "annotate", id ]

                Annotate Nothing ->
                    [ "annotate" ]

                Gallery ->
                    [ "gallery" ]

                Goats ->
                    [ "gallery", "goats" ]
    in
    "#/" ++ String.join "/" pieces
