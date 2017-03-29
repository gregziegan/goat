module AutoExpand exposing (Config, State, initState, config, view)

{-|
Taken from https://github.com/ohanhi/autoexpand/blob/master/src/AutoExpand.elm
Testing this out without forking: experimenting with a more permissive API

This library lets you use automatically expanding textareas in Elm.
This means the textarea grows in height until it reaches the maximum number of
rows allowed and then becomes a scrollable box.
# View
@docs view
# Configuration
@docs config, Config
# State
@docs State, initState
-}

import Html exposing (Html, div, p, br, textarea, text)
import Html.Attributes exposing (rows, style)
import Html.Events exposing (onInput, on)
import Json.Decode exposing (Decoder, field, at, map, map2, int, string)


{-| Keeps track of how many rows we need.
-}
type State
    = State Int


type alias ConfigInternal msg =
    { onInput : { textValue : String, state : State } -> msg
    , padding : Float
    , lineHeight : Float
    , minRows : Int
    , maxRows : Int
    , attributes : List (Html.Attribute msg)
    }


{-| Configuration for your textarea, describing the look and feel.
**Note:** Your `Config` should *never* be held in your model.
It should only appear in `view` code.
-}
type Config msg
    = Config (ConfigInternal msg)


{-| Create the `Config` for the auto expanding textarea.
A typical configuration might look like this:
    type Msg
        = AutoExpandInput { textValue : String, state : AutoExpand.State }
    config : AutoExpand.Config Msg
    config =
        AutoExpand.config
            { onInput = AutoExpandInput
            , padding = 10
            , lineHeight = 20
            , minRows = 1
            , maxRows = 4
            , attributes = []
            }
-}
config :
    { onInput : { textValue : String, state : State } -> msg
    , padding : Float
    , lineHeight : Float
    , minRows : Int
    , maxRows : Int
    , attributes : List (Html.Attribute msg)
    }
    -> Config msg
config values =
    Config values


{-| Sets up the initial `State` for the textarea using a `Config`.
-}
initState : Config msg -> State
initState (Config config) =
    State config.minRows


{-| Show the textarea on your page.
    view : Model -> Html Msg
    view model =
        AutoExpand.view config model.autoExpandState model.textValue
-}
view : Config msg -> State -> String -> Html msg
view (Config config) (State rowCount) textValue =
    textarea
        (config.attributes
            ++ [ on "input" (inputDecoder config)
               , rows rowCount
               , Html.Attributes.value textValue
               , textareaStyles config rowCount
               ]
        )
        []


getRows : ConfigInternal msg -> Int -> Int
getRows config scrollHeight =
    ((toFloat scrollHeight - 2 * config.padding) / config.lineHeight)
        |> ceiling
        |> clamp config.minRows config.maxRows


inputDecoder : ConfigInternal msg -> Decoder msg
inputDecoder config =
    map2 (\t s -> config.onInput { textValue = t, state = s })
        (at [ "target", "value" ] string)
        (at [ "target", "scrollHeight" ] int
            |> map (State << getRows config)
        )


textareaStyles : ConfigInternal msg -> Int -> Html.Attribute msg
textareaStyles config rowCount =
    [ ( "padding", toString config.padding ++ "px" )
    , ( "box-sizing", "border-box" )
    , ( "line-height", toString config.lineHeight ++ "px" )
    , ( "overflow"
      , if rowCount <= config.maxRows then
            "visible"
        else
            "scroll-y"
      )
    ]
        |> style
