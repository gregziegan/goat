module Keyboard.Extra
    exposing
        ( subscriptions
        , ups
        , downs
        , update
        , updateWithKeyChange
        , initialState
        , isPressed
        , arrows
        , wasd
        , arrowsDirection
        , wasdDirection
        , pressedDown
        , Direction(..)
        , Key(..)
        , KeyChange(..)
        , State
        , Msg
        , targetKey
        , forceRelease
        , toCode
        , fromCode
        )

{-| Convenience helpers for working with keyboard inputs.

NOTE: Temporarily using this library while @ohanhi considers adding forceRelease to the API

# Intelligent Helper

Using Keyboard.Extra this way, you get all the help it can provide.
You should not use this together with the plain subscriptions.

@docs State, Msg, subscriptions, initialState, update, KeyChange, updateWithKeyChange

## Helpers
@docs isPressed, pressedDown

## Directions
@docs arrows, wasd, Direction, arrowsDirection, wasdDirection


# Plain Subscriptions

If you prefer to only get "the facts" and do your own handling, use these
subscriptions. Otherwise, you may be more comfortable with the Intelligent Helper.

@docs downs, ups


# Decoder
@docs targetKey


# Keyboard keys
@docs Key


# Low level
@docs forceRelease, fromCode, toCode
-}

import Keyboard exposing (KeyCode)
import Dict exposing (Dict)
import Json.Decode as Json
import Keyboard.Arrows as Arrows exposing (Arrows)


{-| Subscription for key down events.

**Note** When the user presses and holds a key, there will be many of these
messages before the corresponding key up message.
-}
downs : (Key -> msg) -> Sub msg
downs toMsg =
    Keyboard.downs (toMsg << fromCode)


{-| Subscription for key up events.
-}
ups : (Key -> msg) -> Sub msg
ups toMsg =
    Keyboard.ups (toMsg << fromCode)


{-| The message type `Keyboard.Extra` uses.
-}
type Msg
    = Down KeyCode
    | Up KeyCode


{-| You will need to add this to your program's subscriptions.
-}
subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Keyboard.downs Down
        , Keyboard.ups Up
        ]


{-| The internal representation of `Keyboard.Extra`. Useful for type annotation.
-}
type State
    = State (List KeyCode)


{-| Use this to initialize the component.
-}
initialState : State
initialState =
    State []


insert : KeyCode -> List KeyCode -> List KeyCode
insert code list =
    list
        |> remove code
        |> (::) code


remove : KeyCode -> List KeyCode -> List KeyCode
remove code list =
    list
        |> List.filter ((/=) code)


{-| There is an issue with keys sticking in the `pressedDown` list in some
cases. This is a workaround you can try, if you are experiencing this issue:
https://github.com/ohanhi/keyboard-extra/issues/6

Usign this function you can force the removal of a list of keys. Any keys that
are not currently pressed down will be ignored.

Note that this may lead to unexpected situations when the user keeps pressing
on keys involved.

    -- pressedDown state == [ Shift, Control, CharC ]
    newState = forceRelease [ Shift, CharA ] state
    -- pressedDown newState == [ Control, CharC ]

-}
forceRelease : List Key -> State -> State
forceRelease keyList (State state) =
    keyList
        |> List.map toCode
        |> List.foldl (\toRemove pressed -> remove toRemove pressed) state
        |> State


{-| You need to call this (or `updateWithKeyChange`) to have the set of pressed
down keys update. If you need to know exactly what changed just now, have a look
at the `updateWithKeyChange`.
-}
update : Msg -> State -> State
update msg (State state) =
    case msg of
        Down code ->
            State (insert code state)

        Up code ->
            State (remove code state)


{-| The second value `updateWithKeyChange` may return, representing the actual
change that happened during the update.
-}
type KeyChange
    = KeyDown Key
    | KeyUp Key


{-| This alternate update function answers the question: "Did the pressed down
keys in fact change just now?"

You might be wondering why this is a `Maybe KeyChange` &ndash; it's because
`keydown` events happen many times per second when you hold down a key. Thus,
not all incoming messages actually cause a change in the model.

**Note** This is provided for convenience, and may not perform well in real
programs. If you are experiencing slowness or jittering when using
`updateWithKeyChange`, see if the regular `update` makes it go away.
-}
updateWithKeyChange : Msg -> State -> ( State, Maybe KeyChange )
updateWithKeyChange msg (State state) =
    case msg of
        Down code ->
            let
                nextState =
                    insert code state

                change =
                    if List.length nextState /= List.length state then
                        Just (KeyDown (fromCode code))
                    else
                        Nothing
            in
                ( State nextState, change )

        Up code ->
            let
                nextState =
                    remove code state

                change =
                    if List.length nextState /= List.length state then
                        Just (KeyUp (fromCode code))
                    else
                        Nothing
            in
                ( State nextState, change )


{-| Gives the arrow keys' pressed down state as follows:

- `{ x = 0, y = 0 }` when pressing no arrows.
- `{ x =-1, y = 0 }` when pressing the left arrow.
- `{ x = 1, y = 1 }` when pressing the up and right arrows.
- `{ x = 0, y =-1 }` when pressing the down, left, and right arrows (left and right cancel out).
-}
arrows : State -> Arrows
arrows (State state) =
    Arrows.determineArrows state


{-| Similar to `arrows`, gives the W, A, S and D keys' pressed down state.

- `{ x = 0, y = 0 }` when pressing none of W, A, S and D.
- `{ x =-1, y = 0 }` when pressing A.
- `{ x = 1, y = 1 }` when pressing W and D.
- `{ x = 0, y =-1 }` when pressing A, S and D (A and D cancel out).
-}
wasd : State -> Arrows
wasd (State state) =
    Arrows.determineWasd state


{-| Type representation of the arrows.
-}
type Direction
    = North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West
    | NorthWest
    | NoDirection


{-| Gives the arrow keys' pressed down state as follows:

- `None` when pressing no arrows.
- `West` when pressing the left arrow.
- `NorthEast` when pressing the up and right arrows.
- `South` when pressing the down, left, and right arrows (left and right cancel out).
-}
arrowsDirection : State -> Direction
arrowsDirection =
    arrowsToDir << arrows


{-| Similar to `arrows`, gives the W, A, S and D keys' pressed down state.

- `None` when pressing none of W, A, S and D.
- `West` when pressing A.
- `NorthEast` when pressing W and D.
- `South` when pressing A, S and D (A and D cancel out).
-}
wasdDirection : State -> Direction
wasdDirection =
    arrowsToDir << wasd


arrowsToDir : Arrows -> Direction
arrowsToDir { x, y } =
    case ( x, y ) of
        ( 0, 1 ) ->
            North

        ( 1, 1 ) ->
            NorthEast

        ( 1, 0 ) ->
            East

        ( 1, -1 ) ->
            SouthEast

        ( 0, -1 ) ->
            South

        ( -1, -1 ) ->
            SouthWest

        ( -1, 0 ) ->
            West

        ( -1, 1 ) ->
            NorthWest

        _ ->
            NoDirection


{-| Check the pressed down state of any `Key`.
-}
isPressed : Key -> State -> Bool
isPressed key (State state) =
    List.member (toCode key) state


{-| Get the full list of keys that are currently pressed down.

The newest key to go down is the first in the list and so on.
-}
pressedDown : State -> List Key
pressedDown (State state) =
    state
        |> List.map fromCode


{-| Convert a key code into a `Key`.
-}
fromCode : KeyCode -> Key
fromCode code =
    codeDict
        |> Dict.get code
        |> Maybe.withDefault Other


{-| Convert a `Key` into a key code.
-}
toCode : Key -> KeyCode
toCode key =
    codeBook
        |> List.filter (((==) key) << Tuple.second)
        |> List.map Tuple.first
        |> List.head
        |> Maybe.withDefault 0


{-| A `Json.Decoder` for grabbing `event.keyCode` and turning it into a `Key`

    import Json.Decode as Json

    onKey : (Key -> msg) -> Attribute msg
    onKey tagger =
      on "keydown" (Json.map tagger targetKey)
-}
targetKey : Json.Decoder Key
targetKey =
    Json.map fromCode (Json.field "keyCode" Json.int)


{-| These are all the keys that have names in `Keyboard.Extra`.
-}
type Key
    = Cancel
    | Help
    | BackSpace
    | Tab
    | Clear
    | Enter
    | Shift
    | Control
    | Alt
    | Pause
    | CapsLock
    | Escape
    | Convert
    | NonConvert
    | Accept
    | ModeChange
    | Space
    | PageUp
    | PageDown
    | End
    | Home
    | ArrowLeft
    | ArrowUp
    | ArrowRight
    | ArrowDown
    | Select
    | Print
    | Execute
    | PrintScreen
    | Insert
    | Delete
    | Number0
    | Number1
    | Number2
    | Number3
    | Number4
    | Number5
    | Number6
    | Number7
    | Number8
    | Number9
    | Colon
    | Semicolon
    | LessThan
    | Equals
    | GreaterThan
    | QuestionMark
    | At
    | CharA
    | CharB
    | CharC
    | CharD
    | CharE
    | CharF
    | CharG
    | CharH
    | CharI
    | CharJ
    | CharK
    | CharL
    | CharM
    | CharN
    | CharO
    | CharP
    | CharQ
    | CharR
    | CharS
    | CharT
    | CharU
    | CharV
    | CharW
    | CharX
    | CharY
    | CharZ
    | Super
    | ContextMenu
    | Sleep
    | Numpad0
    | Numpad1
    | Numpad2
    | Numpad3
    | Numpad4
    | Numpad5
    | Numpad6
    | Numpad7
    | Numpad8
    | Numpad9
    | Multiply
    | Add
    | Separator
    | Subtract
    | Decimal
    | Divide
    | F1
    | F2
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8
    | F9
    | F10
    | F11
    | F12
    | F13
    | F14
    | F15
    | F16
    | F17
    | F18
    | F19
    | F20
    | F21
    | F22
    | F23
    | F24
    | NumLock
    | ScrollLock
    | Circumflex
    | Exclamation
    | DoubleQuote
    | Hash
    | Dollar
    | Percent
    | Ampersand
    | Underscore
    | OpenParen
    | CloseParen
    | Asterisk
    | Plus
    | Pipe
    | HyphenMinus
    | OpenCurlyBracket
    | CloseCurlyBracket
    | Tilde
    | VolumeMute
    | VolumeDown
    | VolumeUp
    | Comma
    | Minus
    | Period
    | Slash
    | BackQuote
    | OpenBracket
    | BackSlash
    | CloseBracket
    | Quote
    | Meta
    | Altgr
    | Other


codeDict : Dict KeyCode Key
codeDict =
    Dict.fromList codeBook


codeBook : List ( KeyCode, Key )
codeBook =
    [ ( 3, Cancel )
    , ( 6, Help )
    , ( 8, BackSpace )
    , ( 9, Tab )
    , ( 12, Clear )
    , ( 13, Enter )
    , ( 16, Shift )
    , ( 17, Control )
    , ( 18, Alt )
    , ( 19, Pause )
    , ( 20, CapsLock )
    , ( 27, Escape )
    , ( 28, Convert )
    , ( 29, NonConvert )
    , ( 30, Accept )
    , ( 31, ModeChange )
    , ( 32, Space )
    , ( 33, PageUp )
    , ( 34, PageDown )
    , ( 35, End )
    , ( 36, Home )
    , ( 37, ArrowLeft )
    , ( 38, ArrowUp )
    , ( 39, ArrowRight )
    , ( 40, ArrowDown )
    , ( 41, Select )
    , ( 42, Print )
    , ( 43, Execute )
    , ( 44, PrintScreen )
    , ( 45, Insert )
    , ( 46, Delete )
    , ( 48, Number0 )
    , ( 49, Number1 )
    , ( 50, Number2 )
    , ( 51, Number3 )
    , ( 52, Number4 )
    , ( 53, Number5 )
    , ( 54, Number6 )
    , ( 55, Number7 )
    , ( 56, Number8 )
    , ( 57, Number9 )
    , ( 58, Colon )
    , ( 59, Semicolon )
    , ( 60, LessThan )
    , ( 61, Equals )
    , ( 62, GreaterThan )
    , ( 63, QuestionMark )
    , ( 64, At )
    , ( 65, CharA )
    , ( 66, CharB )
    , ( 67, CharC )
    , ( 68, CharD )
    , ( 69, CharE )
    , ( 70, CharF )
    , ( 71, CharG )
    , ( 72, CharH )
    , ( 73, CharI )
    , ( 74, CharJ )
    , ( 75, CharK )
    , ( 76, CharL )
    , ( 77, CharM )
    , ( 78, CharN )
    , ( 79, CharO )
    , ( 80, CharP )
    , ( 81, CharQ )
    , ( 82, CharR )
    , ( 83, CharS )
    , ( 84, CharT )
    , ( 85, CharU )
    , ( 86, CharV )
    , ( 87, CharW )
    , ( 88, CharX )
    , ( 89, CharY )
    , ( 90, CharZ )
    , ( 91, Super )
    , ( 93, ContextMenu )
    , ( 95, Sleep )
    , ( 96, Numpad0 )
    , ( 97, Numpad1 )
    , ( 98, Numpad2 )
    , ( 99, Numpad3 )
    , ( 100, Numpad4 )
    , ( 101, Numpad5 )
    , ( 102, Numpad6 )
    , ( 103, Numpad7 )
    , ( 104, Numpad8 )
    , ( 105, Numpad9 )
    , ( 106, Multiply )
    , ( 107, Add )
    , ( 108, Separator )
    , ( 109, Subtract )
    , ( 110, Decimal )
    , ( 111, Divide )
    , ( 112, F1 )
    , ( 113, F2 )
    , ( 114, F3 )
    , ( 115, F4 )
    , ( 116, F5 )
    , ( 117, F6 )
    , ( 118, F7 )
    , ( 119, F8 )
    , ( 120, F9 )
    , ( 121, F10 )
    , ( 122, F11 )
    , ( 123, F12 )
    , ( 124, F13 )
    , ( 125, F14 )
    , ( 126, F15 )
    , ( 127, F16 )
    , ( 128, F17 )
    , ( 129, F18 )
    , ( 130, F19 )
    , ( 131, F20 )
    , ( 132, F21 )
    , ( 133, F22 )
    , ( 134, F23 )
    , ( 135, F24 )
    , ( 144, NumLock )
    , ( 145, ScrollLock )
    , ( 160, Circumflex )
    , ( 161, Exclamation )
    , ( 162, DoubleQuote )
    , ( 163, Hash )
    , ( 164, Dollar )
    , ( 165, Percent )
    , ( 166, Ampersand )
    , ( 167, Underscore )
    , ( 168, OpenParen )
    , ( 169, CloseParen )
    , ( 170, Asterisk )
    , ( 171, Plus )
    , ( 172, Pipe )
    , ( 173, HyphenMinus )
    , ( 174, OpenCurlyBracket )
    , ( 175, CloseCurlyBracket )
    , ( 176, Tilde )
    , ( 181, VolumeMute )
    , ( 182, VolumeDown )
    , ( 183, VolumeUp )
    , ( 186, Semicolon )
    , ( 187, Equals )
    , ( 188, Comma )
    , ( 189, Minus )
    , ( 190, Period )
    , ( 191, Slash )
    , ( 192, BackQuote )
    , ( 219, OpenBracket )
    , ( 220, BackSlash )
    , ( 221, CloseBracket )
    , ( 222, Quote )
    , ( 224, Meta )
    , ( 225, Altgr )
    ]
