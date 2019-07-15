module Helpers.Controls exposing (fill, fontSize, isWaiting, key, shapesDropdown, strokeColor, strokeStyle, toDropdown, toImmediateDropdown, waitingDropdown, withDropdown)

import Color exposing (Color)
import Controls exposing (Dropdown, DropdownTrigger(..), DropdownType(..), Msg(..), State, initialState)
import Drawing exposing (Drawing(..), LineType(..), ShapeType(..))
import Drawing.Options as Options exposing (DrawingStyles, Fill, FontSize, StrokeColor, StrokeStyle(..))
import Fuzz exposing (int)
import Keyboard exposing (Key(..))
import Palette


strokeColor : StrokeColor
strokeColor =
    Palette.orange


fill : Fill
fill =
    Just Palette.blue


strokeStyle : StrokeStyle
strokeStyle =
    DashedThin


fontSize : FontSize
fontSize =
    40


key : String -> Key
key character =
    Character character


shapesDropdown : Dropdown
shapesDropdown =
    toDropdown False ShapesDropdown


waitingDropdown : Dropdown
waitingDropdown =
    toDropdown True ShapesDropdown


isWaiting : Dropdown -> Bool
isWaiting dropdown =
    case dropdown.trigger of
        ImmediateDropdown ->
            False

        DelayedDropdown { waiting } ->
            waiting


toDropdown : Bool -> DropdownType -> Dropdown
toDropdown waiting kind =
    case kind of
        ShapesDropdown ->
            Dropdown (DelayedDropdown { waiting = waiting }) kind

        SpotlightsDropdown ->
            Dropdown (DelayedDropdown { waiting = False }) kind

        StrokeColors ->
            Dropdown ImmediateDropdown kind

        Fills ->
            Dropdown ImmediateDropdown kind

        Strokes ->
            Dropdown ImmediateDropdown kind

        Fonts ->
            Dropdown ImmediateDropdown kind


toImmediateDropdown : DropdownType -> Dropdown
toImmediateDropdown kind =
    toDropdown False kind


withDropdown : Dropdown -> State -> State
withDropdown dropdown state =
    { state | currentDropdown = Just dropdown }
