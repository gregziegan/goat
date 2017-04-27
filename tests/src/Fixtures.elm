module Fixtures exposing (..)

import Array.Hamt as Array exposing (Array)
import AutoExpand
import Color
import Goat.Flags exposing (Image)
import Goat.Model exposing (..)
import Goat.Update exposing (..)
import Keyboard.Extra as Keyboard
import List.Zipper
import Mouse exposing (Position)
import UndoList


goat : Image
goat =
    { id = "0"
    , url = "goat.jpg"
    , width = 100.0
    , height = 100.0
    , originalWidth = 200.0
    , originalHeight = 200.0
    }


model : Model
model =
    { edits = UndoList.fresh Array.empty
    , waitingForDropdownToggle = Nothing
    , fill = Nothing
    , strokeColor = Color.red
    , strokeStyle = SolidMedium
    , fontSize = 14
    , keyboardState = Keyboard.initialState
    , images = List.Zipper.fromList [ goat ]
    , imageSelected = True
    , currentDropdown = Nothing
    , drawing = DrawLine Arrow
    , shape = DrawShape Rect
    , spotlight = DrawSpotlight RoundedRect
    , annotationState = ReadyToDraw
    , operatingSystem = MacOS
    , annotationMenu = Nothing
    , showingAnyMenu = False
    , clipboard = Nothing
    , context = Web
    }


start : StartPosition
start =
    Mouse.Position 50 50


end : EndPosition
end =
    Mouse.Position 76 88


line : Color.Color -> StrokeStyle -> Shape
line strokeColor strokeStyle =
    Shape start end strokeColor strokeStyle


aShape : Shape
aShape =
    Shape start end model.strokeColor model.strokeStyle


aTextArea : TextArea
aTextArea =
    TextArea start end model.strokeColor model.fontSize "Text" 0 (AutoExpand.initState (autoExpandConfig 0))


autoExpand : AutoExpand.State
autoExpand =
    AutoExpand.initState (autoExpandConfig 0)


testColor : Color.Color
testColor =
    Color.blue
