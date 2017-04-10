module Fixtures exposing (..)

import Array.Hamt as Array exposing (Array)
import AutoExpand
import Color
import Goat.Model exposing (..)
import Goat.Update exposing (..)
import Keyboard.Extra as Keyboard
import List.Zipper
import Mouse exposing (Position)
import UndoList


goat : Image
goat =
    { url = "goat.jpg"
    , width = 100.0
    , height = 100.0
    , originalWidth = 200.0
    , originalHeight = 200.0
    }


model : Model
model =
    { edits = UndoList.fresh Array.empty
    , fill = EmptyFill
    , strokeColor = Color.red
    , strokeStyle = SolidMedium
    , fontSize = 14
    , keyboardState = Keyboard.initialState
    , images = List.Zipper.fromList [ goat ]
    , imageSelected = True
    , currentDropdown = Nothing
    , drawing = DrawLine Arrow DrawingLine
    , annotationState = ReadyToDraw
    , operatingSystem = MacOS
    , annotationMenu = Nothing
    , showingAnyMenu = False
    , clipboard = Nothing
    }


start : StartPosition
start =
    Mouse.Position 50 50


end : EndPosition
end =
    Mouse.Position 76 88


line : Color.Color -> StrokeStyle -> Line
line strokeColor strokeStyle =
    Line start end strokeColor strokeStyle


aLine : Line
aLine =
    Line start end model.strokeColor model.strokeStyle


aShape : Shape
aShape =
    Shape start end model.fill model.strokeColor model.strokeStyle


aTextArea : TextArea
aTextArea =
    TextArea start end model.strokeColor model.fontSize "Text" 0 (AutoExpand.initState (config 0))


autoExpand : AutoExpand.State
autoExpand =
    AutoExpand.initState (config 0)


testColor : Color.Color
testColor =
    Color.blue
