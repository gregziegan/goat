module Fixtures exposing (aShape, aTextArea, attributes, autoExpand, drawingInfo, end, firstFreeDrawPosition, goat, line, model, resizingInfo, secondFreeDrawPosition, start, testColor)

import Array exposing (Array)
import AutoExpand
import Annotation as Annotation exposing (EndPosition, LineType(..), Shape, ShapeType(..), StartPosition, TextArea, autoExpandConfig, shiftPosition)
import Annotation exposing (AnnotationAttributes, DrawingInfo, ResizingInfo, StrokeStyle, Vertex(..))
import EditState as EditState
import Environment exposing (OperatingSystem(MacOS), Platform(Web))
import Model exposing (Image, Model)

import Mouse exposing (Position)
import UndoList
import ZipList


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
    , editState = EditState.initialState
    , waitingForDropdownToggle = Nothing
    , fill = Nothing
    , strokeColor = Color.red
    , strokeStyle = Annotation.defaultStroke
    , fontSize = 14
    , pressedKeys = []
    , images = ZipList.fromList [ goat ]
    , imageSelected = True
    , currentDropdown = Nothing
    , drawing = Annotation.defaultDrawing
    , shape = Annotation.defaultShape
    , spotlight = Annotation.defaultSpotlight
    , operatingSystem = MacOS
    , annotationMenu = Nothing
    , showingAnyMenu = False
    , clipboard = Nothing
    , platform = Web
    }


start : StartPosition
start =
    Position 50 50


end : EndPosition
end =
    Position 76 88


firstFreeDrawPosition : Position
firstFreeDrawPosition =
    Position 65 65


secondFreeDrawPosition : Position
secondFreeDrawPosition =
    Position 80 80


line : Color.Color -> StrokeStyle -> Shape
line strokeColor strokeStyle =
    Shape start end strokeColor strokeStyle


aShape : Shape
aShape =
    Shape start end model.strokeColor model.strokeStyle


aTextArea : TextArea
aTextArea =
    TextArea start end model.strokeColor model.fontSize "Text" 0 (AutoExpand.initState (autoExpandConfig TextBoxInput 0 model.fontSize))


autoExpand : AutoExpand.State
autoExpand =
    AutoExpand.initState (autoExpandConfig TextBoxInput 0 model.fontSize)


testColor : Color.Color
testColor =
    Color.blue


drawingInfo : DrawingInfo
drawingInfo =
    DrawingInfo start end []


resizingInfo : ResizingInfo
resizingInfo =
    ResizingInfo 0 start (shiftPosition -10 -10 start) Start ( start, end ) (extractAnnotationAttributes model)


attributes : AnnotationAttributes
attributes =
    extractAnnotationAttributes model
