module Goat.Model exposing (..)

import Array.Hamt as Array exposing (Array)
import AutoExpand
import Color exposing (Color)
import Keyboard.Extra as Keyboard
import List.Zipper exposing (Zipper)
import Mouse exposing (Position)
import Rocket exposing ((=>))
import UndoList exposing (UndoList)
import Goat.Flags exposing (Flags, Image)
import Goat.Ports as Ports


type Context
    = Zendesk
    | Web


type alias StartPosition =
    Position


type alias EndPosition =
    Position


type Drawing
    = DrawLine LineType
    | DrawShape ShapeType
    | DrawTextBox
    | DrawSpotlight ShapeType
    | DrawBlur


type StrokeStyle
    = SolidThin
    | SolidMedium
    | SolidThick
    | SolidVeryThick
    | DashedThin
    | DashedMedium
    | DashedThick
    | DashedVeryThick


type alias Shape =
    { start : Position
    , end : Position
    , strokeColor : Color
    , strokeStyle : StrokeStyle
    }


type alias TextArea =
    { start : Position
    , end : Position
    , fill : Color
    , fontSize : Int
    , text : String
    , angle : Float
    , autoexpand : AutoExpand.State
    }


type AttributeDropdown
    = Fonts
    | Fills
    | StrokeColors
    | Strokes


type LineType
    = Arrow
    | StraightLine


type ShapeType
    = Rect
    | RoundedRect
    | Ellipse


type Vertices
    = Rectangular
    | Linear


type Annotation
    = Lines LineType Shape
    | Shapes ShapeType (Maybe Color) Shape
    | TextBox TextArea
    | Spotlight ShapeType Shape
    | Blur StartPosition EndPosition


{-| Vertices are classified by their relationship to the `start` and `end`
mouse positions that created the annotation.

e.g: (assume a top-left to bottom-right draw)

Start StartPlusX
+----------+
|**********|
|**********|
|**********|
|**********|
|**********|
+----------+
StartPlusY End

-}
type Vertex
    = Start
    | End
    | StartPlusX
    | StartPlusY


type OperatingSystem
    = MacOS
    | Windows


type ResizeDirection
    = NWSE
    | NESW
    | Move


type alias ResizingData =
    { index : Int
    , start : Position
    , curPos : Position
    , vertex : Vertex
    , originalCoords : ( StartPosition, EndPosition )
    }


type alias AnnotationAttributes =
    { strokeColor : Color
    , fill : Maybe Color
    , strokeStyle : StrokeStyle
    , fontSize : Int
    }


{-| The finite state machine for annotating.
See <https://github.com/thebritican/goat/wiki/The-Annotation-Editor's-Finite-State-Machine>
-}
type AnnotationState
    = ReadyToDraw
    | DrawingAnnotation StartPosition Position
    | SelectedAnnotation Int AnnotationAttributes
    | MovingAnnotation Int StartPosition ( Int, Int ) AnnotationAttributes
    | ResizingAnnotation ResizingData AnnotationAttributes
    | EditingATextBox Int AnnotationAttributes


{-| Annotations are viewed differently based on the kind of selection.

1.  Selected corresponds to annotations that are not in a state for resizing/moving.
    This is currently only relevant to Textboxes when they are being edited.
2.  SelectedWithVertices shows vertices on any annotation that allows for resizing/moving
3.  NotSelected shows the unadorned annotation

-}
type SelectState
    = Selected
    | SelectedWithVertices
    | NotSelected


type alias AnnotationMenu =
    { index : Maybe Int
    , position : Position
    }


type alias Model =
    { edits : UndoList (Array Annotation)
    , annotationState : AnnotationState
    , fill : Maybe Color
    , strokeColor : Color
    , strokeStyle : StrokeStyle
    , fontSize : Int
    , keyboardState : Keyboard.State
    , images : Maybe (Zipper Image)
    , imageSelected : Bool
    , currentDropdown : Maybe AttributeDropdown
    , drawing : Drawing
    , operatingSystem : OperatingSystem
    , annotationMenu : Maybe AnnotationMenu
    , showingAnyMenu : Bool
    , clipboard : Maybe Annotation
    , context : Context
    }


init : Flags -> ( Model, List (Cmd msg) )
init { isMac, inZendesk } =
    { edits = UndoList.fresh Array.empty
    , fill = Nothing
    , strokeColor = Color.rgb 255 0 0
    , strokeStyle = SolidMedium
    , fontSize = 20
    , keyboardState = Keyboard.initialState
    , images = List.Zipper.fromList []
    , imageSelected = False
    , currentDropdown = Nothing
    , drawing = DrawLine Arrow
    , annotationState = ReadyToDraw
    , operatingSystem =
        if isMac then
            MacOS
        else
            Windows
    , annotationMenu = Nothing
    , showingAnyMenu = False
    , clipboard = Nothing
    , context =
        if inZendesk then
            Zendesk
        else
            Web
    }
        => if inZendesk then
            []
           else
            [ Ports.listenForUpload () ]
