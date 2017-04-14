module Goat.Model exposing (Flags, StartPosition, EndPosition, ShapeMode(DrawingShape, DrawingEqualizedShape), LineMode(DrawingLine, DrawingDiscreteLine), Drawing(DrawLine, DrawShape, DrawTextBox, DrawSpotlight), StrokeStyle(SolidThin, SolidMedium, SolidThick, SolidVeryThick, DashedThin, DashedMedium, DashedThick, DashedVeryThick), Shape, TextArea, Image, AttributeDropdown(Fonts, Fills, StrokeColors, Strokes), LineType(Arrow, StraightLine), ShapeType(Rect, RoundedRect, Ellipse), Vertices(Rectangular, Elliptical, Linear), Annotation(Lines, Shapes, TextBox, Spotlight), Vertex(Start, End, StartPlusX, StartPlusY), OperatingSystem(MacOS, Windows), ResizingData, AnnotationState(ReadyToDraw, DrawingAnnotation, SelectedAnnotation, MovingAnnotation, ResizingAnnotation, EditingATextBox), SelectState(Selected, SelectedWithVertices, NotSelected), Model, AnnotationAttributes, init)

import Array.Hamt as Array exposing (Array)
import AutoExpand
import Color exposing (Color)
import Keyboard.Extra as Keyboard
import List.Zipper exposing (Zipper)
import Mouse exposing (Position)
import UndoList exposing (UndoList)


type alias Flags =
    { isMac : Bool }


type alias StartPosition =
    Position


type alias EndPosition =
    Position


type ShapeMode
    = DrawingShape
    | DrawingEqualizedShape


type LineMode
    = DrawingLine
    | DrawingDiscreteLine


type Drawing
    = DrawLine LineType LineMode
    | DrawShape ShapeType ShapeMode
    | DrawTextBox
    | DrawSpotlight ShapeType ShapeMode


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


type alias Image =
    { id : String
    , url : String
    , width : Float
    , height : Float
    , originalWidth : Float
    , originalHeight : Float
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
    | Elliptical
    | Linear


type Annotation
    = Lines LineType Shape
    | Shapes ShapeType (Maybe Color) Shape
    | TextBox TextArea
    | Spotlight ShapeType Shape


{-| Vertices are classified by their relationship to the `start` and `end`
mouse positions that created the annotation.

e.g: (assume a top-left to bottom-right draw)

Start StartPlusX
+----------+
| |
| |
| |
| |
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
    }


init : Flags -> Model
init flags =
    { edits = UndoList.fresh Array.empty
    , fill = Nothing
    , strokeColor = Color.rgb 255 0 0
    , strokeStyle = SolidMedium
    , fontSize = 20
    , keyboardState = Keyboard.initialState
    , images = List.Zipper.fromList []
    , imageSelected = False
    , currentDropdown = Nothing
    , drawing = DrawLine Arrow DrawingLine
    , annotationState = ReadyToDraw
    , operatingSystem =
        if flags.isMac then
            MacOS
        else
            Windows
    , annotationMenu = Nothing
    , showingAnyMenu = False
    , clipboard = Nothing
    }
