module Goat.Model exposing (..)

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
    { url : String
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


{-|
  Vertices are classified by their relationship to the `start` and `end`
  mouse positions that created the annotation.

  e.g: (assume a top-left to bottom-right draw)

  Start            StartPlusX
        +----------+
        |          |
        |          |
        |          |
        |          |
        +----------+
  StartPlusY       End

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


{-|

The finite state machine for annotating.

                 --------------------------------------------------------+
                 |                                                       |
                 +                                                       v

       +------------------+         +------------------+         +------------------+
       |                  |         |                  |         |                  | <--------+
       |                  |         |                  | <-------+                  |          |
+--->  |    ReadyToDraw   +-------> |    Drawing       |         |     Selected     |          |
|      |                  |         |    Annotation    |         |     Annotation   |          |
|      |                  | <-------+                  +----+    |                  | <----+   |
|      |                  |         |                  |    |  +-+                  |      |   |
|      +------------------+         +------------------+    |  | +------------------+      |   |
|                     ^                                     |  |              |            |   |
|                     |                                     |  |              |            |   |
|              +-----------------------------------------------+              |            |   |
|              |                             |              |                 |            |   |
|              v                             v              |                 v            |   |
|                                                           |                              |   |
|      +------------------+         +------------------+    |    +------------------+      |   |
|      |                  |         |                  |    |    |                  |      |   |
|      |                  |         |                  |    |    |                  |      |   |
|      |   Moving         |         |    Resizing      |    +--> |    Editing       |      |   |
|      |   Annotation     |         |    Annotation    |         |    A Textbox     |      |   |
|      |                  |         |                  |         |                  |      |   |
|      |                  |         |                  |         |                  |      |   |
|      +----+-------------+         +--------------+---+         +------------------+      |   |
|           |                                      |                         |             |   |
|           |                                      |                         |             |   |
|           |                                      |                         |             |   |
+----------------------------------------------------------------------------+             |   |
            |                                      |                                       |   |
            |                                      |                                       |   |
            |                                      +---------------------------------------+   |
            |                                                                                  |
            +----------------------------------------------------------------------------------+

-}
type AnnotationState
    = ReadyToDraw
    | DrawingAnnotation StartPosition Position
    | SelectedAnnotation Int
    | MovingAnnotation Int StartPosition ( Int, Int )
    | ResizingAnnotation ResizingData
    | EditingATextBox Int


{-|
   Annotations are viewed differently based on the kind of selection.
   1. Selected corresponds to annotations that are not in a state for resizing/moving.
     This is currently only relevant to Textboxes when they are being edited.
   2. SelectedWithVertices shows vertices on any annotation that allows for resizing/moving
   3. NotSelected shows the unadorned annotation
-}
type SelectState
    = Selected
    | SelectedWithVertices
    | NotSelected


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
    , annotationMenu : Maybe { index : Int, position : Position }
    , showingAnyMenu : Bool
    , clipboard : Maybe Annotation
    }


strokeColorOptions : List Color
strokeColorOptions =
    [ Color.rgb 255 0 0
    , Color.rgb 255 0 212
    , Color.rgb 73 0 255
    , Color.rgb 0 202 255
    , Color.rgb 16 255 0
    , Color.rgb 255 226 0
    , Color.rgb 255 129 0
    , Color.black
    , Color.white
    ]


fillOptions : List (Maybe Color)
fillOptions =
    [ Nothing
    , Just (Color.rgb 255 0 0)
    , Just (Color.rgb 255 0 212)
    , Just (Color.rgb 73 0 255)
    , Just (Color.rgb 0 202 255)
    , Just (Color.rgb 16 255 0)
    , Just (Color.rgb 255 226 0)
    , Just (Color.rgb 255 129 0)
    , Just Color.black
    , Just Color.white
    ]


strokeStyleOptions : List StrokeStyle
strokeStyleOptions =
    [ SolidThin
    , SolidMedium
    , SolidThick
    , SolidVeryThick
    , DashedThin
    , DashedMedium
    , DashedThick
    , DashedVeryThick
    ]


drawingOptions : Bool -> List Drawing
drawingOptions shiftPressed =
    if shiftPressed then
        [ DrawLine Arrow DrawingDiscreteLine
        , DrawLine StraightLine DrawingDiscreteLine
        , DrawShape Rect DrawingEqualizedShape
        , DrawShape RoundedRect DrawingEqualizedShape
        , DrawShape Ellipse DrawingEqualizedShape
        , DrawTextBox
        , DrawSpotlight Rect DrawingEqualizedShape
        , DrawSpotlight RoundedRect DrawingEqualizedShape
        , DrawSpotlight Ellipse DrawingEqualizedShape
        ]
    else
        [ DrawLine Arrow DrawingLine
        , DrawLine StraightLine DrawingLine
        , DrawShape Rect DrawingShape
        , DrawShape RoundedRect DrawingShape
        , DrawShape Ellipse DrawingShape
        , DrawTextBox
        , DrawSpotlight Rect DrawingShape
        , DrawSpotlight RoundedRect DrawingShape
        , DrawSpotlight Ellipse DrawingShape
        ]


fontSizes : List Int
fontSizes =
    [ 14
    , 16
    , 20
    , 26
    , 32
    , 40
    ]


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


{-|
  The sidebar is a hard-coded width. This offset is used to shift the incoming mouse position.
  TODO: investigate whether this can be skipped by using position: relative, or some
  other CSS rule.
-}
controlUIWidth : number
controlUIWidth =
    83
