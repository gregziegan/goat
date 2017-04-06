module Goat.Model exposing (..)

import Array.Hamt as Array exposing (Array)
import AutoExpand
import Color exposing (Color)
import Keyboard.Extra as Keyboard
import List.Zipper exposing (Zipper)
import Mouse exposing (Position)
import Rocket exposing ((=>))
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


type Fill
    = SolidFill Color
    | EmptyFill
    | MaskFill
    | SpotlightFill


type StrokeStyle
    = SolidThin
    | SolidMedium
    | SolidThick
    | SolidVeryThick
    | DashedThin
    | DashedMedium
    | DashedThick
    | DashedVeryThick


type alias Line =
    { start : Position
    , end : Position
    , strokeColor : Color
    , strokeStyle : StrokeStyle
    }


type alias Shape =
    { start : Position
    , end : Position
    , fill : Fill
    , strokeColor : Color
    , strokeStyle : StrokeStyle
    }


type alias TextArea =
    { start : Position
    , end : Position
    , fill : Color
    , fontSize : Float
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
    = Lines LineType Line
    | Shapes ShapeType Shape
    | TextBox TextArea
    | Spotlight ShapeType Shape


type Vertex
    = Start
    | End
    | StartPlusX
    | StartPlusY


type OperatingSystem
    = MacOS
    | Windows


type AnnotationState
    = ReadyToDraw
    | DrawingAnnotation StartPosition
    | SelectedAnnotation Int Annotation
    | MovingAnnotation Int Annotation StartPosition
    | ResizingAnnotation Int Annotation StartPosition Vertex
    | EditingATextBox Int


type SelectState
    = Selected
    | SelectedWithVertices
    | NotSelected


type alias Model =
    { edits : UndoList (Array Annotation)
    , annotationState :
        AnnotationState
        -- annotation attributes
    , fill : Fill
    , strokeColor : Color
    , strokeStyle : StrokeStyle
    , fontSize :
        Float
        -- Important I/O device info
    , mouse : Mouse.Position
    , keyboardState :
        Keyboard.State
        -- Image selection fields
    , images : Maybe (Zipper Image)
    , imageSelected :
        Bool
        -- Control Interface
    , currentDropdown : Maybe AttributeDropdown
    , drawing : Drawing
    , operatingSystem : OperatingSystem
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
    ]


fillOptions : List Fill
fillOptions =
    [ EmptyFill
    , SolidFill (Color.rgb 255 0 0)
    , SolidFill (Color.rgb 255 0 212)
    , SolidFill (Color.rgb 73 0 255)
    , SolidFill (Color.rgb 0 202 255)
    , SolidFill (Color.rgb 16 255 0)
    , SolidFill (Color.rgb 255 226 0)
    , SolidFill (Color.rgb 255 129 0)
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


fontSizes : List Float
fontSizes =
    [ 10
    , 12
    , 14
    , 16
    , 18
    , 20
    ]


init : Flags -> ( Model, List (Cmd msg) )
init flags =
    { edits = UndoList.fresh Array.empty
    , fill = EmptyFill
    , strokeColor = Color.rgb 255 0 0
    , strokeStyle = SolidMedium
    , fontSize = 14
    , mouse = Mouse.Position 0 0
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
    }
        => []


controlUIWidth : number
controlUIWidth =
    83
