module Goat.Model exposing (..)

import Array.Hamt as Array exposing (Array)
import Color exposing (Color)
import Goat.AnnotationAttributes as AnnotationAttributes exposing (Annotation, LineType(..), ShapeType(..), StrokeStyle, defaultStroke)
import Goat.EditState as EditState exposing (EditState)
import Goat.Flags exposing (Flags, Image)
import Goat.Ports as Ports
import Keyboard.Extra as Keyboard exposing (Key)
import List.Zipper exposing (Zipper)
import Mouse exposing (Position)
import Rocket exposing ((=>))
import UndoList exposing (UndoList)


type Platform
    = Zendesk
    | Web


type alias StartPosition =
    Position


type alias EndPosition =
    Position


type AttributeDropdown
    = ShapesDropdown
    | SpotlightsDropdown
    | Fonts
    | Fills
    | StrokeColors
    | Strokes


type Vertices
    = Rectangular
    | Linear


type OperatingSystem
    = MacOS
    | Windows


type ResizeDirection
    = NWSE
    | NESW
    | Move


type Drawing
    = DrawLine LineType
    | DrawFreeHand
    | DrawShape ShapeType
    | DrawTextBox
    | DrawSpotlight ShapeType
    | DrawPixelate


type alias AnnotationMenu =
    { index : Maybe Int
    , position : Position
    }


type alias Model =
    { -- Annotation Editing State
      edits : UndoList (Array Annotation)
    , editState : EditState
    , clipboard : Maybe Annotation

    -- Control UI State
    , drawing : Drawing
    , shape : Drawing
    , spotlight : Drawing
    , waitingForDropdownToggle : Maybe AttributeDropdown
    , fill : Maybe Color
    , strokeColor : Color
    , strokeStyle : StrokeStyle
    , fontSize : Int
    , currentDropdown : Maybe AttributeDropdown

    -- Image Annotator Modals
    , annotationMenu : Maybe AnnotationMenu
    , showingAnyMenu : Bool

    -- Image Selection State
    , images : Maybe (Zipper Image)
    , imageSelected : Bool

    -- Keys pressed
    , pressedKeys : List Key

    -- System/Environment State
    , operatingSystem : OperatingSystem
    , platform : Platform
    }


shapes : List Drawing
shapes =
    [ DrawShape RoundedRect
    , DrawShape Rect
    , DrawShape Ellipse
    , DrawLine StraightLine
    ]


spotlights : List Drawing
spotlights =
    [ DrawSpotlight RoundedRect
    , DrawSpotlight Rect
    , DrawSpotlight Ellipse
    ]


init : Flags -> ( Model, List (Cmd msg) )
init { isMac, inZendesk } =
    { edits = UndoList.fresh Array.empty
    , editState = EditState.initialState
    , clipboard = Nothing
    , drawing = DrawLine Arrow
    , shape = DrawShape RoundedRect
    , spotlight = DrawSpotlight RoundedRect
    , waitingForDropdownToggle = Nothing
    , fill = Nothing
    , strokeColor = Color.rgb 255 0 212
    , strokeStyle = AnnotationAttributes.defaultStroke
    , fontSize = 20
    , currentDropdown = Nothing
    , annotationMenu = Nothing
    , showingAnyMenu = False
    , images = List.Zipper.fromList []
    , imageSelected = False
    , pressedKeys = []
    , operatingSystem =
        if isMac then
            MacOS
        else
            Windows
    , platform =
        if inZendesk then
            Zendesk
        else
            Web
    }
        => if inZendesk then
            []
           else
            [ Ports.listenForUpload () ]
