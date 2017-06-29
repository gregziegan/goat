module Goat.Model exposing (Model, AttributeDropdown(..), AnnotationMenu, Image, init)

import Array.Hamt as Array exposing (Array)
import Color exposing (Color)
import Goat.Annotation exposing (Annotation, Drawing, defaultDrawing, defaultShape, defaultSpotlight, defaultStroke)
import Goat.Annotation.Shared exposing (StrokeStyle)
import Goat.EditState as EditState exposing (EditState)
import Goat.Environment exposing (OperatingSystem(..), Platform(..))
import Goat.Ports as Ports
import Keyboard.Extra as Keyboard exposing (Key)
import List.Zipper exposing (Zipper)
import Mouse exposing (Position)
import Rocket exposing ((=>))
import UndoList exposing (UndoList)


type alias Image =
    { id : String
    , url : String
    , width : Float
    , height : Float
    , originalWidth : Float
    , originalHeight : Float
    }


type AttributeDropdown
    = ShapesDropdown
    | SpotlightsDropdown
    | Fonts
    | Fills
    | StrokeColors
    | Strokes


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


init :
    Result String { os : OperatingSystem, platform : Platform }
    -> ( Model, List (Cmd msg) )
init decodeResult =
    case decodeResult of
        Ok { os, platform } ->
            initialModel os platform
                => case platform of
                    Zendesk ->
                        []

                    Web ->
                        [ Ports.listenForUpload () ]

        Err _ ->
            initialModel Windows Web
                => [ Ports.listenForUpload () ]


initialModel : OperatingSystem -> Platform -> Model
initialModel os platform =
    { edits = UndoList.fresh Array.empty
    , editState = EditState.initialState
    , clipboard = Nothing
    , drawing = defaultDrawing
    , shape = defaultShape
    , spotlight = defaultSpotlight
    , waitingForDropdownToggle = Nothing
    , fill = Nothing
    , strokeColor = Color.rgb 255 0 212
    , strokeStyle = defaultStroke
    , fontSize = 20
    , currentDropdown = Nothing
    , annotationMenu = Nothing
    , showingAnyMenu = False
    , images = List.Zipper.fromList []
    , imageSelected = False
    , pressedKeys = []
    , operatingSystem = os
    , platform = platform
    }
