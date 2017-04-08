module Goat.Model exposing (..)

import Array.Hamt as Array exposing (Array)
import AutoExpand
import Color exposing (Color)
import Color.Convert
import Http
import Json.Decode as Json exposing (Decoder, oneOf)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import Json.Encode exposing (Value, bool, float, int, list, null, object, string)
import Keyboard.Extra as Keyboard
import List.Zipper exposing (Zipper)
import Mouse exposing (Position)
import Rocket exposing ((=>))
import UndoList exposing (UndoList)
import Native.BugReport


type alias Flags =
    { isMac : Bool
    , debugModel : Maybe Value
    }


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
    { annotationState : AnnotationState
    , bugReportText : Maybe String
    , currentDropdown : Maybe AttributeDropdown
    , drawing : Drawing
    , edits : UndoList (Array Annotation)
    , fill : Fill
    , fontSize : Float
    , imageSelected : Bool
    , images : Maybe (Zipper Image)
    , keyboardState : Keyboard.State
    , mouse : Mouse.Position
    , operatingSystem : OperatingSystem
    , strokeColor : Color
    , strokeStyle : StrokeStyle
    }


type alias BugReportModel =
    { messages : List Msg
    , bugReportState : Model
    , app : Model
    }


type Msg
    = StartDrawing StartPosition
    | ContinueDrawing StartPosition
    | FinishDrawing StartPosition EndPosition
      -- TextArea Updates
    | StartEditingText Int TextArea
    | SwitchToEditingText Int
    | FinishEditingText Int
    | SetText Int TextArea String
    | AutoExpandInput Int { textValue : String, state : AutoExpand.State }
      -- Annotation Attribute updates
    | SelectFill Fill
    | SelectStrokeColor Color
    | SelectStrokeStyle StrokeStyle
    | SelectFontSize Float
      -- Control UI updates
    | ToggleDropdown AttributeDropdown
    | ChangeDrawing Drawing
    | CloseDropdown
      -- Selection Updates
    | ResetToReadyToDraw
    | SelectAnnotation Int Annotation StartPosition
      -- Move updates
    | StartMovingAnnotation Int Annotation StartPosition
    | MoveAnnotation Int Annotation StartPosition EndPosition
    | FinishMovingAnnotation Int Annotation StartPosition EndPosition
      -- Resize updates
    | StartResizingAnnotation Int Annotation Vertex StartPosition
    | ResizeAnnotation Int Annotation Vertex StartPosition EndPosition
    | FinishResizingAnnotation Int Annotation Vertex StartPosition EndPosition
      -- History updates
    | Undo
    | Redo
    | Save
      -- Image Selection updates
    | SelectImage Image
    | SetImages (List Image)
    | Cancel
      -- Keyboard updates
    | KeyboardMsg Keyboard.Msg
      -- Bug reports
    | BugReports BugReportMsg
      -- Fun
    | ShowMeTheGoats


type BugReportMsg
    = ShowBugReportInput
    | BugReportInput String
    | CancelBugReport
    | SubmitBugReport
    | SubmittedBugReport (Result Http.Error String)


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
    , SolidFill Color.black
    , SolidFill Color.white
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
    [ 14
    , 16
    , 20
    , 26
    , 32
    , 40
    ]


initialModel : Flags -> Model
initialModel flags =
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
    , bugReportText = Nothing
    }


initialDebugModel : Flags -> Model
initialDebugModel flags =
    case Debug.log "debugModel" flags.debugModel of
        Just modelValue ->
            modelDecoder modelValue

        -- |> Result.withDefault (initialModel flags)
        Nothing ->
            initialModel flags


modelDecoder : Value -> Model
modelDecoder value =
    jsToElm value


init : Flags -> BugReportModel
init flags =
    { messages = []
    , bugReportState = initialDebugModel flags
    , app = initialDebugModel flags
    }


controlUIWidth : number
controlUIWidth =
    83



-- posToValue : Position -> Value
-- posToValue { x, y } =
--     object [ "x" => int x, "y" => int y ]
--
--
-- encodeLineAttrs line =
--     [ "start" => posToValue line.start
--     , "end" => posToValue line.end
--     , "strokeColor" => string (Color.Convert.colorToHex line.strokeColor)
--     , "strokeStyle" => string (toString line.strokeStyle)
--     ]
--
--
-- lineToValue : Line -> Value
-- lineToValue line =
--     object <| encodeLineAttrs line
--
--
-- shapeToValue : Shape -> Value
-- shapeToValue shape =
--     object (encodeLineAttrs shape ++ [ "fill" => string (fillToString shape.fill) ])
--
--
-- imageToValue : Image -> Value
-- imageToValue img =
--     object
--         [ "url" => string img.url
--         , "width" => float img.width
--         , "height" => float img.height
--         , "originalWidth" => float img.originalWidth
--         , "originalHeight" => float img.originalHeight
--         ]
--
--
-- textAreaToValue : TextArea -> Value
-- textAreaToValue { start, end, fill, fontSize, text, angle, autoexpand } =
--     object
--         [ "start" => posToValue start
--         , "end" => posToValue end
--         , "fill" => string (Color.Convert.colorToHex fill)
--         , "fontSize" => float fontSize
--         , "text" => string text
--         , "angle" => float angle
--         , "autoexpand" => AutoExpand.encode autoexpand
--         ]
--
--
-- annotationToValue : Annotation -> Value
-- annotationToValue annotation =
--     case annotation of
--         Lines lineType line ->
--             object [ "tag" => string "Lines", "lineType" => string (toString lineType), "line" => lineToValue line ]
--
--         Shapes shapeType shape ->
--             object [ "tag" => string "Shapes", "shapeType" => string (toString shapeType), "shape" => shapeToValue shape ]
--
--         TextBox textArea ->
--             object [ "tag" => string "TextBox", "textArea" => textAreaToValue textArea ]
--
--         Spotlight shapeType shape ->
--             object [ "tag" => string "Spotlight", "shapeType" => string (toString shapeType), "shape" => shapeToValue shape ]
--
-- modelEncoder : Model -> Value
-- modelEncoder model =
--     object
--         [ "edits" => list (Array.toList <| Array.map annotationToValue model.edits.present)
--         , "fill" => string (fillToString model.fill)
--         , "strokeColor" => string (Color.Convert.colorToHex model.strokeColor)
--         , "strokeStyle" => string (toString model.strokeStyle)
--         , "fontSize" => float model.fontSize
--         , "mouse" => object [ "x" => int model.mouse.x, "y" => int model.mouse.y ]
--         , "images"
--             => case model.images of
--                 Just images ->
--                     list (List.map imageToValue (List.Zipper.toList images))
--
--                 Nothing ->
--                     null
--         , "imageSelected" => bool model.imageSelected
--         , "currentDropdown"
--             => case model.currentDropdown of
--                 Just attributeOption ->
--                     string (toString attributeOption)
--
--                 Nothing ->
--                     null
--         , "drawing" => string (toString model.drawing)
--         , "operatingSystem" => string (toString model.operatingSystem)
--         ]
--
--
-- fillToString : Fill -> String
-- fillToString fill =
--     case fill of
--         SolidFill color ->
--             Color.Convert.colorToHex color
--
--         SpotlightFill ->
--             "SpotlightFill"
--
--         EmptyFill ->
--             "EmptyFill"
--
--         _ ->
--             "EmptyFill"
--
--
-- fillDecoder : Decoder Fill
-- fillDecoder =
--     Json.string
--         |> Json.map
--             (\hexString ->
--                 case hexString of
--                     "0" ->
--                         EmptyFill
--
--                     _ ->
--                         Color.Convert.hexToColor hexString
--                             |> Result.withDefault Color.black
--                             |> SolidFill
--             )
--
--
-- colorDecoder : Decoder Color
-- colorDecoder =
--     Json.string
--         |> Json.map (Result.withDefault Color.black << Color.Convert.hexToColor)
--
--
-- strokeStyleDecoder : String -> Decoder StrokeStyle
-- strokeStyleDecoder tag =
--     case tag of
--         "SolidThin" ->
--             Json.succeed SolidThin
--
--         "SolidMedium" ->
--             Json.succeed SolidMedium
--
--         "SolidThick" ->
--             Json.succeed SolidThick
--
--         "SolidVeryThick" ->
--             Json.succeed SolidVeryThick
--
--         "DashedThin" ->
--             Json.succeed DashedThin
--
--         "DashedMedium" ->
--             Json.succeed DashedMedium
--
--         "DashedThick" ->
--             Json.succeed DashedThick
--
--         "DashedVeryThick" ->
--             Json.succeed DashedVeryThick
--
--         _ ->
--             Json.fail (tag ++ " is not a recognized tag for StrokeStyle")
--
--
-- lineTypeDecoder : String -> Decoder LineType
-- lineTypeDecoder tag =
--     case tag of
--         "Arrow" ->
--             Json.succeed Arrow
--
--         "StraightLine" ->
--             Json.succeed StraightLine
--
--         _ ->
--             Json.fail (tag ++ " is not a recognized tag for LineType")
--
--
-- shapeTypeDecoder : String -> Decoder ShapeType
-- shapeTypeDecoder tag =
--     case tag of
--         "Rect" ->
--             Json.succeed Rect
--
--         "RoundedRect" ->
--             Json.succeed RoundedRect
--
--         "Ellipse" ->
--             Json.succeed Ellipse
--
--         _ ->
--             Json.fail (tag ++ " is not a recognized tag for ShapeType")
--
--
-- decodeShape : Decoder Shape
-- decodeShape =
--     decode Shape
--         |> required "start" mouseDecoder
--         |> required "end" mouseDecoder
--         |> required "fill" fillDecoder
--         |> required "strokeColor" colorDecoder
--         |> required "strokeStyle" (Json.string |> Json.andThen strokeStyleDecoder)
--
--
-- decodeLine : Decoder Line
-- decodeLine =
--     decode Line
--         |> required "start" mouseDecoder
--         |> required "end" mouseDecoder
--         |> required "strokeColor" colorDecoder
--         |> required "strokeStyle" (Json.string |> Json.andThen strokeStyleDecoder)
--
--
-- textAreaDecoder : Decoder TextArea
-- textAreaDecoder =
--     decode TextArea
--         |> required "start" mouseDecoder
--         |> required "end" mouseDecoder
--         |> required "fill" colorDecoder
--         |> required "fontSize" Json.float
--         |> required "text" Json.string
--         |> required "angle" Json.float
--         |> required "autoexpand" (Json.int |> Json.andThen AutoExpand.decode)
--
--
-- annotationInfo : String -> Decoder Annotation
-- annotationInfo tag =
--     case tag of
--         "Lines" ->
--             Json.map2 Lines
--                 (Json.string |> Json.andThen lineTypeDecoder)
--                 decodeLine
--
--         "Shapes" ->
--             Json.map2 Shapes
--                 (Json.string |> Json.andThen shapeTypeDecoder)
--                 decodeShape
--
--         "TextBox" ->
--             Json.map TextBox
--                 textAreaDecoder
--
--         "Spotlight" ->
--             Json.map2 Spotlight
--                 (Json.string |> Json.andThen shapeTypeDecoder)
--                 decodeShape
--
--         _ ->
--             Json.fail (tag ++ " is not a recognized tag for Annotation Types")
--
--
-- annotationDecoder : Decoder Annotation
-- annotationDecoder =
--     (Json.field "tag" Json.string)
--         |> Json.andThen annotationInfo
--
--
-- editsDecoder : Decoder (UndoList (Array Annotation))
-- editsDecoder =
--     Json.list annotationDecoder
--         |> Json.andThen decodeList
--
--
-- decodeList : List Annotation -> Decoder (UndoList (Array Annotation))
-- decodeList xs =
--     Json.succeed <| UndoList.fresh (Array.fromList xs)
--
--
-- osDecoder : String -> Decoder OperatingSystem
-- osDecoder tag =
--     case tag of
--         "MacOS" ->
--             Json.succeed MacOS
--
--         "Windows" ->
--             Json.succeed Windows
--
--         _ ->
--             Json.fail (tag ++ " is not a valid OperatingSystem")
--
--
-- decodeImage : Decoder Image
-- decodeImage =
--     decode Image
--         |> required "url" Json.string
--         |> required "width" Json.float
--         |> required "height" Json.float
--         |> required "originalWidth" Json.float
--         |> required "originalHeight" Json.float
--
--
-- imagesDecoder : Decoder (Maybe (Zipper Image))
-- imagesDecoder =
--     Json.list decodeImage
--         |> Json.map (List.Zipper.fromList)
--
--
-- modelDecoder : Decoder Model
-- modelDecoder =
--     decode Model
--         |> hardcoded ReadyToDraw
--         |> hardcoded Nothing
--         |> hardcoded Nothing
--         |> hardcoded (DrawLine Arrow DrawingLine)
--         |> required "edits" editsDecoder
--         |> required "fill" fillDecoder
--         |> required "fontSize" Json.float
--         |> required "imageSelected" Json.bool
--         |> required "images" imagesDecoder
--         |> hardcoded Keyboard.initialState
--         |> required "mouse" mouseDecoder
--         |> required "operatingSystem" (Json.string |> Json.andThen osDecoder)
--         |> required "strokeColor" colorDecoder
--         |> required "strokeStyle" (Json.string |> Json.andThen strokeStyleDecoder)
--
--
-- mouseDecoder : Decoder Position
-- mouseDecoder =
--     decode Position
--         |> required "x" (Json.int)
--         |> required "y" (Json.int)


jsToElm : Json.Encode.Value -> a
jsToElm =
    Native.BugReport.unsafeCoerce


elmToJs : a -> Json.Encode.Value
elmToJs =
    Native.BugReport.unsafeCoerce
