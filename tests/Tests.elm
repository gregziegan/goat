module Tests exposing (..)

import Annotator exposing (..)
import Array.Hamt as Array exposing (Array)
import AutoExpand
import Color
import Color.Convert
import Expect
import Fuzz exposing (Fuzzer)
import Html
import Html.Attributes as Html
import Keyboard.Extra as Keyboard
import List.Zipper
import Mouse exposing (Position)
import Random.Pcg as Random
import Shrink
import Svg exposing (svg)
import Svg.Attributes as Attr exposing (fontSize)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as HtmlSelector exposing (Selector, all, attribute, tag, text, class)
import UndoList


goat : Image
goat =
    { url = "goat.jpg"
    , width = 100.0
    , height = 100.0
    , originalWidth = 200.0
    , originalHeight = 200.0
    }


svgDrawspace =
    svg
        [ Attr.id "drawing"
        , Attr.class "drawing"
        , Attr.width <| toString <| round goat.width
        , Attr.height <| toString <| round goat.height
        , Html.attribute "xmlns" "http://www.w3.org/2000/svg"
        ]


model : Model
model =
    { edits = UndoList.fresh Array.empty
    , fill = EmptyFill
    , strokeColor = Color.red
    , strokeStyle = SolidMedium
    , fontSize = 14
    , mouse = Mouse.Position 0 0
    , keyboardState = Keyboard.initialState
    , images = List.Zipper.fromList [ goat ]
    , imageSelected = True
    , currentDropdown = Nothing
    , drawing = DrawLine Arrow DrawingLine
    , annotationState = ReadyToDraw
    , operatingSystem = MacOS
    }


start =
    Mouse.Position 50 50


end =
    Mouse.Position 76 88


line strokeColor strokeStyle =
    Line start end strokeColor strokeStyle


getFirstAnnotation model =
    model
        |> .edits
        |> .present
        |> Array.get 0


position : Fuzzer Position
position =
    Fuzz.custom
        (Random.map2 Position (Random.int -100 100) (Random.int -100 100))
        (\{ x, y } -> Shrink.map Position (Shrink.int x) |> Shrink.andMap (Shrink.int y))


aLine =
    Line start end model.strokeColor model.strokeStyle


aShape =
    Shape start end model.fill model.strokeColor model.strokeStyle


lineSelector : Line -> List Selector
lineSelector line =
    [ attribute "fill" "none"
    , attribute "d" (linePath line.start line.end)
    ]
        ++ (uncurry strokeSelectors (toLineStyle line.strokeStyle)) line.strokeColor


strokeSelectors : String -> String -> Color.Color -> List Selector
strokeSelectors strokeWidth dashArray strokeColor =
    [ attribute "stroke" <| Color.Convert.colorToHex strokeColor
    , attribute "stroke-width" strokeWidth
    , attribute "stroke-dasharray" dashArray
    ]


ellipseSelector : Shape -> List Selector
ellipseSelector shape =
    [ attribute "rx" <| toString <| abs end.x - start.x
    , attribute "ry" <| toString <| abs end.y - start.y
    , attribute "cx" <| toString start.x
    , attribute "cy" <| toString start.y
    , attribute "filter" "url(#dropShadow)"
    ]
        ++ (uncurry strokeSelectors (toLineStyle shape.strokeStyle)) shape.strokeColor


roundedRectSelector shape =
    [ attribute "rx" "15"
    , attribute "rx" "15"
    ]
        ++ rectSelector shape


fillSelectors : Fill -> List Selector
fillSelectors fill =
    let
        ( fillColor, isVisible ) =
            fillStyle fill
    in
        if isVisible then
            [ attribute "fill" fillColor ]
        else
            [ attribute "fill" fillColor, attribute "fill-opacity" "0" ]


rectSelector : Shape -> List Selector
rectSelector shape =
    attribute "filter" "url(#dropShadow)"
        :: fillSelectors shape.fill
        ++ (uncurry strokeSelectors (toLineStyle shape.strokeStyle)) shape.strokeColor


aTextArea : TextArea
aTextArea =
    TextArea start end model.strokeColor model.fontSize "Text" 0 (AutoExpand.initState (Annotator.config 0 model.fontSize))


svgTextSelector : TextArea -> List Selector
svgTextSelector { start, end } =
    [ tag "text"
    , attribute "x" <| toString <| Basics.min start.y end.y
    ]


tspanSelector : TextArea -> List Selector
tspanSelector { start, end, fontSize, fill } =
    [ tag "tspan"
    , attribute "dy" <| toString <| fontSize
    , attribute "x" <| toString <| Basics.min start.x end.x
    , attribute "fill" <| Color.Convert.colorToHex fill
    ]


all : Test
all =
    describe "Annotation App Suite"
        [ describe "Drawing"
            [ test "finishLineDrawing should add a line annotation to the edit history" <|
                \() ->
                    model
                        |> finishLineDrawing start end StraightLine DrawingLine
                        |> getFirstAnnotation
                        |> Maybe.map (Expect.equal (Lines StraightLine <| Line start end model.strokeColor model.strokeStyle))
                        |> Maybe.withDefault (Expect.fail "Array missing line annotation")
            , test "finishLineDrawing should add an arrow annotation to the edit history" <|
                \() ->
                    model
                        |> finishLineDrawing start end Arrow DrawingLine
                        |> getFirstAnnotation
                        |> Maybe.map (Expect.equal (Lines Arrow <| Line start end model.strokeColor model.strokeStyle))
                        |> Maybe.withDefault (Expect.fail "Array missing arrow annotation")
            , test "finishShapeDrawing should add a shape annotation to the edit history" <|
                \() ->
                    model
                        |> finishShapeDrawing start end Rect DrawingShape
                        |> getFirstAnnotation
                        |> Maybe.map (Expect.equal (Shapes Rect <| Shape start end model.fill model.strokeColor model.strokeStyle))
                        |> Maybe.withDefault (Expect.fail "Array missing rect annotation")
            , test "finishShapeDrawing should add a spotlight annotation with a spotlight fill to the edit history" <|
                -- should spotlights be refactored? this is some custom logic to get around modeling
                \() ->
                    model
                        |> finishSpotlightDrawing start end Rect DrawingShape
                        |> getFirstAnnotation
                        |> Maybe.map (Expect.equal (Spotlight Rect <| Shape start end SpotlightFill model.strokeColor model.strokeStyle))
                        |> Maybe.withDefault (Expect.fail "Array missing spotlight rect annotation")
            ]
        , describe "annotations"
            [ test "A straight line has the appropriate view attributes" <|
                \() ->
                    aLine
                        |> Lines StraightLine
                        |> viewAnnotation ReadyToDraw 0
                        |> svgDrawspace
                        |> Query.fromHtml
                        |> Query.find [ tag "path" ]
                        |> Query.has (lineSelector aLine)
            , test "An arrow has the appropriate view attributes" <|
                \() ->
                    aLine
                        |> Lines Arrow
                        |> viewAnnotation ReadyToDraw 0
                        |> svgDrawspace
                        |> Query.fromHtml
                        |> Query.find [ tag "path" ]
                        |> Query.has (lineSelector aLine)
            , test "A rectangle has the appropriate view attributes" <|
                \() ->
                    aShape
                        |> Shapes Rect
                        |> viewAnnotation ReadyToDraw 0
                        |> svgDrawspace
                        |> Query.fromHtml
                        |> Query.find [ tag "rect" ]
                        |> Query.has (rectSelector aShape)
            , test "A rounded rectangle has the appropriate view attributes" <|
                \() ->
                    aShape
                        |> Shapes RoundedRect
                        |> viewAnnotation ReadyToDraw 0
                        |> svgDrawspace
                        |> Query.fromHtml
                        |> Query.find [ tag "rect" ]
                        |> Query.has (roundedRectSelector aShape)
            , test "An ellipse has the appropriate view attributes" <|
                \() ->
                    aShape
                        |> Shapes Ellipse
                        |> viewAnnotation ReadyToDraw 0
                        |> svgDrawspace
                        |> Query.fromHtml
                        |> Query.find [ tag "ellipse" ]
                        |> Query.has (ellipseSelector aShape)
            , test "A textbox's unselected svg text has the appropriate view attributes" <|
                \() ->
                    aTextArea
                        |> TextBox
                        |> viewAnnotation ReadyToDraw 0
                        |> svgDrawspace
                        |> Query.fromHtml
                        |> Query.find [ tag "text" ]
                        |> Query.has (svgTextSelector aTextArea)
            , test "A textbox's unselected svg tspans have the appropriate view attributes" <|
                \() ->
                    aTextArea
                        |> TextBox
                        |> viewAnnotation ReadyToDraw 0
                        |> svgDrawspace
                        |> Query.fromHtml
                        |> Query.findAll [ tag "tspan" ]
                        |> Query.each
                            (Expect.all
                                [ Query.has (tspanSelector aTextArea) ]
                            )
            , test "A spotlight has the appropriate view attributes" <|
                \() ->
                    aShape
                        |> Spotlight Rect
                        |> viewAnnotation ReadyToDraw 0
                        |> svgDrawspace
                        |> Query.fromHtml
                        |> Query.find [ tag "rect" ]
                        |> Query.has (rectSelector aShape)
            ]
        , describe "Utils"
            [ fuzz2 position position "mouse step function works properly" <|
                \pos1 pos2 ->
                    stepMouse pos1 pos2
                        |> arrowAngle pos1
                        |> round
                        |> flip (%) (round (pi / 4))
                        |> Expect.equal 0
            ]
        ]
