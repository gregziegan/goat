module Tests exposing (..)

import Annotator exposing (..)
import Array exposing (Array)
import Color
import Expect
import Fuzz exposing (Fuzzer)
import Keyboard.Extra as Keyboard
import List.Zipper
import Mouse exposing (Position)
import Random.Pcg as Random
import Shrink
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as HtmlSelector exposing (Selector, text, tag, attribute, all)
import UndoList
import Svg exposing (svg)
import Svg.Attributes as Attr
import Html
import Html.Attributes as Html


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


lineSelector : Line -> Selector
lineSelector line =
    HtmlSelector.all
        [ attribute "fill" "none"
        , attribute "d" (linePath line.start line.end)
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
                -- should spotlights be refactored, this is some custom logic to get around modeling?
                \() ->
                    model
                        |> finishShapeDrawing start end SpotlightRect DrawingShape
                        |> getFirstAnnotation
                        |> Maybe.map (Expect.equal (Shapes SpotlightRect <| Shape start end SpotlightFill model.strokeColor model.strokeStyle))
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
                        |> Query.has [ lineSelector aLine ]
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
