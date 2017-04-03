module Tests exposing (..)

import Annotator exposing (..)
import Array exposing (Array)
import Color
import Expect
import Fuzz exposing (Fuzzer)
import Keyboard.Extra as Keyboard
import Mouse exposing (Position)
import Random.Pcg as Random
import Shrink
import Test exposing (..)
import UndoList
import List.Zipper


goat : Image
goat =
    { url = "goat.jpg"
    , width = 100.0
    , height = 100.0
    , originalWidth = 200.0
    , originalHeight = 200.0
    }


initialImageEditState : Model
initialImageEditState =
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


line =
    Line start end Color.red SolidMedium


lineDrawing =
    Lines StraightLine line


position : Fuzzer Position
position =
    Fuzz.custom
        (Random.map2 Position (Random.int -100 100) (Random.int -100 100))
        (\{ x, y } -> Shrink.map Position (Shrink.int x) |> Shrink.andMap (Shrink.int y))


all : Test
all =
    describe "Annotation App Suite"
        [ describe "Drawing"
            [ test "finishLineDrawing should add a line annotation to the edit history" <|
                \() ->
                    initialImageEditState
                        |> finishLineDrawing start end StraightLine DrawingLine
                        |> .edits
                        |> .present
                        |> Array.get 0
                        |> Maybe.map (Expect.equal lineDrawing)
                        |> Maybe.withDefault (Expect.fail "Array missing line annotation")
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
