module Fixtures exposing (..)

import Array.Hamt as Array exposing (Array)
import AutoExpand
import Color
import Color.Convert
import Fuzz exposing (Fuzzer)
import Goat.Helpers exposing (..)
import Goat.Model exposing (..)
import Goat.Update exposing (..)
import Goat.View exposing (..)
import Html
import Html.Attributes as Html
import Keyboard.Extra as Keyboard
import List.Zipper
import Mouse exposing (Position)
import Random.Pcg as Random
import Shrink
import Svg exposing (svg)
import Svg.Attributes as Attr exposing (fontSize)
import Test.Html.Selector as HtmlSelector exposing (Selector, all, attribute, class, tag, text)
import UndoList


goat : Image
goat =
    { url = "goat.jpg"
    , width = 100.0
    , height = 100.0
    , originalWidth = 200.0
    , originalHeight = 200.0
    }


svgDrawspace : List (Svg.Svg msg) -> Html.Html msg
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


start : StartPosition
start =
    Mouse.Position 50 50


end : EndPosition
end =
    Mouse.Position 76 88


line : Color.Color -> StrokeStyle -> Line
line strokeColor strokeStyle =
    Line start end strokeColor strokeStyle


position : Fuzzer Position
position =
    Fuzz.custom
        (Random.map2 Position (Random.int -100 100) (Random.int -100 100))
        (\{ x, y } -> Shrink.map Position (Shrink.int x) |> Shrink.andMap (Shrink.int y))


aLine : Line
aLine =
    Line start end model.strokeColor model.strokeStyle


aShape : Shape
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


roundedRectSelector : Shape -> List Selector
roundedRectSelector shape =
    [ attribute "rx" "15"
    , attribute "ry" "15"
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
    TextArea start end model.strokeColor model.fontSize "Text" 0 (AutoExpand.initState (config 0))


svgTextSelector : TextArea -> List Selector
svgTextSelector { start, end } =
    [ tag "text"
    , attribute "x" <| toString <| Basics.min start.y end.y
    ]


tspanSelector : TextArea -> List Selector
tspanSelector { start, end, fontSize, fill } =
    [ tag "tspan"
    , attribute "dy" <| toString <| fontSizeToLineHeight fontSize
    , attribute "x" <| toString <| Basics.min start.x end.x
    , attribute "fill" <| Color.Convert.colorToHex fill
    ]


testColor : Color.Color
testColor =
    Color.blue
