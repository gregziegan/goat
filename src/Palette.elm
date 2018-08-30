module Palette exposing (black, blue, cyan, gray, green, orange, purple, red, white, yellow)

import Color exposing (Color)
import Palette.X11 as X11


purple : Color
purple =
    X11.magenta


red : Color
red =
    X11.red


blue : Color
blue =
    X11.blue


cyan : Color
cyan =
    X11.cyan


green : Color
green =
    X11.lime


black : Color
black =
    X11.black


white : Color
white =
    X11.white


gray : Color
gray =
    X11.gray


yellow : Color
yellow =
    X11.yellow


orange : Color
orange =
    X11.orange
