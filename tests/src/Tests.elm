module Tests exposing (..)



import Utils
import Test exposing (..)
import DrawingArea.Annotation
import DrawingArea.Definitions


all : Test
all =
    describe "Annotation App Suite"
        [ Update.Controls.all
        , DrawingArea.Annotation.all
        , DrawingArea.Definitions.all
        , Update.all
        , Utils.all
        ]
