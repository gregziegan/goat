module Tests exposing (..)

import Update
import Update.Controls
import Utils
import Test exposing (..)
import View.DrawingArea.Annotation
import View.DrawingArea.Definitions


all : Test
all =
    describe "Annotation App Suite"
        [ Update.Controls.all

        -- , Annotation.all
        , View.DrawingArea.Annotation.all
        , View.DrawingArea.Definitions.all
        , Update.all
        , Utils.all
        ]
