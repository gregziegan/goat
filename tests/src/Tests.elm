module Tests exposing (..)

import Update.DrawingAnnotation
import Update.EditingATextBox
import Update.MovingAnnotation
import Update.ResizingAnnotation
import Update.SelectedAnnotation
import Utils
import Test exposing (..)
import View.DrawingArea.Annotation
import View.DrawingArea.Definitions


all : Test
all =
    describe "Annotation App Suite"
        [ Update.DrawingAnnotation.all
        , Update.SelectedAnnotation.all
        , Update.MovingAnnotation.all
        , Update.ResizingAnnotation.all
        , Update.EditingATextBox.all
        , View.DrawingArea.Annotation.all
        , View.DrawingArea.Definitions.all
        , Utils.all
        ]
