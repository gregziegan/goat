module Tests exposing (..)

import DrawingAnnotations
import EditingATextBox
import Utils
import MovingAnnotation
import ResizingAnnotation
import SelectedAnnotation
import Test exposing (..)
import View.DrawingArea.Annotation as Annotation
import View.DrawingArea.Definitions as Definitions


all : Test
all =
    describe "Annotation App Suite"
        [ DrawingAnnotations.all
        , SelectedAnnotation.all
        , MovingAnnotation.all
        , ResizingAnnotation.all
        , EditingATextBox.all
        , Annotation.all
        , Definitions.all
        , Utils.all
        ]
