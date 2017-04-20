module Tests exposing (..)

import DrawingAnnotations
import EditingATextBox
import Helpers
import MovingAnnotation
import ResizingAnnotation
import SelectedAnnotation
import Test exposing (..)
import View.Annotation
import View.DrawingDefinitions


all : Test
all =
    describe "Annotation App Suite"
        [ DrawingAnnotations.all
        , SelectedAnnotation.all
        , MovingAnnotation.all
        , ResizingAnnotation.all
        , EditingATextBox.all
        , View.Annotation.all
        , View.DrawingDefinitions.all
        , Helpers.all
        ]
