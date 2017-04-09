module Tests exposing (..)

import DrawingAnnotations
import Helpers
import MovingAnnotation
import SelectedAnnotation
import Test exposing (..)
import View.Annotation


all : Test
all =
    describe "Annotation App Suite"
        [ DrawingAnnotations.all
        , SelectedAnnotation.all
        , MovingAnnotation.all
        , View.Annotation.all
        , Helpers.all
        ]
