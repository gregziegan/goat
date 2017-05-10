module Tests exposing (..)

import Update


-- import Update.Controls
-- import Update.DrawingAnnotation
-- import Update.EditingATextBox
-- import Update.MovingAnnotation
-- import Update.ResizingAnnotation
-- import Annotation

import Utils
import Test exposing (..)


-- import View.DrawingArea.Annotation

import View.DrawingArea.Definitions


all : Test
all =
    describe "Annotation App Suite"
        -- [ Update.Controls.all
        [ -- , Update.DrawingAnnotation.all
          -- , Annotation.all
          -- , Update.MovingAnnotation.all
          -- , Update.ResizingAnnotation.all
          -- , Update.EditingATextBox.all
          -- , View.DrawingArea.Annotation.all
          View.DrawingArea.Definitions.all
        , Update.all
        , Utils.all
        ]
