module Tests exposing (..)

import Array.Hamt as Array exposing (Array)
import DrawingAnnotations
import Expect
import Fixtures exposing (..)
import Goat.Helpers exposing (..)
import Goat.Model exposing (..)
import Goat.Update exposing (..)
import Goat.View exposing (..)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as HtmlSelector exposing (Selector, all, attribute, class, tag, text)
import TestUtil exposing (getFirstAnnotation)


all : Test
all =
    describe "Annotation App Suite"
        [ DrawingAnnotations.all
        , describe "updateAnySelectedAnnotations"
            [ test "updates a Selected annotation" <|
                \() ->
                    model
                        |> addAnnotation (Lines Arrow aLine)
                        |> selectAnnotation 0
                        |> updateAnySelectedAnnotations (updateStrokeColor testColor)
                        |> getFirstAnnotation
                        |> Maybe.map (Expect.equal (Lines Arrow <| Line start end testColor model.strokeStyle))
                        |> Maybe.withDefault (Expect.fail "Array missing desired annotation")
            , test "does not update a NotSelected annotation" <|
                \() ->
                    model
                        |> addAnnotation (Lines Arrow aLine)
                        |> updateAnySelectedAnnotations (updateStrokeStyle DashedMedium)
                        |> getFirstAnnotation
                        |> Maybe.map (Expect.equal (Lines Arrow aLine))
                        |> Maybe.withDefault (Expect.fail "Array missing desired annotation")
            , test "only updates the Selected annotation" <|
                \() ->
                    model
                        |> addAnnotation (Lines Arrow aLine)
                        |> addAnnotation (Shapes Rect aShape)
                        |> addAnnotation (Shapes Ellipse aShape)
                        |> selectAnnotation 1
                        |> updateAnySelectedAnnotations (updateFill (SolidFill testColor))
                        |> .edits
                        |> .present
                        |> Array.get 2
                        |> Maybe.map (Expect.equal (Shapes Ellipse aShape))
                        |> Maybe.withDefault (Expect.fail "Array missing desired annotation")
            ]
        , describe "annotations"
            [ test "A straight line has the appropriate view attributes" <|
                \() ->
                    aLine
                        |> Lines StraightLine
                        |> viewAnnotation ReadyToDraw 0
                        |> svgDrawspace
                        |> Query.fromHtml
                        |> Query.find [ tag "path" ]
                        |> Query.has (lineSelector aLine)
            , test "An arrow has the appropriate view attributes" <|
                \() ->
                    aLine
                        |> Lines Arrow
                        |> viewAnnotation ReadyToDraw 0
                        |> svgDrawspace
                        |> Query.fromHtml
                        |> Query.find [ tag "path" ]
                        |> Query.has (lineSelector aLine)
            , test "A rectangle has the appropriate view attributes" <|
                \() ->
                    aShape
                        |> Shapes Rect
                        |> viewAnnotation ReadyToDraw 0
                        |> svgDrawspace
                        |> Query.fromHtml
                        |> Query.find [ tag "rect" ]
                        |> Query.has (rectSelector aShape)
            , test "A rounded rectangle has the appropriate view attributes" <|
                \() ->
                    aShape
                        |> Shapes RoundedRect
                        |> viewAnnotation ReadyToDraw 0
                        |> svgDrawspace
                        |> Query.fromHtml
                        |> Query.find [ tag "rect" ]
                        |> Query.has (roundedRectSelector aShape)
            , test "An ellipse has the appropriate view attributes" <|
                \() ->
                    aShape
                        |> Shapes Ellipse
                        |> viewAnnotation ReadyToDraw 0
                        |> svgDrawspace
                        |> Query.fromHtml
                        |> Query.find [ tag "ellipse" ]
                        |> Query.has (ellipseSelector aShape)
            , test "A textbox's unselected svg text has the appropriate view attributes" <|
                \() ->
                    aTextArea
                        |> TextBox
                        |> viewAnnotation ReadyToDraw 0
                        |> svgDrawspace
                        |> Query.fromHtml
                        |> Query.find [ tag "text" ]
                        |> Query.has (svgTextSelector aTextArea)
            , test "A textbox's unselected svg tspans have the appropriate view attributes" <|
                \() ->
                    aTextArea
                        |> TextBox
                        |> viewAnnotation ReadyToDraw 0
                        |> svgDrawspace
                        |> Query.fromHtml
                        |> Query.findAll [ tag "tspan" ]
                        |> Query.each
                            (Expect.all
                                [ Query.has (tspanSelector aTextArea) ]
                            )
            , test "A spotlight has the appropriate view attributes" <|
                \() ->
                    aShape
                        |> Spotlight Rect
                        |> viewAnnotation ReadyToDraw 0
                        |> svgDrawspace
                        |> Query.fromHtml
                        |> Query.find [ tag "rect" ]
                        |> Query.has (rectSelector aShape)
            ]
        , describe "Helpers"
            [ fuzz2 position position "mouse step function works properly" <|
                \pos1 pos2 ->
                    stepMouse pos1 pos2
                        |> arrowAngle pos1
                        |> round
                        |> flip (%) (round (pi / 4))
                        |> Expect.equal 0
            ]
        ]
