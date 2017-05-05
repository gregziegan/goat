module Goat.EditState exposing (EditState, Config, ResizingData, Vertex(..), initialState, startDrawing, continueDrawing, finishDrawing, startMoving, continueMoving, finishMoving, startResizing, continueResizing, finishResizing, selectAnnotation, startEditingText, finishEditingText, selectedId, updateSelectedAttributes, keyboardInteractions, subscriptions, getDrawingAttributes, map, getSelectState, toDrawingAreaCursor)

import Color exposing (Color)
import Goat.AnnotationAttributes as Annotation exposing (AnnotationAttributes, SelectState, StrokeStyle, SelectState(..))
import Goat.View.EventUtils exposing (stopPropagation)
import Html.Events exposing (onWithOptions)
import Keyboard.Extra as Keyboard exposing (KeyChange)
import Mouse exposing (Position)
import Svg
import Svg.Attributes as Attr


{-| Vertices are classified by their relationship to the `start` and `end`
mouse positions that created the annotation.

e.g: (assume a top-left to bottom-right draw)

Start StartPlusX
+----------+
|**********|
|**********|
|**********|
|**********|
|**********|
+----------+
StartPlusY End

-}
type Vertex
    = Start
    | End
    | StartPlusX
    | StartPlusY


type alias ResizingData =
    { index : Int
    , start : Position
    , curPos : Position
    , vertex : Vertex
    , originalCoords : ( Position, Position )
    }


{-| The finite state machine for annotating.
See <https://github.com/thebritican/goat/wiki/The-Annotation-Editor's-Finite-State-Machine>
-}
type EditState
    = ReadyToDraw
    | DrawingAnnotation Position Position (List Position)
    | SelectedAnnotation Int AnnotationAttributes
    | MovingAnnotation Int Position ( Int, Int ) AnnotationAttributes
    | ResizingAnnotation ResizingData AnnotationAttributes
    | EditingATextBox Int AnnotationAttributes


initialState : EditState
initialState =
    ReadyToDraw


editStateAttributes :
    { strokeColor : Color
    , fill : Maybe Color
    , strokeStyle : StrokeStyle
    , fontSize : Int
    }
    -> EditState
    -> AnnotationAttributes
editStateAttributes { strokeColor, fill, strokeStyle, fontSize } editState =
    case editState of
        SelectedAnnotation _ annotationAttrs ->
            annotationAttrs

        MovingAnnotation _ _ _ annotationAttrs ->
            annotationAttrs

        ResizingAnnotation _ annotationAttrs ->
            annotationAttrs

        EditingATextBox _ annotationAttrs ->
            annotationAttrs

        _ ->
            AnnotationAttributes strokeColor fill strokeStyle fontSize


startDrawing start editState =
    DrawingAnnotation start start []


continueDrawing pos isFreeHand editState =
    case editState of
        DrawingAnnotation start _ freeDrawPositions ->
            if isFreeHand then
                DrawingAnnotation start pos <|
                    case freeDrawPositions of
                        [] ->
                            [ pos ]

                        lastPos :: rest ->
                            if (abs (lastPos.x - pos.x)) < 10 && (abs (lastPos.y - pos.y)) < 10 then
                                freeDrawPositions
                            else
                                pos :: freeDrawPositions
            else
                DrawingAnnotation start pos []

        _ ->
            editState


finishDrawing editState =
    case editState of
        DrawingAnnotation start _ freeDrawPositions ->
            Just ( ReadyToDraw, start, freeDrawPositions )

        _ ->
            Nothing


selectAnnotation id annotationAttrs editState =
    SelectedAnnotation id annotationAttrs


startMoving start editState =
    case editState of
        SelectedAnnotation id attrs ->
            MovingAnnotation id start ( 0, 0 ) attrs

        _ ->
            editState


continueMoving newPos editState =
    case editState of
        MovingAnnotation index start ( dx, dy ) annotationAttrs ->
            MovingAnnotation index start ( newPos.x - start.x, newPos.y - start.y ) annotationAttrs

        _ ->
            editState


finishMoving editState =
    case editState of
        MovingAnnotation annotationId _ translateAmt _ ->
            Just ( ReadyToDraw, annotationId, translateAmt )

        _ ->
            Nothing


startResizing start vertex annotation editState =
    case editState of
        SelectedAnnotation id attrs ->
            ResizingAnnotation (ResizingData id start start vertex (Annotation.positions annotation)) attrs

        _ ->
            editState


continueResizing curPos editState =
    case editState of
        ResizingAnnotation resizingData annotationAttrs ->
            -- | edits = UndoList.mapPresent (mapAtIndex resizingData.index (resize (List.member Shift model.pressedKeys) { resizingData | curPos = curPos })) model.edits
            Just
                ( ResizingAnnotation { resizingData | curPos = curPos } annotationAttrs
                , resizingData
                , annotationAttrs
                )

        _ ->
            Nothing


finishResizing editState =
    case editState of
        ResizingAnnotation { index } attrs ->
            SelectedAnnotation index attrs

        _ ->
            editState


startEditingText id attrs editState =
    EditingATextBox id attrs


finishEditingText editState =
    case editState of
        EditingATextBox _ _ ->
            ReadyToDraw

        _ ->
            editState


updateSelectedAttributes updateAttrs editState =
    case editState of
        SelectedAnnotation id annotationAttrs ->
            SelectedAnnotation id (updateAttrs annotationAttrs)

        EditingATextBox id annotationAttrs ->
            EditingATextBox id (updateAttrs annotationAttrs)

        _ ->
            editState


subscriptions config editState =
    Sub.batch <|
        case editState of
            DrawingAnnotation _ _ _ ->
                [ Mouse.moves config.drawToMsg
                , Sub.map config.keyboardToMsg Keyboard.subscriptions
                ]

            ResizingAnnotation _ _ ->
                [ Mouse.moves config.resizeToMsg
                , Sub.map config.keyboardToMsg Keyboard.subscriptions
                ]

            MovingAnnotation _ _ _ _ ->
                [ Mouse.moves config.moveToMsg ]

            _ ->
                [ Sub.map config.keyboardToMsg Keyboard.subscriptions ]


type alias Config msg a =
    { drawToMsg : Position -> msg
    , resizeToMsg : Position -> msg
    , moveToMsg : Position -> msg
    , keyboardToMsg : Keyboard.Msg -> msg
    , whenNotSelecting : a -> a
    , whenDrawing : a -> a
    , whenSelecting : Int -> a -> a
    , whenMoving : a -> a
    , whenResizing : a -> a
    , whenEditingText : Int -> a -> a
    }


keyboardInteractions : Config msg a -> EditState -> a -> a
keyboardInteractions config editState someModel =
    case editState of
        ReadyToDraw ->
            config.whenNotSelecting someModel

        DrawingAnnotation _ _ _ ->
            config.whenDrawing someModel

        SelectedAnnotation id _ ->
            config.whenSelecting id someModel

        MovingAnnotation _ _ _ _ ->
            config.whenMoving someModel

        ResizingAnnotation _ _ ->
            config.whenResizing someModel

        EditingATextBox id _ ->
            config.whenEditingText id someModel


selectedId editState =
    case editState of
        SelectedAnnotation index _ ->
            Just index

        EditingATextBox index _ ->
            Just index

        _ ->
            Nothing



-- EditingATextBox index _ ->
--     { model
--         | edits = UndoList.mapPresent (mapAtIndex index fn) model.edits
--     }


toDrawingAreaCursor : EditState -> String
toDrawingAreaCursor editState =
    case editState of
        ReadyToDraw ->
            "crosshair"

        DrawingAnnotation _ _ _ ->
            "crosshair"

        MovingAnnotation _ _ _ _ ->
            "move"

        ResizingAnnotation _ _ ->
            "nesw-resize"

        EditingATextBox _ _ ->
            "default"

        _ ->
            "crosshair"


getSelectState : Int -> Bool -> EditState -> SelectState
getSelectState annIndex isFreeHand editState =
    case editState of
        SelectedAnnotation index _ ->
            if index == annIndex then
                if isFreeHand then
                    Selected
                else
                    SelectedWithVertices
            else
                NotSelected

        MovingAnnotation index _ _ _ ->
            if index == annIndex then
                if isFreeHand then
                    Selected
                else
                    SelectedWithVertices
            else
                NotSelected

        ResizingAnnotation { index } _ ->
            if index == annIndex then
                if isFreeHand then
                    Selected
                else
                    SelectedWithVertices
            else
                NotSelected

        EditingATextBox index _ ->
            if index == annIndex then
                Selected
            else
                NotSelected

        _ ->
            NotSelected


map : Config msg a -> EditState -> a -> a
map config editState someModel =
    case editState of
        ReadyToDraw ->
            config.whenNotSelecting someModel

        DrawingAnnotation _ _ _ ->
            config.whenDrawing someModel

        SelectedAnnotation id _ ->
            config.whenSelecting id someModel

        MovingAnnotation _ _ _ _ ->
            config.whenMoving someModel

        ResizingAnnotation _ _ ->
            config.whenResizing someModel

        EditingATextBox id _ ->
            config.whenEditingText id someModel


getDrawingAttributes editState =
    case editState of
        DrawingAnnotation start curPos freeDrawPositions ->
            Just ( start, curPos, freeDrawPositions )

        _ ->
            Nothing
