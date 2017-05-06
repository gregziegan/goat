module Goat.EditState exposing (EditState, Config, MoveInfo, ResizingInfo, Vertex(..), initialState, startDrawing, continueDrawing, finishDrawing, startMoving, continueMoving, finishMoving, startResizing, continueResizing, finishResizing, selectAnnotation, startEditingText, finishEditingText, selectedId, updateSelectedAttributes, subscriptions, getDrawingAttributes, getSelectState, toDrawingAreaCursor, whenNotSelecting, whenDrawing, whenSelecting, whenMoving, whenResizing, whenEditingText)

import Color exposing (Color)
import Goat.AnnotationAttributes as Annotation exposing (AnnotationAttributes, SelectState, StrokeStyle, SelectState(..))
import Keyboard.Extra as Keyboard exposing (KeyChange)
import Mouse exposing (Position)


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


type alias DrawingInfo =
    { start : Position
    , curPos : Position
    , freeDrawPositions : List Position
    }


type alias MoveInfo =
    { id : Int
    , start : Position
    , translate : ( Int, Int )
    , attributes : AnnotationAttributes
    }


type alias ResizingInfo =
    { index : Int
    , start : Position
    , curPos : Position
    , vertex : Vertex
    , originalCoords : ( Position, Position )
    , attributes : AnnotationAttributes
    }


{-| The finite state machine for annotating.
See <https://github.com/thebritican/goat/wiki/The-Annotation-Editor's-Finite-State-Machine>
-}
type EditState
    = ReadyToDraw
    | DrawingAnnotation DrawingInfo
    | SelectedAnnotation Int AnnotationAttributes
    | MovingAnnotation MoveInfo
    | ResizingAnnotation ResizingInfo
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

        MovingAnnotation { attributes } ->
            attributes

        ResizingAnnotation { attributes } ->
            attributes

        EditingATextBox _ annotationAttrs ->
            annotationAttrs

        _ ->
            AnnotationAttributes strokeColor fill strokeStyle fontSize


startDrawing start editState =
    DrawingAnnotation (DrawingInfo start start [])


continueDrawing pos isFreeHand editState =
    case editState of
        DrawingAnnotation { start, freeDrawPositions } ->
            if isFreeHand then
                DrawingAnnotation
                    (DrawingInfo start pos <|
                        case freeDrawPositions of
                            [] ->
                                [ pos ]

                            lastPos :: rest ->
                                if (abs (lastPos.x - pos.x)) < 10 && (abs (lastPos.y - pos.y)) < 10 then
                                    freeDrawPositions
                                else
                                    pos :: freeDrawPositions
                    )
            else
                DrawingAnnotation (DrawingInfo start pos [])

        _ ->
            editState


finishDrawing editState =
    case editState of
        DrawingAnnotation { start, freeDrawPositions } ->
            Just ( ReadyToDraw, start, freeDrawPositions )

        _ ->
            Nothing


selectAnnotation id annotationAttrs editState =
    SelectedAnnotation id annotationAttrs


startMoving start editState =
    case editState of
        SelectedAnnotation id attrs ->
            MovingAnnotation (MoveInfo id start ( 0, 0 ) attrs)

        _ ->
            editState


continueMoving newPos editState =
    case editState of
        MovingAnnotation moveInfo ->
            MovingAnnotation { moveInfo | translate = ( newPos.x - moveInfo.start.x, newPos.y - moveInfo.start.y ) }

        _ ->
            editState


finishMoving editState =
    case editState of
        MovingAnnotation { id, translate } ->
            Just ( ReadyToDraw, id, translate )

        _ ->
            Nothing


startResizing start vertex annotation editState =
    case editState of
        SelectedAnnotation id attrs ->
            ResizingAnnotation (ResizingInfo id start start vertex (Annotation.positions annotation) attrs)

        _ ->
            editState


continueResizing curPos editState =
    case editState of
        ResizingAnnotation resizingData ->
            -- | edits = UndoList.mapPresent (mapAtIndex resizingData.index (resize (List.member Shift model.pressedKeys) { resizingData | curPos = curPos })) model.edits
            Just
                ( ResizingAnnotation { resizingData | curPos = curPos }
                , resizingData
                )

        _ ->
            Nothing


finishResizing editState =
    case editState of
        ResizingAnnotation { index, attributes } ->
            SelectedAnnotation index attributes

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
            DrawingAnnotation _ ->
                [ Mouse.moves config.drawToMsg
                , Sub.map config.keyboardToMsg Keyboard.subscriptions
                ]

            ResizingAnnotation _ ->
                [ Mouse.moves config.resizeToMsg
                , Sub.map config.keyboardToMsg Keyboard.subscriptions
                ]

            MovingAnnotation _ ->
                [ Mouse.moves config.moveToMsg ]

            _ ->
                [ Sub.map config.keyboardToMsg Keyboard.subscriptions ]


type alias Config msg =
    { drawToMsg : Position -> msg
    , resizeToMsg : Position -> msg
    , moveToMsg : Position -> msg
    , keyboardToMsg : Keyboard.Msg -> msg
    }


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

        DrawingAnnotation _ ->
            "crosshair"

        MovingAnnotation _ ->
            "move"

        ResizingAnnotation _ ->
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

        MovingAnnotation { id } ->
            if id == annIndex then
                if isFreeHand then
                    Selected
                else
                    SelectedWithVertices
            else
                NotSelected

        ResizingAnnotation { index } ->
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



-- map : Config msg a -> EditState -> a -> a
-- map config editState someModel =
--     case editState of
--         ReadyToDraw ->
--             config.whenNotSelecting someModel
--
--         DrawingAnnotation _ _ _ ->
--             config.whenDrawing someModel
--
--         SelectedAnnotation id _ ->
--             config.whenSelecting id someModel
--
--         MovingAnnotation moveInfo ->
--             config.whenMoving moveInfo someModel
--
--         ResizingAnnotation _ _ ->
--             config.whenResizing someModel
--
--         EditingATextBox id _ ->
--             config.whenEditingText id someModel


whenNotSelecting : a -> EditState -> a -> a
whenNotSelecting notSelectingVal editState a =
    case editState of
        ReadyToDraw ->
            notSelectingVal

        _ ->
            a


whenDrawing : (DrawingInfo -> a) -> EditState -> a -> a
whenDrawing f editState a =
    case editState of
        DrawingAnnotation drawingInfo ->
            f drawingInfo

        _ ->
            a


whenSelecting : (Int -> a) -> EditState -> a -> a
whenSelecting f editState a =
    case editState of
        SelectedAnnotation id _ ->
            f id

        _ ->
            a


whenMoving : (MoveInfo -> a) -> EditState -> a -> a
whenMoving f editState a =
    case editState of
        MovingAnnotation moveInfo ->
            f moveInfo

        _ ->
            a


whenResizing : (ResizingInfo -> a) -> EditState -> a -> a
whenResizing f editState a =
    case editState of
        ResizingAnnotation resizingInfo ->
            f resizingInfo

        _ ->
            a


whenEditingText : (Int -> a) -> EditState -> a -> a
whenEditingText f editState a =
    case editState of
        EditingATextBox id _ ->
            f id

        _ ->
            a


getDrawingAttributes editState =
    case editState of
        DrawingAnnotation { start, curPos, freeDrawPositions } ->
            Just ( start, curPos, freeDrawPositions )

        _ ->
            Nothing
