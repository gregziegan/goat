module Goat.EditState exposing (EditState, Config, DrawingInfo, SelectingInfo, MovingInfo, ResizingInfo, EditingTextInfo, Vertex(..), initialState, startDrawing, continueDrawing, finishDrawing, startMoving, continueMoving, finishMoving, startResizing, continueResizing, finishResizing, selectAnnotation, startEditingText, finishEditingText, updateSelectedAttributes, subscriptions, selectState, whenNotSelecting, whenDrawing, whenSelecting, whenMoving, whenResizing, whenEditingText)

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


type alias Config msg =
    { drawToMsg : Position -> msg
    , resizeToMsg : Position -> msg
    , moveToMsg : Position -> msg
    , keyboardToMsg : Keyboard.Msg -> msg
    }


type alias DrawingInfo =
    { start : Position
    , curPos : Position
    , positions : List Position
    }


type alias SelectingInfo =
    { id : Int
    , attributes : AnnotationAttributes
    }


type alias MovingInfo =
    { id : Int
    , start : Position
    , translate : ( Int, Int )
    , attributes : AnnotationAttributes
    }


type alias ResizingInfo =
    { id : Int
    , start : Position
    , curPos : Position
    , vertex : Vertex
    , originalCoords : ( Position, Position )
    , attributes : AnnotationAttributes
    }


type alias EditingTextInfo =
    { id : Int
    , attributes : AnnotationAttributes
    }


{-| The finite state machine for annotating.
See <https://github.com/thebritican/goat/wiki/The-Annotation-Editor's-Finite-State-Machine>
-}
type EditState
    = NotSelecting
    | Drawing DrawingInfo
    | Selecting SelectingInfo
    | Moving MovingInfo
    | Resizing ResizingInfo
    | EditingText EditingTextInfo


initialState : EditState
initialState =
    NotSelecting


startDrawing : Position -> EditState -> EditState
startDrawing start editState =
    Drawing (DrawingInfo start start [])


continueDrawing : Position -> Bool -> EditState -> EditState
continueDrawing pos trackPositions editState =
    case editState of
        Drawing { start, positions } ->
            if trackPositions then
                Drawing
                    (DrawingInfo start pos <|
                        case positions of
                            [] ->
                                [ pos ]

                            lastPos :: rest ->
                                if (abs (lastPos.x - pos.x)) < 10 && (abs (lastPos.y - pos.y)) < 10 then
                                    positions
                                else
                                    pos :: positions
                    )
            else
                Drawing (DrawingInfo start pos [])

        _ ->
            editState


finishDrawing : EditState -> EditState
finishDrawing editState =
    case editState of
        Drawing _ ->
            NotSelecting

        _ ->
            editState


selectAnnotation : Int -> AnnotationAttributes -> EditState -> EditState
selectAnnotation id annotationAttrs editState =
    Selecting (SelectingInfo id annotationAttrs)


startMoving : Position -> EditState -> EditState
startMoving start editState =
    case editState of
        Selecting { id, attributes } ->
            Moving (MovingInfo id start ( 0, 0 ) attributes)

        _ ->
            editState


continueMoving : Position -> EditState -> EditState
continueMoving newPos editState =
    case editState of
        Moving moveInfo ->
            Moving { moveInfo | translate = ( newPos.x - moveInfo.start.x, newPos.y - moveInfo.start.y ) }

        _ ->
            editState


finishMoving : EditState -> EditState
finishMoving editState =
    case editState of
        Moving { id, attributes } ->
            Selecting (SelectingInfo id attributes)

        _ ->
            editState


startResizing : Position -> Vertex -> ( Position, Position ) -> EditState -> EditState
startResizing start vertex originalCoords editState =
    case editState of
        Selecting { id, attributes } ->
            Resizing (ResizingInfo id start start vertex originalCoords attributes)

        _ ->
            editState


continueResizing : Position -> EditState -> EditState
continueResizing curPos editState =
    case editState of
        Resizing resizingData ->
            Resizing { resizingData | curPos = curPos }

        _ ->
            editState


finishResizing : EditState -> EditState
finishResizing editState =
    case editState of
        Resizing { id, attributes } ->
            Selecting (SelectingInfo id attributes)

        _ ->
            editState


startEditingText : Int -> AnnotationAttributes -> EditState -> EditState
startEditingText id attrs editState =
    EditingText (EditingTextInfo id attrs)


finishEditingText : EditState -> EditState
finishEditingText editState =
    case editState of
        EditingText _ ->
            NotSelecting

        _ ->
            editState


updateSelectedAttributes : (AnnotationAttributes -> AnnotationAttributes) -> EditState -> EditState
updateSelectedAttributes updateAttrs editState =
    case editState of
        Selecting selectingInfo ->
            Selecting { selectingInfo | attributes = (updateAttrs selectingInfo.attributes) }

        EditingText editingInfo ->
            EditingText { editingInfo | attributes = (updateAttrs editingInfo.attributes) }

        _ ->
            editState


subscriptions : Config msg -> EditState -> Sub msg
subscriptions config editState =
    Sub.batch <|
        case editState of
            Drawing _ ->
                [ Mouse.moves config.drawToMsg
                , Sub.map config.keyboardToMsg Keyboard.subscriptions
                ]

            Resizing _ ->
                [ Mouse.moves config.resizeToMsg
                , Sub.map config.keyboardToMsg Keyboard.subscriptions
                ]

            Moving _ ->
                [ Mouse.moves config.moveToMsg ]

            _ ->
                [ Sub.map config.keyboardToMsg Keyboard.subscriptions ]


selectState : Int -> Bool -> EditState -> SelectState
selectState candidateId usesVertices editState =
    case editState of
        Selecting { id } ->
            if id == candidateId then
                if usesVertices then
                    SelectedWithVertices
                else
                    Selected
            else
                NotSelected

        Moving { id } ->
            if id == candidateId then
                if usesVertices then
                    SelectedWithVertices
                else
                    Selected
            else
                NotSelected

        Resizing { id } ->
            if id == candidateId then
                if usesVertices then
                    SelectedWithVertices
                else
                    Selected
            else
                NotSelected

        EditingText { id } ->
            if id == candidateId then
                Selected
            else
                NotSelected

        _ ->
            NotSelected


whenNotSelecting : a -> EditState -> a -> a
whenNotSelecting notSelectingValue editState a =
    case editState of
        NotSelecting ->
            notSelectingValue

        _ ->
            a


whenDrawing : (DrawingInfo -> a) -> EditState -> a -> a
whenDrawing f editState a =
    case editState of
        Drawing drawingInfo ->
            f drawingInfo

        _ ->
            a


whenSelecting : (SelectingInfo -> a) -> EditState -> a -> a
whenSelecting f editState a =
    case editState of
        Selecting selectingInfo ->
            f selectingInfo

        _ ->
            a


whenMoving : (MovingInfo -> a) -> EditState -> a -> a
whenMoving f editState a =
    case editState of
        Moving moveInfo ->
            f moveInfo

        _ ->
            a


whenResizing : (ResizingInfo -> a) -> EditState -> a -> a
whenResizing f editState a =
    case editState of
        Resizing resizingInfo ->
            f resizingInfo

        _ ->
            a


whenEditingText : (EditingTextInfo -> a) -> EditState -> a -> a
whenEditingText f editState a =
    case editState of
        EditingText editingTextInfo ->
            f editingTextInfo

        _ ->
            a
