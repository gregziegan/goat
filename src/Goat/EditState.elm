module Goat.EditState exposing (EditState, DrawingConfig, AnnotationConfig, SubscriptionConfig, KeyboardConfig, DrawingInfo, SelectingInfo, MovingInfo, ResizingInfo, EditingTextInfo, initialState, startDrawing, continueDrawing, finishDrawing, startMoving, continueMoving, finishMoving, startResizing, continueResizing, finishResizing, selectAnnotation, startEditingText, finishEditingText, updateSelectedAttributes, subscriptions, selectState, updateAnySelectedAnnotations, keyboard, annotationEvents, vertexEvents, drawingEvents, ifDrawing, ifMoving, currentAnnotationAttributes)

import Goat.Annotation exposing (AnnotationAttributes, SelectState, SelectState(..), StrokeStyle, Vertex)
import Goat.ControlOptions as ControlOptions
import Goat.View.EventUtils exposing (defaultPrevented, onMouseDown, onMouseUp, stopPropagation)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json
import Keyboard.Extra as Keyboard exposing (KeyChange)
import Mouse exposing (Position)
import SingleTouch as ST
import Svg exposing (Attribute)
import Svg.Attributes as Attr
import Touch as T


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


type alias SubscriptionConfig msg =
    { drawToMsg : Position -> msg
    , resizeToMsg : Position -> msg
    , moveToMsg : Position -> msg
    , keyboardToMsg : Keyboard.Msg -> msg
    }


type alias AnnotationConfig msg =
    { selectAndMove : Position -> msg
    , contextMenu : Position -> msg
    , startMoving : Position -> msg
    , finishMoving : Position -> msg
    }


type alias DrawingConfig msg =
    { startDrawing : Position -> msg
    , continueDrawing : Position -> msg
    , finishDrawing : Position -> msg
    , continueMoving : Position -> msg
    , finishMoving : Position -> msg
    , continueResizing : Position -> msg
    , finishResizing : Position -> msg
    , finishEditingText : Int -> msg
    , contextMenu : Position -> msg
    }


type alias VertexConfig msg =
    { startResizing : Vertex -> Position -> msg
    }


type alias KeyboardConfig a b =
    { notSelecting : a -> b
    , drawing : DrawingInfo -> a -> b
    , selecting : SelectingInfo -> a -> b
    , moving : MovingInfo -> a -> b
    , resizing : ResizingInfo -> a -> b
    , editingText : EditingTextInfo -> a -> b
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


finishDrawing : EditState -> Maybe ( EditState, DrawingInfo )
finishDrawing editState =
    case editState of
        Drawing drawingInfo ->
            Just ( NotSelecting, drawingInfo )

        _ ->
            Nothing


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


continueMoving : Position -> EditState -> Maybe ( EditState, MovingInfo )
continueMoving newPos editState =
    case editState of
        Moving movingInfo ->
            Just ( Moving { movingInfo | translate = ( newPos.x - movingInfo.start.x, newPos.y - movingInfo.start.y ) }, movingInfo )

        _ ->
            Nothing


finishMoving : EditState -> Maybe ( EditState, MovingInfo )
finishMoving editState =
    case editState of
        Moving ({ id, attributes } as movingInfo) ->
            Just ( Selecting (SelectingInfo id attributes), movingInfo )

        _ ->
            Nothing


startResizing : Position -> Vertex -> ( Position, Position ) -> EditState -> EditState
startResizing start vertex originalCoords editState =
    case editState of
        Selecting { id, attributes } ->
            Resizing (ResizingInfo id start start vertex originalCoords attributes)

        _ ->
            editState


continueResizing : Position -> EditState -> Maybe ( EditState, ResizingInfo )
continueResizing curPos editState =
    case editState of
        Resizing resizingData ->
            Just ( Resizing { resizingData | curPos = curPos }, resizingData )

        _ ->
            Nothing


finishResizing : EditState -> Maybe ( EditState, ResizingInfo )
finishResizing editState =
    case editState of
        Resizing ({ id, attributes } as resizingInfo) ->
            Just ( Selecting (SelectingInfo id attributes), resizingInfo )

        _ ->
            Nothing


startEditingText : Int -> AnnotationAttributes -> EditState -> EditState
startEditingText id attrs editState =
    EditingText (EditingTextInfo id attrs)


finishEditingText : EditState -> Maybe ( EditState, EditingTextInfo )
finishEditingText editState =
    case editState of
        EditingText editingTextInfo ->
            Just ( NotSelecting, editingTextInfo )

        _ ->
            Nothing


updateSelectedAttributes : (AnnotationAttributes -> AnnotationAttributes) -> EditState -> EditState
updateSelectedAttributes updateAttrs editState =
    case editState of
        Selecting selectingInfo ->
            Selecting { selectingInfo | attributes = (updateAttrs selectingInfo.attributes) }

        EditingText editingInfo ->
            EditingText { editingInfo | attributes = (updateAttrs editingInfo.attributes) }

        _ ->
            editState


toDrawingPosition : Mouse.Position -> Mouse.Position
toDrawingPosition mouse =
    { mouse | x = mouse.x - ControlOptions.controlUIWidth, y = mouse.y - 10 }


toPosition : ST.SingleTouch -> Position
toPosition st =
    Position (round st.touch.clientX) (round st.touch.clientY)


subscriptions : SubscriptionConfig msg -> EditState -> Sub msg
subscriptions config editState =
    Sub.batch <|
        case editState of
            Drawing _ ->
                [ Mouse.moves (config.drawToMsg << toDrawingPosition)
                , Sub.map config.keyboardToMsg Keyboard.subscriptions
                ]

            Resizing _ ->
                [ Mouse.moves (config.resizeToMsg << toDrawingPosition)
                , Sub.map config.keyboardToMsg Keyboard.subscriptions
                ]

            Moving _ ->
                [ Mouse.moves (config.moveToMsg << toDrawingPosition) ]

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


keyboard : KeyboardConfig a b -> EditState -> a -> b
keyboard config editState a =
    case editState of
        NotSelecting ->
            config.notSelecting a

        Drawing drawingInfo ->
            config.drawing drawingInfo a

        Selecting selectingInfo ->
            config.selecting selectingInfo a

        Moving movingInfo ->
            config.moving movingInfo a

        Resizing resizingInfo ->
            config.resizing resizingInfo a

        EditingText editingTextInfo ->
            config.editingText editingTextInfo a


annotationEvents : AnnotationConfig msg -> Int -> EditState -> List (Svg.Attribute msg)
annotationEvents config candidateId editState =
    case editState of
        NotSelecting ->
            [ Html.Events.onWithOptions "mousedown" stopPropagation <| Json.map (config.selectAndMove << toDrawingPosition) Mouse.position
            , Attr.class "pointerCursor"
            , onWithOptions "contextmenu" (Html.Events.Options True True) (Json.map (config.contextMenu) Mouse.position)
            ]

        Drawing drawingInfo ->
            [ Attr.class "crosshairCursor" ]

        Selecting selectingInfo ->
            [ Attr.class "moveCursor"
            , Html.Events.onWithOptions "mousedown" stopPropagation <| Json.map (config.startMoving << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchStart T.preventAndStop (config.startMoving << toDrawingPosition << toPosition)
            , onWithOptions "contextmenu" defaultPrevented (Json.map config.contextMenu Mouse.position)
            ]

        Moving { id, translate } ->
            let
                ( dx, dy ) =
                    translate
            in
                [ onMouseUp <| Json.map (config.finishMoving << toDrawingPosition) Mouse.position
                , ST.onSingleTouch T.TouchEnd T.preventAndStop (config.finishMoving << toDrawingPosition << toPosition)
                , Attr.class "moveCursor"
                ]
                    ++ if id == candidateId then
                        [ Attr.transform <| "translate(" ++ toString dx ++ "," ++ toString dy ++ ")" ]
                       else
                        []

        Resizing resizingInfo ->
            [ Attr.class "resizeCursor" ]

        EditingText editingTextInfo ->
            [ Attr.class "crosshairCursor" ]


vertexEvents : VertexConfig msg -> EditState -> Vertex -> List (Attribute msg)
vertexEvents config editState vertex =
    [ Html.Events.onWithOptions "mousedown" stopPropagation <| Json.map (config.startResizing vertex << toDrawingPosition) Mouse.position
    , ST.onSingleTouch T.TouchStart T.preventAndStop (config.startResizing vertex << toDrawingPosition << toPosition)
    ]
        ++ case editState of
            Moving movingInfo ->
                vertexAttrsWhenMoving movingInfo

            _ ->
                []


vertexAttrsWhenMoving : MovingInfo -> List (Svg.Attribute msg)
vertexAttrsWhenMoving { translate } =
    let
        ( dx, dy ) =
            translate
    in
        [ Attr.transform <| "translate(" ++ toString dx ++ "," ++ toString dy ++ ")" ]


updateAnySelectedAnnotations : (Int -> a) -> EditState -> Maybe a
updateAnySelectedAnnotations fn editState =
    case editState of
        Selecting { id } ->
            Just (fn id)

        EditingText { id } ->
            Just (fn id)

        _ ->
            Nothing


currentAnnotationAttributes : EditState -> AnnotationAttributes -> AnnotationAttributes
currentAnnotationAttributes editState defaultAttributes =
    case editState of
        Selecting { attributes } ->
            attributes

        Moving { attributes } ->
            attributes

        Resizing { attributes } ->
            attributes

        EditingText { attributes } ->
            attributes

        _ ->
            defaultAttributes


drawingEvents : DrawingConfig msg -> EditState -> List (Attribute msg)
drawingEvents config editState =
    case editState of
        NotSelecting ->
            [ onMouseDown <| Json.map (config.startDrawing << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchStart T.preventAndStop <| (config.startDrawing << toDrawingPosition << toPosition)
            , onWithOptions "contextmenu" defaultPrevented (Json.map config.contextMenu Mouse.position)
            ]

        Drawing _ ->
            [ onMouseUp (Json.map (config.finishDrawing << toDrawingPosition) Mouse.position)
            , ST.onSingleTouch T.TouchEnd T.preventAndStop (config.finishDrawing << toDrawingPosition << toPosition)
            , ST.onSingleTouch T.TouchMove T.preventAndStop (config.continueDrawing << toDrawingPosition << toPosition)
            , onWithOptions "contextmenu" defaultPrevented (Json.map config.contextMenu Mouse.position)
            ]

        Selecting _ ->
            [ onMouseDown <| Json.map (config.startDrawing << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchStart T.preventAndStop <| (config.startDrawing << toDrawingPosition << toPosition)
            ]

        Moving _ ->
            [ onMouseUp <| Json.map (config.finishMoving << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchMove T.preventAndStop (config.continueMoving << toDrawingPosition << toPosition)
            , ST.onSingleTouch T.TouchEnd T.preventAndStop (config.finishMoving << toDrawingPosition << toPosition)
            , onWithOptions "contextmenu" defaultPrevented (Json.map config.contextMenu Mouse.position)
            ]

        Resizing _ ->
            [ onMouseUp <| Json.map (config.finishResizing << toDrawingPosition) Mouse.position
            , ST.onSingleTouch T.TouchMove T.preventAndStop (config.continueResizing << toDrawingPosition << toPosition)
            , ST.onSingleTouch T.TouchEnd T.preventAndStop (config.continueResizing << toDrawingPosition << toPosition)
            , onWithOptions "contextmenu" defaultPrevented (Json.map config.contextMenu Mouse.position)
            ]

        EditingText { id } ->
            [ Html.Events.onMouseDown (config.finishEditingText id)
            , ST.onSingleTouch T.TouchStart T.preventAndStop (\_ -> config.finishEditingText id)
            , onWithOptions "contextmenu" defaultPrevented (Json.map config.contextMenu Mouse.position)
            , Attr.style "cursor: default;"
            ]


ifDrawing : EditState -> Maybe DrawingInfo
ifDrawing editState =
    case editState of
        Drawing drawingInfo ->
            Just drawingInfo

        _ ->
            Nothing


ifMoving : EditState -> Maybe MovingInfo
ifMoving editState =
    case editState of
        Moving movingInfo ->
            Just movingInfo

        _ ->
            Nothing
