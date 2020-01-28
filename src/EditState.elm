module EditState exposing (AnnotationConfig, DrawingConfig, EditState, KeyboardConfig, SubscriptionConfig, annotationEvents, continueDrawing, continueMoving, continueResizing, currentAnnotationAttributes, drawingEvents, finishDrawing, finishEditingText, finishMoving, finishResizing, finishTextDrawing, ifMoving, initialState, keyboard, selectAnnotation, selectState, startDrawing, startEditingText, startMoving, startResizing, subscriptions, updateAnySelectedAnnotations, updateSelectedAttributes, vertexEvents, viewDrawing)

{-| The finite state machine for annotating.
See <https://github.com/thebritican/goat/wiki/The-Annotation-Editor's-Finite-State-Machine>

EditState's constructors are not exposed so that we force transitions via these top-level
functions, with the exception of `initialState`.

-}

import Annotation exposing (Vertex)
import Annotation.Attributes
import Browser.Events exposing (onMouseMove)
import EventUtils exposing (alwaysPreventDefault, onMouseDown, onMouseUp, stopPropagationAndDefault)
import Html.Events exposing (custom, preventDefaultOn, stopPropagationOn)
import Html.Events.Extra.Touch as T
import Json.Decode as Json
import Keyboard exposing (KeyChange)
import Mouse exposing (Position)
import Svg exposing (Attribute)
import Svg.Attributes as Attr


type alias SelectingInfo =
    { id : Int
    , attributes : Annotation.Attributes
    }


type alias MovingInfo =
    { id : Int
    , start : Position
    , translate : ( Int, Int )
    , attributes : Annotation.Attributes
    }


type alias ResizingInfo =
    { id : Int
    , start : Position
    , curPos : Position
    , vertex : Vertex
    , originalCoords : ( Position, Position )
    , attributes : Annotation.Attributes
    }


type alias EditingTextInfo =
    { id : Int
    , attributes : Annotation.Attributes
    }


type alias DrawingInfo =
    { start : StartPosition
    , current : Position
    , positions : List Position
    , annotation : Annotation
    }


type EditState
    = NotSelecting
    | Drawing DrawingInfo
    | Selecting SelectingInfo
    | Moving MovingInfo
    | Resizing ResizingInfo
    | EditingText EditingTextInfo


{-| EditState provides subscriptions for mouse movements and keyboard interactions.
These subscriptions will be turned on/off depending on the edit state.
-}
type alias SubscriptionConfig msg =
    { drawToMsg : Position -> msg
    , resizeToMsg : Position -> msg
    , moveToMsg : Position -> msg
    , keyboardToMsg : Keyboard.Msg -> msg
    }


{-| Individual Annotations (svgs) are moved on mouse/touch interaction.
-}
type alias AnnotationConfig msg =
    { selectAndMove : Position -> msg
    , contextMenu : Position -> msg
    , startMoving : Position -> msg
    , finishMoving : Position -> msg
    }


{-| This configuration is for the svg drawing area.

This drawing area _should_ only need to be used for drawing, but sometimes the selected vertex is not directly under a quickly
moving mouse. So, we make sure to capture resizing events on the drawspace as the source of truth.

-}
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


{-| This record is exposing all of the information in the state machine.
Ths is used by the consuming app handling all keyboard shortcuts, copy/paste, etc.

TODO: investigate what state dependent logic can be done in this module so that we do
not have to provide this configuration.

-}
type alias KeyboardConfig a b =
    { notSelecting : a -> b
    , drawing : DrawingInfo -> a -> b
    , selecting : SelectingInfo -> a -> b
    , moving : MovingInfo -> a -> b
    , resizing : ResizingInfo -> a -> b
    , editingText : EditingTextInfo -> a -> b
    }


initialState : EditState
initialState =
    NotSelecting


errorMessage : EditState -> String
errorMessage editState =
    editStateToString editState ++ " is not a valid state to start this transition."


startDrawing : Position -> EditState -> Result String EditState
startDrawing start editState =
    case editState of
        NotSelecting ->
            Ok (Drawing (DrawingInfo start start []))

        Selecting selectingInfo ->
            Ok (Drawing (DrawingInfo start start []))

        _ ->
            Err (errorMessage editState)


continueDrawing : Position -> Bool -> EditState -> Result String EditState
continueDrawing pos trackPositions editState =
    case editState of
        Drawing { start, positions } ->
            Ok <|
                if trackPositions then
                    Drawing
                        (DrawingInfo start pos <|
                            case positions of
                                [] ->
                                    [ pos ]

                                lastPos :: rest ->
                                    if abs (lastPos.x - pos.x) < 10 && abs (lastPos.y - pos.y) < 10 then
                                        positions

                                    else
                                        pos :: positions
                        )

                else
                    Drawing (DrawingInfo start pos [])

        _ ->
            Err (errorMessage editState)


editStateToString : EditState -> String
editStateToString editState =
    case editState of
        NotSelecting ->
            "NotSelecting"

        Drawing drawingInfo ->
            "Drawing"

        Selecting selectingInfo ->
            "Selecting"

        Moving movingInfo ->
            "Moving"

        Resizing resizingInfo ->
            "Resizing"

        EditingText editingTextInfo ->
            "EditingText"


finishDrawing : Position -> EditState -> Result String ( EditState, DrawingInfo )
finishDrawing end editState =
    case editState of
        Drawing drawingInfo ->
            Ok ( NotSelecting, { drawingInfo | curPos = end } )

        _ ->
            Err (errorMessage editState)


finishTextDrawing : Position -> Int -> AnnotationAttributes -> EditState -> Result String ( EditState, DrawingInfo )
finishTextDrawing end id attributes editState =
    case editState of
        Drawing drawingInfo ->
            Ok ( EditingText (EditingTextInfo id attributes), { drawingInfo | curPos = end } )

        _ ->
            Err (errorMessage editState)


selectAnnotation : Int -> AnnotationAttributes -> EditState -> Result String EditState
selectAnnotation id annotationAttrs editState =
    case editState of
        NotSelecting ->
            Ok (Selecting (SelectingInfo id annotationAttrs))

        Selecting selectingInfo ->
            Ok (Selecting (SelectingInfo id annotationAttrs))

        Moving movingInfo ->
            Ok (Selecting (SelectingInfo id annotationAttrs))

        Resizing resizingInfo ->
            Ok (Selecting (SelectingInfo id annotationAttrs))

        _ ->
            Err (errorMessage editState)


startMoving : Position -> EditState -> Result String EditState
startMoving start editState =
    case editState of
        Selecting { id, attributes } ->
            Ok (Moving (MovingInfo id start ( 0, 0 ) attributes))

        _ ->
            Err (errorMessage editState)


continueMoving : Position -> EditState -> Result String ( EditState, MovingInfo )
continueMoving newPos editState =
    case editState of
        Moving movingInfo ->
            Ok ( Moving { movingInfo | translate = ( newPos.x - movingInfo.start.x, newPos.y - movingInfo.start.y ) }, movingInfo )

        _ ->
            Err (errorMessage editState)


finishMoving : EditState -> Result String ( EditState, MovingInfo )
finishMoving editState =
    case editState of
        Moving ({ id, attributes } as movingInfo) ->
            Ok ( Selecting (SelectingInfo id attributes), movingInfo )

        _ ->
            Err (errorMessage editState)


startResizing : Position -> Vertex -> ( Position, Position ) -> EditState -> Result String EditState
startResizing start vertex originalCoords editState =
    case editState of
        Selecting { id, attributes } ->
            Ok (Resizing (ResizingInfo id start start vertex originalCoords attributes))

        _ ->
            Err (errorMessage editState)


continueResizing : Position -> EditState -> Result String ( EditState, ResizingInfo )
continueResizing curPos editState =
    case editState of
        Resizing resizingData ->
            Ok ( Resizing { resizingData | curPos = curPos }, resizingData )

        _ ->
            Err (errorMessage editState)


finishResizing : EditState -> Result String ( EditState, ResizingInfo )
finishResizing editState =
    case editState of
        Resizing ({ id, attributes } as resizingInfo) ->
            Ok ( Selecting (SelectingInfo id attributes), resizingInfo )

        _ ->
            Err (errorMessage editState)


startEditingText : Int -> AnnotationAttributes -> EditState -> Result String EditState
startEditingText index annotationAttributes editState =
    case editState of
        Drawing _ ->
            Ok (EditingText (EditingTextInfo index annotationAttributes))

        Selecting { id, attributes } ->
            Ok (EditingText (EditingTextInfo id attributes))

        _ ->
            Err (errorMessage editState)


finishEditingText : EditState -> Result String ( EditState, EditingTextInfo )
finishEditingText editState =
    case editState of
        EditingText editingTextInfo ->
            Ok ( NotSelecting, editingTextInfo )

        _ ->
            Err (errorMessage editState)


updateSelectedAttributes : (AnnotationAttributes -> AnnotationAttributes) -> EditState -> EditState
updateSelectedAttributes updateAttrs editState =
    case editState of
        Selecting selectingInfo ->
            Selecting { selectingInfo | attributes = updateAttrs selectingInfo.attributes }

        EditingText editingInfo ->
            EditingText { editingInfo | attributes = updateAttrs editingInfo.attributes }

        _ ->
            editState


{-| The sidebar is a hard-coded width. This offset is used to shift the incoming mouse position.
TODO: investigate whether this can be skipped by using position: relative, or some
other CSS rule.
-}
controlUIWidth : number
controlUIWidth =
    83


toDrawingPosition : Mouse.Position -> Mouse.Position
toDrawingPosition mouse =
    { mouse | x = mouse.x - controlUIWidth, y = mouse.y - 10 }


subscriptions : SubscriptionConfig msg -> EditState -> Sub msg
subscriptions config editState =
    Sub.batch <|
        case editState of
            Drawing _ ->
                [ onMouseMove (Json.map (config.drawToMsg << toDrawingPosition) Mouse.position)
                , Sub.map config.keyboardToMsg Keyboard.subscriptions
                ]

            Resizing _ ->
                [ onMouseMove (Json.map (config.resizeToMsg << toDrawingPosition) Mouse.position)
                , Sub.map config.keyboardToMsg Keyboard.subscriptions
                ]

            Moving _ ->
                [ onMouseMove (Json.map (config.moveToMsg << toDrawingPosition) Mouse.position) ]

            EditingText _ ->
                []

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


annotationEvents : AnnotationConfig msg -> Int -> EditState -> List (Attribute msg)
annotationEvents config candidateId editState =
    case editState of
        NotSelecting ->
            [ stopPropagationAndDefault "mousedown" (Json.map (config.selectAndMove << toDrawingPosition) Mouse.position)
            , Attr.class "pointerCursor"
            , stopPropagationAndDefault "contextmenu" (Json.map config.contextMenu Mouse.position)
            ]

        Drawing drawingInfo ->
            [ Attr.class "crosshairCursor" ]

        Selecting selectingInfo ->
            [ Attr.class "moveCursor"
            , stopPropagationAndDefault "mousedown" (Json.map (config.startMoving << toDrawingPosition) Mouse.position)
            , stopPropagationAndDefault "contextmenu" (Json.map config.contextMenu Mouse.position)
            ]

        Moving { id, translate } ->
            let
                ( dx, dy ) =
                    translate
            in
            [ onMouseUp <| Json.map (config.finishMoving << toDrawingPosition) Mouse.position
            , Attr.class "moveCursor"
            ]
                ++ (if id == candidateId then
                        [ Attr.transform <| "translate(" ++ String.fromInt dx ++ "," ++ String.fromInt dy ++ ")" ]

                    else
                        []
                   )

        Resizing resizingInfo ->
            [ Attr.class "resizeCursor" ]

        EditingText editingTextInfo ->
            [ Attr.class "crosshairCursor" ]


vertexEvents : (Vertex -> Position -> msg) -> EditState -> Vertex -> List (Attribute msg)
vertexEvents resize editState vertex =
    [ stopPropagationOn "mousedown" <| Json.map (alwaysPreventDefault << resize vertex << toDrawingPosition) Mouse.position
    ]
        ++ (case editState of
                Moving movingInfo ->
                    vertexAttrsWhenMoving movingInfo

                _ ->
                    []
           )


vertexAttrsWhenMoving : MovingInfo -> List (Attribute msg)
vertexAttrsWhenMoving { translate } =
    let
        ( dx, dy ) =
            translate
    in
    [ Attr.transform <| "translate(" ++ String.fromInt dx ++ "," ++ String.fromInt dy ++ ")" ]


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
            , stopPropagationAndDefault "contextmenu" (Json.map config.contextMenu Mouse.position)
            ]

        Drawing _ ->
            [ onMouseUp (Json.map (config.finishDrawing << toDrawingPosition) Mouse.position)
            , stopPropagationAndDefault "contextmenu" (Json.map config.contextMenu Mouse.position)
            ]

        Selecting _ ->
            [ onMouseDown <| Json.map (config.startDrawing << toDrawingPosition) Mouse.position
            ]

        Moving _ ->
            [ onMouseUp <| Json.map (config.finishMoving << toDrawingPosition) Mouse.position
            , stopPropagationAndDefault "contextmenu" (Json.map config.contextMenu Mouse.position)
            ]

        Resizing _ ->
            [ onMouseUp <| Json.map (config.finishResizing << toDrawingPosition) Mouse.position
            , stopPropagationAndDefault "contextmenu" (Json.map config.contextMenu Mouse.position)
            ]

        EditingText { id } ->
            [ Html.Events.onMouseDown (config.finishEditingText id)
            , stopPropagationAndDefault "contextmenu" (Json.map config.contextMenu Mouse.position)
            , Attr.style "cursor: default;"
            ]



{-
   The below functions are being used for two special cases that may be able to be solved
   in a different way.

   viewDrawing -> Pixelate needs to be added to the svg <defs/>, and we therefore need access to DrawingInfo
   ifMoving -> The arrow head is being rotated and translated, but the Svg.Attributes.translate attribute does not stack.
               TODO:
                 a) try to resize/move the arrow path with only Svg.Attributes.translate so both are treated equally
                 b) try to resize/move the arrow head with only the Svg.Attributes.d string
-}


viewDrawing : (DrawingInfo -> a) -> EditState -> Maybe a
viewDrawing fn editState =
    case editState of
        Drawing drawingInfo ->
            Just (fn drawingInfo)

        _ ->
            Nothing


ifMoving : EditState -> Maybe MovingInfo
ifMoving editState =
    case editState of
        Moving movingInfo ->
            Just movingInfo

        _ ->
            Nothing
