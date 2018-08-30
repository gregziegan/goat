module EditState exposing
    ( AnnotationConfig
    , DrawingConfig
    , DrawingInfo
    , EditState
    , EndPosition
    , MovingInfo
    , ResizingInfo
    , SelectState(..)
    , SelectingInfo
    , StartPosition
    , SubscriptionConfig
    , annotationAttributes
    , annotationEvents
    , continueDrawing
    , continueMoving
    , continueResizing
    , drawingEvents
    , finishDrawing
    , finishEditingText
    , finishMoving
    , finishResizing
    , finishTextDrawing
    , ifMoving
    , initialState
    , mapSelected
    , resizeVertices
    , selectAnnotation
    , selectState
    , startDrawing
    , startEditingText
    , startMoving
    , startResizing
    , subscriptions
    , textAreaDomId
    , updateAnySelectedAnnotations
    , updateSelectedStyles
    , vertexEvents
    , viewDrawing
    , whenTyping
    )

{-| The finite state machine for annotating.
See <https://github.com/thebritican/goat/wiki/The-Annotation-Editor's-Finite-State-Machine>

EditState's constructors are not exposed so that we force transitions via these top-level
functions, with the exception of `initialState`.

-}

import Browser.Events exposing (onMouseMove)
import Drawing.Options exposing (DrawingStyles)
import DrawingArea.Vertices exposing (Vertex(..))
import EventUtils exposing (alwaysPreventDefault, onMouseDown, onMouseUp, stopPropagationAndDefault)
import Html.Events exposing (stopPropagationOn)
import Json.Decode as Json
import Keyboard
import Position exposing (Position)
import Svg exposing (Attribute)
import Svg.Attributes as Attr


type alias StartPosition =
    Position


type alias EndPosition =
    Position


type alias Id =
    Int


type alias DrawingInfo =
    { start : Position
    , curPos : Position
    , positions : List Position
    }


type alias SelectingInfo =
    { id : Id
    , styles : DrawingStyles
    }


type alias MovingInfo =
    { id : Id
    , start : Position
    , translate : ( Int, Int )
    , styles : DrawingStyles
    }


type alias ResizingInfo =
    { id : Id
    , start : Position
    , curPos : Position
    , vertex : Vertex
    , originalCoords : ( Position, Position )
    , styles : DrawingStyles
    }


type alias EditingTextInfo =
    { id : Id
    , styles : DrawingStyles
    }


{-| Annotations are viewed differently based on the kind of selection.

1.  Selected corresponds to annotations that are not in a state for resizing/moving.
    This is currently only relevant to Textboxes when they are being edited.
2.  SelectedWithVertices shows vertices on any annotation that allows for resizing/moving
3.  NotSelected shows the unadorned annotation

-}
type SelectState
    = Selected
    | SelectedWithVertices
    | NotSelected


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
    { selectAndMove : Id -> Position -> msg
    , contextMenu : Id -> Position -> msg
    , startMoving : Id -> Position -> msg
    , finishMoving : Position -> msg
    , defaultAttributes : DrawingStyles
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
    , finishEditingText : Id -> msg
    , contextMenu : Position -> msg
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

        Selecting _ ->
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

                                lastPos :: _ ->
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

        Drawing _ ->
            "Drawing"

        Selecting _ ->
            "Selecting"

        Moving _ ->
            "Moving"

        Resizing _ ->
            "Resizing"

        EditingText _ ->
            "EditingText"


finishDrawing : Position -> EditState -> Result String ( EditState, DrawingInfo )
finishDrawing end editState =
    case editState of
        Drawing drawingInfo ->
            Ok ( NotSelecting, { drawingInfo | curPos = end } )

        _ ->
            Err (errorMessage editState)


finishTextDrawing : Position -> Id -> DrawingStyles -> EditState -> Result String ( EditState, DrawingInfo )
finishTextDrawing end id styles editState =
    case editState of
        Drawing drawingInfo ->
            Ok ( EditingText (EditingTextInfo id styles), { drawingInfo | curPos = end } )

        _ ->
            Err (errorMessage editState)


selectAnnotation : Id -> DrawingStyles -> EditState -> Result String EditState
selectAnnotation id annotationAttrs editState =
    case editState of
        NotSelecting ->
            Ok (Selecting (SelectingInfo id annotationAttrs))

        Selecting _ ->
            Ok (Selecting (SelectingInfo id annotationAttrs))

        Moving _ ->
            Ok (Selecting (SelectingInfo id annotationAttrs))

        Resizing _ ->
            Ok (Selecting (SelectingInfo id annotationAttrs))

        _ ->
            Err (errorMessage editState)


startMoving : Position -> EditState -> Result String EditState
startMoving start editState =
    case editState of
        Selecting { id, styles } ->
            Ok (Moving (MovingInfo id start ( 0, 0 ) styles))

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
        Moving ({ id, styles } as movingInfo) ->
            Ok ( Selecting (SelectingInfo id styles), movingInfo )

        _ ->
            Err (errorMessage editState)


startResizing : Position -> Vertex -> ( Position, Position ) -> EditState -> Result String EditState
startResizing start vertex originalCoords editState =
    case editState of
        Selecting { id, styles } ->
            Ok (Resizing (ResizingInfo id start start vertex originalCoords styles))

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
        Resizing ({ id, styles } as resizingInfo) ->
            Ok ( Selecting (SelectingInfo id styles), resizingInfo )

        _ ->
            Err (errorMessage editState)


startEditingText : Id -> DrawingStyles -> EditState -> Result String EditState
startEditingText index annotationAttrs editState =
    case editState of
        Drawing _ ->
            Ok (EditingText (EditingTextInfo index annotationAttrs))

        Selecting { id, styles } ->
            Ok (EditingText (EditingTextInfo id styles))

        _ ->
            Err (errorMessage editState)


finishEditingText : EditState -> Result String ( EditState, EditingTextInfo )
finishEditingText editState =
    case editState of
        EditingText editingTextInfo ->
            Ok ( NotSelecting, editingTextInfo )

        _ ->
            Err (errorMessage editState)


updateSelectedStyles : (DrawingStyles -> DrawingStyles) -> EditState -> EditState
updateSelectedStyles updateStyles editState =
    case editState of
        Selecting selectingInfo ->
            Selecting { selectingInfo | styles = updateStyles selectingInfo.styles }

        EditingText editingInfo ->
            EditingText { editingInfo | styles = updateStyles editingInfo.styles }

        _ ->
            editState


{-| The sidebar is a hard-coded width. This offset is used to shift the incoming mouse position.
TODO: investigate whether this can be skipped by using position: relative, or some
other CSS rule.
-}
controlUIWidth : number
controlUIWidth =
    83


toDrawingPosition : Position -> Position
toDrawingPosition mouse =
    { mouse | x = mouse.x - controlUIWidth, y = mouse.y - 10 }


subscriptions : SubscriptionConfig msg -> EditState -> Sub msg
subscriptions config editState =
    Sub.batch <|
        case editState of
            Drawing _ ->
                [ onMouseMove (Json.map (config.drawToMsg << toDrawingPosition) Position.decoder)
                , Sub.map config.keyboardToMsg Keyboard.subscriptions
                ]

            Resizing _ ->
                [ onMouseMove (Json.map (config.resizeToMsg << toDrawingPosition) Position.decoder)
                , Sub.map config.keyboardToMsg Keyboard.subscriptions
                ]

            Moving _ ->
                [ onMouseMove (Json.map (config.moveToMsg << toDrawingPosition) Position.decoder) ]

            EditingText _ ->
                []

            _ ->
                [ Sub.map config.keyboardToMsg Keyboard.subscriptions ]


type alias UsesVertices =
    Bool


selectState : Id -> UsesVertices -> EditState -> SelectState
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


annotationEvents : AnnotationConfig msg -> Id -> EditState -> List (Attribute msg)
annotationEvents config candidateId editState =
    case editState of
        NotSelecting ->
            [ stopPropagationAndDefault "mousedown" (Json.map (config.selectAndMove candidateId << toDrawingPosition) Position.decoder)
            , Attr.class "pointerCursor"
            , stopPropagationAndDefault "contextmenu" (Json.map (config.contextMenu candidateId) Position.decoder)
            ]

        Drawing _ ->
            [ Attr.class "crosshairCursor" ]

        Selecting _ ->
            [ Attr.class "moveCursor"
            , stopPropagationAndDefault "mousedown" (Json.map (config.startMoving candidateId << toDrawingPosition) Position.decoder)
            , stopPropagationAndDefault "contextmenu" (Json.map (config.contextMenu candidateId) Position.decoder)
            ]

        Moving { id, translate } ->
            let
                ( dx, dy ) =
                    translate
            in
            [ onMouseUp <| Json.map (config.finishMoving << toDrawingPosition) Position.decoder
            , Attr.class "moveCursor"
            ]
                ++ (if id == candidateId then
                        [ Attr.transform <| "translate(" ++ String.fromInt dx ++ "," ++ String.fromInt dy ++ ")" ]

                    else
                        []
                   )

        Resizing _ ->
            [ Attr.class "resizeCursor" ]

        EditingText _ ->
            [ Attr.class "crosshairCursor" ]


vertexEvents : (Vertex -> Position -> msg) -> EditState -> Vertex -> List (Attribute msg)
vertexEvents resize editState vertex =
    (stopPropagationOn "mousedown" <| Json.map (alwaysPreventDefault << resize vertex << toDrawingPosition) Position.decoder)
        :: (case editState of
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


updateAnySelectedAnnotations : (Id -> a) -> EditState -> Maybe a
updateAnySelectedAnnotations fn editState =
    case editState of
        Selecting { id } ->
            Just (fn id)

        EditingText { id } ->
            Just (fn id)

        _ ->
            Nothing


annotationAttributes : AnnotationConfig msg -> EditState -> DrawingStyles
annotationAttributes config editState =
    case editState of
        Selecting { styles } ->
            styles

        Moving { styles } ->
            styles

        Resizing { styles } ->
            styles

        EditingText { styles } ->
            styles

        _ ->
            config.defaultAttributes


drawingEvents : DrawingConfig msg -> EditState -> List (Attribute msg)
drawingEvents config editState =
    case editState of
        NotSelecting ->
            [ onMouseDown <| Json.map (config.startDrawing << toDrawingPosition) Position.decoder
            , stopPropagationAndDefault "contextmenu" (Json.map config.contextMenu Position.decoder)
            ]

        Drawing _ ->
            [ onMouseUp (Json.map (config.finishDrawing << toDrawingPosition) Position.decoder)
            , stopPropagationAndDefault "contextmenu" (Json.map config.contextMenu Position.decoder)
            ]

        Selecting _ ->
            [ onMouseDown <| Json.map (config.startDrawing << toDrawingPosition) Position.decoder
            ]

        Moving _ ->
            [ onMouseUp <| Json.map (config.finishMoving << toDrawingPosition) Position.decoder
            , stopPropagationAndDefault "contextmenu" (Json.map config.contextMenu Position.decoder)
            ]

        Resizing _ ->
            [ onMouseUp <| Json.map (config.finishResizing << toDrawingPosition) Position.decoder
            , stopPropagationAndDefault "contextmenu" (Json.map config.contextMenu Position.decoder)
            ]

        EditingText { id } ->
            [ Html.Events.onMouseDown (config.finishEditingText id)
            , stopPropagationAndDefault "contextmenu" (Json.map config.contextMenu Position.decoder)
            , Attr.style "cursor: default;"
            ]



{-
   The below functions are being used for two special cases that may be able to be solved
   in a different way.

   viewDrawing -> Pixelate needs to be added to the svg <defs/>, and we therefore need access to DrawingInfo
   ifMoving -> The arrow head is being rotated and translated, but the Svg.Attributes.translate attribute does not stack.
              We could
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


whenTyping : (Maybe Id -> a) -> EditState -> a
whenTyping fn editState =
    case editState of
        EditingText { id } ->
            fn (Just id)

        _ ->
            fn Nothing


mapSelected : (Maybe Id -> a) -> EditState -> a
mapSelected fn editState =
    case editState of
        Selecting selectingInfo ->
            fn (Just selectingInfo.id)

        Moving movingInfo ->
            fn (Just movingInfo.id)

        Resizing resizingInfo ->
            fn (Just resizingInfo.id)

        EditingText editingInfo ->
            fn (Just editingInfo.id)

        _ ->
            fn Nothing


resizeVertices :
    (StartPosition -> EndPosition -> Position)
    -> ResizingInfo
    -> { a | start : Position, end : Position }
    -> { a | start : Position, end : Position }
resizeVertices constrain { curPos, vertex, originalCoords } annotation =
    let
        ( start, end ) =
            originalCoords
    in
    case vertex of
        Start ->
            { annotation | start = constrain annotation.end curPos }

        End ->
            { annotation | end = constrain annotation.start curPos }

        StartPlusX ->
            { annotation | start = constrain annotation.end curPos, end = Position start.x end.y }

        StartPlusY ->
            { annotation | start = constrain annotation.end curPos, end = Position end.x start.y }


textAreaDomId : Id -> String
textAreaDomId id =
    "text-box-edit--" ++ String.fromInt id
