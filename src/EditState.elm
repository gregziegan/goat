module EditState exposing
    ( AnnotationConfig
    , DrawingConfig
    , EditState
    , EndPosition
    , Finish(..)
    , MovingInfo
    , ResizingInfo
    , StartPosition
    , SubscriptionConfig
    , continueDrawing
    , continueMoving
    , continueResizing
    , drawingEvents
    , finish
    , initialState
    , selectAnnotation
    , selected
    , startDrawing
    , startEditingText
    , startMoving
    , startResizing
    , subscriptions
    , updateAnySelectedAnnotations
    , view
    , viewDef
    )

{-| The finite state machine for annotating.
See <https://github.com/thebritican/goat/wiki/The-Annotation-Editor's-Finite-State-Machine>

EditState's constructors are not exposed so that we force transitions via these top-level
functions, with the exception of `initialState`.

-}

import Annotation exposing (Annotation, Choice(..))
import Annotation.Vertices exposing (Vertex(..))
import Browser.Events as Events
import EventUtils exposing (alwaysPreventDefault, onMouseDown, onMouseUp, stopPropagationAndDefault)
import Html.Events exposing (stopPropagationOn)
import Json.Decode as Json
import Keyboard
import Position exposing (Position, calcDistance)
import Svg exposing (Attribute, Svg)
import Svg.Attributes as Attr


type alias StartPosition =
    Position


type alias EndPosition =
    Position


type alias MovingInfo =
    { start : Position
    , translate : ( Int, Int )
    }


type alias ResizingInfo =
    { start : Position
    , curPos : Position
    , vertex : Vertex
    , originalCoords : ( Position, Position )
    }


type Finish
    = InvalidTransition String
    | Unsuccessful EditState
    | Successful EditState Annotation


type EditState
    = NotSelecting
    | Drawing Annotation.Id
    | Selecting Annotation.Id
    | Moving Annotation.Id MovingInfo
    | Resizing Annotation.Id ResizingInfo
    | EditingText Annotation.Id


{-| EditState provides subscriptions for mouse movements and keyboard interactions.
These subscriptions will be turned on/off depending on the edit state.
-}
type alias SubscriptionConfig msg =
    { drew : Position -> msg
    , resized : Position -> msg
    , moved : Position -> msg
    , changedKey : Keyboard.Msg -> msg
    , clicked : Annotation.Id -> msg
    , finished : Annotation.Id -> msg
    }


{-| Individual Annotations (svgs) are moved on mouse/touch interaction.
-}
type alias AnnotationConfig msg =
    { selectAndMove : StartPosition -> msg
    , contextMenu : Position -> msg
    , startMoving : StartPosition -> msg
    , resize : Vertex -> StartPosition -> msg
    , annotation : Annotation.Config msg
    , snap : Bool
    }


{-| This configuration is for the svg drawing area.
-}
type alias DrawingConfig msg =
    { startDrawing : Position -> msg
    , contextMenu : Position -> msg
    }


initialState : EditState
initialState =
    NotSelecting


errorMessage : EditState -> String
errorMessage editState =
    editStateToString editState ++ " is not a valid state to start this transition."


startDrawing : Annotation -> EditState -> Result String EditState
startDrawing annotation editState =
    case editState of
        NotSelecting ->
            Ok (Drawing annotation.id)

        Selecting _ ->
            Ok (Drawing annotation.id)

        _ ->
            Err (errorMessage editState)


continueDrawing : Position -> Annotation -> EditState -> Result String ( EditState, Annotation )
continueDrawing pos annotation editState =
    case editState of
        Drawing _ ->
            case annotation.choice of
                FreeHand ->
                    Ok ( editState, { annotation | end = pos, positions = pos :: annotation.positions } )

                _ ->
                    Ok ( editState, { annotation | end = pos } )

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

        Moving _ _ ->
            "Moving"

        Resizing _ _ ->
            "Resizing"

        EditingText _ ->
            "EditingText"


sumDistance : Position -> ( Float, Position ) -> ( Float, Position )
sumDistance position ( distance, previousPosition ) =
    ( distance
        + calcDistance position previousPosition
    , position
    )


finishDrawingFreeHand : Annotation -> Finish
finishDrawingFreeHand annotation =
    let
        ( totalDistance, _ ) =
            List.foldl sumDistance ( 0.0, annotation.start ) (annotation.positions ++ [ annotation.end ])
    in
    if totalDistance < minDrawingDistance then
        Unsuccessful NotSelecting

    else
        Successful NotSelecting annotation


finishDrawingSpotlight : Annotation -> Finish
finishDrawingSpotlight annotation =
    if calcDistance annotation.start annotation.end > minSpotlightDrawingDistance then
        Successful NotSelecting annotation

    else
        Unsuccessful NotSelecting


finishDrawingSvg : Annotation -> Finish
finishDrawingSvg annotation =
    if calcDistance annotation.start annotation.end > minDrawingDistance then
        Successful NotSelecting annotation

    else
        Unsuccessful NotSelecting


finish : AnnotationConfig msg -> Annotation -> EditState -> Finish
finish config annotation editState =
    case editState of
        NotSelecting ->
            InvalidTransition (errorMessage editState)

        Drawing _ ->
            case annotation.choice of
                FreeHand ->
                    finishDrawingFreeHand annotation

                TextBox ->
                    Successful (EditingText annotation.id) annotation

                SpotlightRectangle ->
                    finishDrawingSpotlight annotation

                SpotlightRoundedRectangle ->
                    finishDrawingSpotlight annotation

                SpotlightEllipse ->
                    finishDrawingSpotlight annotation

                _ ->
                    finishDrawingSvg annotation

        Selecting _ ->
            InvalidTransition (errorMessage editState)

        Moving id { translate } ->
            Successful (Selecting id) (Annotation.move translate annotation)

        Resizing id resizingInfo ->
            Successful (Selecting id) (resize (attributes config annotation editState).snap resizingInfo annotation)

        EditingText _ ->
            Successful NotSelecting annotation


selectAnnotation : Annotation -> EditState -> Result String EditState
selectAnnotation annotation editState =
    case editState of
        NotSelecting ->
            Ok (Selecting annotation.id)

        Selecting _ ->
            Ok (Selecting annotation.id)

        Moving _ _ ->
            Ok (Selecting annotation.id)

        Resizing _ _ ->
            Ok (Selecting annotation.id)

        _ ->
            Err (errorMessage editState)


startMoving : Position -> EditState -> Result String EditState
startMoving start editState =
    case editState of
        Selecting annotation ->
            Ok (Moving annotation (MovingInfo start ( 0, 0 )))

        _ ->
            Err (errorMessage editState)


continueMoving : Position -> Annotation -> EditState -> Result String ( EditState, Annotation )
continueMoving newPos annotation editState =
    case editState of
        Moving id movingInfo ->
            let
                ( dx, dy ) =
                    ( newPos.x - movingInfo.start.x, newPos.y - movingInfo.start.y )
            in
            Ok ( Moving id { movingInfo | translate = ( dx, dy ) }, annotation )

        _ ->
            Err (errorMessage editState)


startResizing : Position -> Vertex -> ( Position, Position ) -> EditState -> Result String EditState
startResizing start vertex originalCoords editState =
    case editState of
        Selecting id ->
            Ok (Resizing id (ResizingInfo start start vertex originalCoords))

        _ ->
            Err (errorMessage editState)


continueResizing : Position -> Annotation -> EditState -> Result String ( EditState, Annotation )
continueResizing curPos annotation editState =
    case editState of
        Resizing id resizingData ->
            Ok ( Resizing id { resizingData | curPos = curPos }, annotation )

        _ ->
            Err (errorMessage editState)


startEditingText : EditState -> Result String EditState
startEditingText editState =
    case editState of
        Drawing id ->
            Ok (EditingText id)

        Selecting id ->
            Ok (EditingText id)

        _ ->
            Err (errorMessage editState)


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
            NotSelecting ->
                [ Sub.map config.changedKey Keyboard.subscriptions ]

            Selecting _ ->
                [ Sub.map config.changedKey Keyboard.subscriptions ]

            Drawing id ->
                [ Events.onMouseMove (Json.map (config.drew << toDrawingPosition) Position.decoder)
                , Sub.map config.changedKey Keyboard.subscriptions
                , Events.onMouseUp (Json.succeed (config.finished id))
                ]

            Moving id _ ->
                [ Events.onMouseMove (Json.map (config.moved << toDrawingPosition) Position.decoder)
                , Events.onMouseUp (Json.succeed (config.finished id))
                ]

            Resizing id _ ->
                [ Events.onMouseMove (Json.map (config.resized << toDrawingPosition) Position.decoder)
                , Sub.map config.changedKey Keyboard.subscriptions
                , Events.onMouseUp (Json.succeed (config.finished id))
                ]

            EditingText id ->
                [ Events.onMouseDown (Json.succeed (config.clicked id)) ]


annotationEvents : AnnotationConfig msg -> EditState -> List (Attribute msg)
annotationEvents config editState =
    case editState of
        NotSelecting ->
            [ stopPropagationAndDefault "mousedown" (Json.map (config.selectAndMove << toDrawingPosition) Position.decoder)
            , Attr.class "pointerCursor"
            , stopPropagationAndDefault "contextmenu" (Json.map config.contextMenu Position.decoder)
            ]

        Drawing _ ->
            [ Attr.class "crosshairCursor" ]

        Selecting _ ->
            [ Attr.class "moveCursor"
            , stopPropagationAndDefault "mousedown" (Json.map (config.startMoving << toDrawingPosition) Position.decoder)
            , stopPropagationAndDefault "contextmenu" (Json.map config.contextMenu Position.decoder)
            ]

        Moving _ { translate } ->
            let
                ( dx, dy ) =
                    translate
            in
            [ Attr.class "moveCursor"
            , Attr.transform <| "translate(" ++ String.fromInt dx ++ "," ++ String.fromInt dy ++ ")"
            ]

        Resizing _ _ ->
            [ Attr.class "resizeCursor" ]

        EditingText _ ->
            [ Attr.class "crosshairCursor" ]


vertexEvents : AnnotationConfig msg -> Maybe ( Int, Int ) -> Vertex -> List (Svg.Attribute msg)
vertexEvents config moving vertex =
    (stopPropagationOn "mousedown" <|
        Json.map (alwaysPreventDefault << config.resize vertex << toDrawingPosition) Position.decoder
    )
        :: (case moving of
                Just translate ->
                    vertexAttrsWhenMoving translate

                Nothing ->
                    []
           )


vertexAttrsWhenMoving : ( Int, Int ) -> List (Attribute msg)
vertexAttrsWhenMoving ( dx, dy ) =
    [ Attr.transform <| "translate(" ++ String.fromInt dx ++ "," ++ String.fromInt dy ++ ")" ]


updateAnySelectedAnnotations : (Annotation.Id -> a) -> EditState -> Maybe a
updateAnySelectedAnnotations fn editState =
    case editState of
        Selecting id ->
            Just (fn id)

        EditingText id ->
            Just (fn id)

        _ ->
            Nothing


drawingEvents : DrawingConfig msg -> EditState -> List (Attribute msg)
drawingEvents config editState =
    let
        listenForContextMenu =
            stopPropagationAndDefault "contextmenu" (Json.map config.contextMenu Position.decoder)

        listenForDrawing =
            onMouseDown <| Json.map (config.startDrawing << toDrawingPosition) Position.decoder
    in
    case editState of
        NotSelecting ->
            [ listenForDrawing ]

        Selecting _ ->
            [ listenForDrawing ]

        EditingText _ ->
            [ listenForContextMenu
            , Attr.style "cursor: default;"
            ]

        _ ->
            [ listenForContextMenu ]


resize : Bool -> ResizingInfo -> Annotation -> Annotation
resize snap { curPos, vertex, originalCoords } annotation =
    let
        ( start, end ) =
            originalCoords

        constrain =
            Annotation.resizeFn snap annotation
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


viewHelper : (Annotation.Attributes msg -> Annotation -> view) -> AnnotationConfig msg -> Annotation -> EditState -> view
viewHelper render config annotation editState =
    let
        attrs =
            attributes config annotation editState
    in
    case editState of
        NotSelecting ->
            render attrs annotation

        Drawing _ ->
            render attrs annotation

        Selecting _ ->
            render attrs annotation

        Moving _ _ ->
            render attrs annotation

        Resizing id resizingInfo ->
            if id == annotation.id then
                render attrs (resize (attributes config annotation editState).snap resizingInfo annotation)

            else
                render attrs annotation

        EditingText _ ->
            render attrs annotation


attributes : AnnotationConfig msg -> Annotation -> EditState -> Annotation.Attributes msg
attributes config annotation editState =
    let
        eventsForVertex translate =
            vertexEvents config translate

        static =
            { events = [], translate = ( 0, 0 ), config = config.annotation, snap = False }

        interactive =
            { events = annotationEvents config editState, translate = ( 0, 0 ), config = config.annotation, snap = config.snap }

        moving translate configuredVertices =
            { events = annotationEvents config editState, translate = translate, config = configuredVertices, snap = config.snap }
    in
    case editState of
        NotSelecting ->
            { interactive | snap = False }

        Drawing id ->
            if id == annotation.id then
                interactive

            else
                static

        Selecting id ->
            if id == annotation.id then
                { interactive | config = Annotation.withVertices (eventsForVertex Nothing) config.annotation }

            else
                static

        Moving id { translate } ->
            if id == annotation.id then
                moving translate (Annotation.withVertices (eventsForVertex (Just translate)) config.annotation)

            else
                static

        Resizing id _ ->
            if id == annotation.id then
                { interactive | config = Annotation.withVertices (eventsForVertex Nothing) config.annotation }

            else
                static

        EditingText id ->
            if id == annotation.id then
                interactive

            else
                static


viewDef : AnnotationConfig msg -> Annotation -> EditState -> Annotation.Def msg
viewDef =
    viewHelper Annotation.viewDef


view : AnnotationConfig msg -> Annotation -> EditState -> Svg msg
view =
    viewHelper Annotation.view


minDrawingDistance : number
minDrawingDistance =
    8


minSpotlightDrawingDistance : number
minSpotlightDrawingDistance =
    12


selected : EditState -> Maybe Annotation.Id
selected editState =
    case editState of
        Drawing id ->
            Just id

        Selecting id ->
            Just id

        Resizing id _ ->
            Just id

        Moving id _ ->
            Just id

        EditingText id ->
            Just id

        _ ->
            Nothing
