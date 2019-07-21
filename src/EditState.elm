module EditState exposing
    ( AnnotationConfig
    , DrawingConfig
    , EditState
    , EndPosition
    , MovingInfo
    , ResizingInfo
    , StartPosition
    , SubscriptionConfig
    , continueDrawing
    , continueMoving
    , continueResizing
    , drawingEvents
    , finishDrawing
    , finishEditingText
    , finishMoving
    , finishResizing
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
import Browser.Events exposing (onMouseMove)
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
    }


{-| Individual Annotations (svgs) are moved on mouse/touch interaction.
-}
type alias AnnotationConfig msg =
    { selectAndMove : StartPosition -> msg
    , contextMenu : Position -> msg
    , startMoving : StartPosition -> msg
    , finishMoving : EndPosition -> msg
    , finishResizing : EndPosition -> msg
    , resize : Vertex -> StartPosition -> msg
    , annotation : Annotation.Config msg
    }


{-| This configuration is for the svg drawing area.
-}
type alias DrawingConfig msg =
    { startDrawing : Position -> msg
    , finishDrawing : Position -> msg
    , finishMoving : Annotation.Id -> Position -> msg
    , finishResizing : Annotation.Id -> Position -> msg
    , finishEditingText : Annotation.Id -> msg
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


finishDrawingFreeHand : Annotation -> Maybe Annotation
finishDrawingFreeHand annotation =
    let
        ( totalDistance, _ ) =
            List.foldl sumDistance ( 0.0, annotation.start ) (annotation.positions ++ [ annotation.end ])
    in
    if totalDistance < minDrawingDistance then
        Nothing

    else
        Just annotation


finishDrawingSpotlight : Annotation -> Maybe Annotation
finishDrawingSpotlight annotation =
    if calcDistance annotation.start annotation.end > minSpotlightDrawingDistance then
        Just annotation

    else
        Nothing


finishDrawingSvg : Annotation -> Maybe Annotation
finishDrawingSvg annotation =
    if calcDistance annotation.start annotation.end > minDrawingDistance then
        Just annotation

    else
        Nothing


finishDrawing : Position -> Annotation -> EditState -> Result String ( EditState, Maybe Annotation )
finishDrawing end annotation editState =
    case editState of
        Drawing id ->
            let
                updatedAnnotation =
                    { annotation | end = end }
            in
            case annotation.choice of
                FreeHand ->
                    Ok ( NotSelecting, finishDrawingFreeHand updatedAnnotation )

                TextBox ->
                    Ok ( EditingText id, Just updatedAnnotation )

                SpotlightRectangle ->
                    Ok ( NotSelecting, finishDrawingSpotlight updatedAnnotation )

                SpotlightRoundedRectangle ->
                    Ok ( NotSelecting, finishDrawingSpotlight updatedAnnotation )

                SpotlightEllipse ->
                    Ok ( NotSelecting, finishDrawingSpotlight updatedAnnotation )

                _ ->
                    Ok ( NotSelecting, finishDrawingSvg updatedAnnotation )

        _ ->
            Err (errorMessage editState)


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


finishMoving : Annotation -> EditState -> Result String ( EditState, Annotation )
finishMoving annotation editState =
    case editState of
        Moving id { translate } ->
            Ok ( Selecting id, Annotation.move translate annotation )

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


finishResizing : Annotation.Config msg -> Annotation -> EditState -> Result String ( EditState, Annotation )
finishResizing config annotation editState =
    case editState of
        Resizing id resizingInfo ->
            Ok ( Selecting id, resize config resizingInfo annotation )

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


finishEditingText : Annotation -> EditState -> Result String ( EditState, Annotation )
finishEditingText annotation editState =
    case editState of
        EditingText _ ->
            Ok ( NotSelecting, annotation )

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

            Drawing _ ->
                [ onMouseMove (Json.map (config.drew << toDrawingPosition) Position.decoder)
                , Sub.map config.changedKey Keyboard.subscriptions
                ]

            Moving _ _ ->
                [ onMouseMove (Json.map (config.moved << toDrawingPosition) Position.decoder) ]

            Resizing _ _ ->
                [ onMouseMove (Json.map (config.resized << toDrawingPosition) Position.decoder)
                , Sub.map config.changedKey Keyboard.subscriptions
                ]

            EditingText id ->
                [ Browser.Events.onMouseDown (Json.succeed (config.clicked id)) ]


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
            [ onMouseUp <| Json.map (config.finishMoving << toDrawingPosition) Position.decoder
            , Attr.class "moveCursor"
            , Attr.transform <| "translate(" ++ String.fromInt dx ++ "," ++ String.fromInt dy ++ ")"
            ]

        Resizing _ _ ->
            [ Attr.class "resizeCursor" ]

        EditingText _ ->
            [ Attr.class "crosshairCursor" ]


vertexEvents : AnnotationConfig msg -> Maybe ( Int, Int ) -> Vertex -> List (Svg.Attribute msg)
vertexEvents config moving vertex =
    [ stopPropagationOn "mousedown" <|
        Json.map (alwaysPreventDefault << config.resize vertex << toDrawingPosition) Position.decoder
    , onMouseUp <| Json.map (config.finishResizing << toDrawingPosition) Position.decoder
    ]
        ++ (case moving of
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

        Moving id _ ->
            [ onMouseUp <| Json.map (config.finishMoving id << toDrawingPosition) Position.decoder
            , stopPropagationAndDefault "contextmenu" (Json.map config.contextMenu Position.decoder)
            ]

        Resizing id _ ->
            [ onMouseUp <| Json.map (config.finishResizing id << toDrawingPosition) Position.decoder
            , stopPropagationAndDefault "contextmenu" (Json.map config.contextMenu Position.decoder)
            ]

        EditingText _ ->
            [ stopPropagationAndDefault "contextmenu" (Json.map config.contextMenu Position.decoder)
            , Attr.style "cursor: default;"
            ]


resize : Annotation.Config msg -> ResizingInfo -> Annotation -> Annotation
resize config { curPos, vertex, originalCoords } annotation =
    let
        ( start, end ) =
            originalCoords

        constrain =
            Annotation.resizeFn config annotation
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

        Resizing _ resizingInfo ->
            render attrs (resize config.annotation resizingInfo annotation)

        EditingText _ ->
            render attrs annotation


attributes : AnnotationConfig msg -> Annotation -> EditState -> Annotation.Attributes msg
attributes config annotation editState =
    let
        eventsForVertex translate =
            vertexEvents config translate

        static =
            { events = [], translate = ( 0, 0 ), config = config.annotation }

        interactive =
            { events = annotationEvents config editState, translate = ( 0, 0 ), config = config.annotation }

        moving translate verticesConfigured =
            { events = annotationEvents config editState, translate = translate, config = verticesConfigured }
    in
    case editState of
        NotSelecting ->
            interactive

        Drawing _ ->
            interactive

        Selecting id ->
            if id == annotation.id then
                { interactive | config = Annotation.withVertices (eventsForVertex Nothing) config.annotation }

            else
                interactive

        Moving id { translate } ->
            if id == annotation.id then
                moving translate (Annotation.withVertices (eventsForVertex (Just translate)) config.annotation)

            else
                static

        Resizing id resizingInfo ->
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
    4


minSpotlightDrawingDistance : number
minSpotlightDrawingDistance =
    8


isDrawingTooSmall : Bool -> StartPosition -> EndPosition -> Bool
isDrawingTooSmall isSpotlightDrawing start end =
    if isSpotlightDrawing then
        abs (start.x - end.x) < minSpotlightDrawingDistance && abs (start.y - end.y) < minSpotlightDrawingDistance

    else
        abs (start.x - end.x) < minDrawingDistance && abs (start.y - end.y) < minDrawingDistance


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
