module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Environment exposing (Environment, OperatingSystem(..), Platform(..))
import Flags
import Html
import Image exposing (Image)
import Json.Decode as Decode exposing (Value)
import Page
import Page.Annotate as Annotate
import Page.Blank as Blank
import Page.Gallery as Gallery
import Page.NotFound as NotFound
import Page.Upload as Upload
import Ports
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)


type App
    = Redirect Session
    | NotFound Session
    | Upload Upload.Model
    | Annotate Annotate.Model
    | Gallery Gallery.Model


type alias Model =
    { environment : Environment
    , images : RemoteData String (List Image)
    , selected : Maybe Image.Id
    , app : App
    }



-- MODEL


initHelp : Environment -> App -> Model
initHelp environment app =
    { environment = environment
    , images = NotAsked
    , selected = Nothing
    , app = app
    }


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init value url navKey =
    case Flags.decode value of
        Ok flags ->
            changeRouteTo (Route.fromUrl url) (initHelp flags.environment (Redirect (Session.fromNavKey navKey)))

        Err _ ->
            changeRouteTo (Route.fromUrl url) (initHelp (Environment Windows Web) (Redirect (Session.fromNavKey navKey)))



-- VIEW


view : Model -> Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model.app of
        Redirect _ ->
            viewPage Page.Other (\_ -> Ignored) Blank.view

        NotFound _ ->
            viewPage Page.Other (\_ -> Ignored) NotFound.view

        Upload upload ->
            viewPage Page.Upload GotUploadMsg (Upload.view model.environment upload)

        Annotate annotate ->
            case getImage model of
                Just image ->
                    viewPage Page.Annotate GotAnnotateMsg (Annotate.view model.environment image annotate)

                Nothing ->
                    viewPage Page.Other (\_ -> Ignored) Blank.view

        Gallery gallery ->
            case model.images of
                Success [] ->
                    viewPage Page.Other (\_ -> Ignored) Blank.view

                Success images ->
                    viewPage Page.Gallery GotGalleryMsg (Gallery.view images gallery)

                _ ->
                    viewPage Page.Other (\_ -> Ignored) Blank.view



-- UPDATE


type Msg
    = Ignored
    | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotUploadMsg Upload.Msg
    | GotAnnotateMsg Annotate.Msg
    | GotGalleryMsg Gallery.Msg
    | UploadImage (Result Decode.Error Image)
    | SetImages (Result Decode.Error (List Image))


toSession : App -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Upload upload ->
            Upload.toSession upload

        Gallery gallery ->
            Gallery.toSession gallery

        Annotate annotate ->
            Annotate.toSession annotate


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model.app
    in
    case maybeRoute of
        Nothing ->
            ( { model | app = NotFound session }, Cmd.none )

        Just Route.Root ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Upload )

        Just Route.Upload ->
            Upload.init session
                |> updateWith Upload GotUploadMsg model
                |> Tuple.mapSecond (addCmd (Ports.listenForUpload ()))

        Just (Route.Annotate Nothing) ->
            changeRouteToAnnotate session model

        Just (Route.Annotate (Just imageId)) ->
            changeRouteToAnnotate session { model | selected = Just imageId }

        Just Route.Gallery ->
            Gallery.init session
                |> updateWith Gallery GotGalleryMsg model
                |> Tuple.mapSecond (addCmd (Ports.requestImages ()))

        Just Route.Goats ->
            Gallery.init session
                |> updateWith Gallery GotGalleryMsg { model | images = Success Image.goats }


getImage : Model -> Maybe Image
getImage model =
    case model.images of
        Success images ->
            case model.selected of
                Just imageId ->
                    selectedImage imageId images

                Nothing ->
                    Nothing

        _ ->
            Nothing


selectedImage : Image.Id -> List Image -> Maybe Image
selectedImage selected images =
    List.head <| List.filter ((==) selected << .id) images


changeRouteToAnnotate : Session -> Model -> ( Model, Cmd Msg )
changeRouteToAnnotate session model =
    case model.selected of
        Just imageId ->
            changeRouteToAnnotateHelp session imageId model

        Nothing ->
            case model.images of
                NotAsked ->
                    ( model, Nav.replaceUrl (Session.navKey (toSession model.app)) "#/upload" )

                Loading ->
                    ( model, Cmd.none )

                Success [] ->
                    ( model, Nav.replaceUrl (Session.navKey (toSession model.app)) "#/upload" )

                Success _ ->
                    ( model, Nav.replaceUrl (Session.navKey (toSession model.app)) "#/gallery" )

                Failure _ ->
                    ( model, Cmd.none )


changeRouteToAnnotateHelp : Session -> Image.Id -> Model -> ( Model, Cmd Msg )
changeRouteToAnnotateHelp session imageId model =
    case model.images of
        NotAsked ->
            ( model, Nav.replaceUrl (Session.navKey (toSession model.app)) "#/upload" )

        Loading ->
            ( model, Cmd.none )

        Success [] ->
            ( model, Nav.replaceUrl (Session.navKey (toSession model.app)) "#/upload" )

        Success images ->
            case selectedImage imageId images of
                Just image ->
                    Annotate.init session model.environment
                        |> updateWith Annotate GotAnnotateMsg model

                Nothing ->
                    ( model, Cmd.none )

        Failure _ ->
            ( model, Cmd.none )


addCmd : Cmd msg -> Cmd msg -> Cmd msg
addCmd cmd1 cmd2 =
    Cmd.batch [ cmd1, cmd2 ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.app ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Nav.pushUrl (Session.navKey (toSession model.app)) (Url.toString url)
                            )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model

        ( GotUploadMsg subMsg, Upload upload ) ->
            Upload.update subMsg upload
                |> updateWith Upload GotUploadMsg model

        ( GotGalleryMsg subMsg, Gallery gallery ) ->
            Gallery.update subMsg gallery
                |> updateWith Gallery GotGalleryMsg model

        ( GotAnnotateMsg subMsg, Annotate annotate ) ->
            case getImage model of
                Just image ->
                    Annotate.update model.environment image subMsg annotate
                        |> updateWith Annotate GotAnnotateMsg model

                Nothing ->
                    ( model, Nav.pushUrl (Session.navKey (toSession model.app)) "#/upload" )

        ( UploadImage result, _ ) ->
            case result of
                Ok image ->
                    ( { model | selected = Just image.id, images = Success [ image ] }, Nav.pushUrl (Session.navKey (toSession model.app)) "#/annotate" )

                Err _ ->
                    ( model, Cmd.none )

        ( SetImages result, _ ) ->
            case result of
                Ok images ->
                    ( { model | images = Success images }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )



-- SUBSCRIPTIONS


valueToImage : Value -> Result Decode.Error Image
valueToImage =
    Decode.decodeValue Image.decode


valueToImages : Value -> Result Decode.Error (List Image)
valueToImages =
    Decode.decodeValue (Decode.list Image.decode)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.app of
        Redirect _ ->
            Sub.none

        NotFound _ ->
            Sub.none

        Upload _ ->
            Ports.newImage (UploadImage << Decode.decodeValue Image.decode)

        Annotate annotate ->
            Sub.map GotAnnotateMsg (Annotate.subscriptions annotate)

        Gallery _ ->
            Ports.setImages (SetImages << valueToImages)


updateWith : (subModel -> App) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toApp toMsg model ( subModel, subCmd ) =
    ( { model | app = toApp subModel }
    , Cmd.map toMsg subCmd
    )


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
