port module Main exposing (..)

import Api
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Create
import Delete
import Dialog exposing (Config)
import Edit
import EditExisting
import EditNew
import Element exposing (centerX, column, Element, inFront, layout, padding, spacing)
import Element.Font as Font
import Flags exposing (Flags)
import Generate
import GenerateAlphabet
import GenerateCourseWare
import Http exposing (Response, stringResolver)
import Json.Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import LanguageHelpers
import Loading
import Open
import Procedure
import Procedure.Channel as Channel
import Procedure.Program
import Routes
import Start
import Task exposing (Task)
import Url exposing (Url)
import Url.Builder as Builder
import UUID exposing (Seeds)

---- PORTS ----

port syncMceEditor : () -> Cmd msg
port updateDirtyFlag : Bool -> Cmd msg
port consoleLog : String -> Cmd msg

port dirtyReceived : (Bool -> msg) -> Sub msg
port mceEditorSubscription : (String -> msg) -> Sub msg

---- PROCEDURES ----

syncMceEditorProcedure : Msg -> Cmd Msg
syncMceEditorProcedure nextMsg =
    Channel.open (\_ -> syncMceEditor () )
        |> Channel.connect mceEditorSubscription
        |> Channel.acceptOne
        |> Procedure.run ProcMsg (ReceivedMceEditorMsg nextMsg)

---- SUBSCRIPTIONS ----

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dirtyReceived Dirty
        , Procedure.Program.subscriptions model.procModel
        ]

---- MODEL ----

type Page
    = Create Create.Model
    | Delete Delete.Model
    | Edit Edit.Model
    | Generate Generate.Model
    | Open Open.Model
    | NotFound
    | Start Start.Model

type DataError
    = NoLanguages

type Error
    = HttpError Http.Error
    | DataError DataError

type alias Version =
    { version : String }

type alias Model =
    { dialog : Maybe (Config Msg)
    , flags : Flags.Model
    , languages : Api.Status LanguageHelpers.Model
    , navigationKey : Navigation.Key
    , page : Page
    , procModel : (Procedure.Program.Model Msg)
    , serverVersion : Api.Status Version
    }

initialModel : Navigation.Key -> Flags -> Model
initialModel navigationKey flags =
    { dialog = Nothing
    , flags = Flags.init flags
    , languages = Api.Loading LanguageHelpers.initEmptyLanguageHelpers
    , navigationKey = navigationKey
    , page = NotFound
    , procModel = Procedure.Program.init
    , serverVersion = Api.Loading { version = "" }
    }

versionDecoder : Decoder Version
versionDecoder =
    Json.Decode.succeed Version
        |> required "version" string

fetchVersion : Flags -> Task Http.Error Version
fetchVersion flags =
    let
        url = Builder.relative [flags.candorUrl, "version"] []
    in
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = stringResolver (Api.handleJsonResponse versionDecoder)
        , timeout = Nothing
        }

fetchPreliminaryInfo : Flags -> Task Error ( LanguageHelpers.Model, Version )
fetchPreliminaryInfo flags =
    LanguageHelpers.fetchLanguages
        |> Task.mapError HttpError
        |> Task.andThen
            (\languages ->
                case languages of
                    [] ->
                        Task.fail (DataError NoLanguages)

                    _ ->
                        fetchVersion flags
                            |> Task.mapError HttpError
                            |> Task.map (\v -> ((LanguageHelpers.init languages), v))
            )

init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url navigationKey =
    let
        -- Make sure path is empty; Tomcat sends context path which is not handled in the Route matchings.
        (model, cmd) =
            setNewPage (Routes.match { url | path = "/" }) (initialModel navigationKey flags)
    in
    ( model
    , Cmd.batch
        [ cmd
        , Task.attempt CompletedPreliminaryLoad (fetchPreliminaryInfo flags)
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )

---- UPDATE ----

type Msg
    = CompletedPreliminaryLoad ( Result Error ( LanguageHelpers.Model, Version ) )
    | ConsoleOut String
    | CreateMsg Create.Msg
    | DeleteMsg Delete.Msg
    | Dirty Bool
    | EditExistingMsg EditExisting.Msg
    | EditMsg Edit.Msg
    | EditNewMsg EditNew.Msg
    | GenerateMsg Generate.Msg
    | NewRoute (Maybe Routes.Route)
    | OpenMsg Open.Msg
    | PassedSlowLoadThreshold
    | ProcMsg (Procedure.Program.Msg Msg)
    | ReceivedMceEditorMsg Msg String
    | StartMsg Start.Msg
    | Visit UrlRequest

setNewPage : Maybe Routes.Route -> Model -> ( Model, Cmd Msg )
setNewPage maybeRoute ( { navigationKey, flags, languages } as model ) =
    let
        languagesModel =
            case languages of
                Api.Loaded ls ->
                    ls

                _ ->
                    LanguageHelpers.init []
    in
    case maybeRoute of
        Just Routes.Create ->
            let
                createModel = Create.init navigationKey languagesModel
            in
            ( { model | page = Create createModel }
            , Cmd.none
            )

        Just (Routes.Delete k l p) ->
            case p of
                Just projectName ->
                    let
                        ( deleteModel, deleteCommand ) =
                            Delete.init
                                { flags = flags
                                , kl = LanguageHelpers.languageFromContentCode languagesModel k
                                , key = navigationKey
                                , ll = LanguageHelpers.languageFromContentCode languagesModel l
                                , pn =  projectName
                                }
                    in
                    ( { model | page = Delete deleteModel }
                    , Cmd.map DeleteMsg deleteCommand )

                Nothing ->
                    ( model, Cmd.none )

        Just (Routes.EditNew k l p) ->
            case p of
                Just projectName ->
                    let
                        ( editModel, editCmd ) =
                            EditNew.init
                                { flags = flags
                                , kl = LanguageHelpers.languageFromContentCode languagesModel k
                                , key = navigationKey
                                , ll = LanguageHelpers.languageFromContentCode languagesModel l
                                , pn =  projectName
                                }
                    in
                    ( { model | page = Edit editModel }
                    , Cmd.map EditNewMsg editCmd )

                Nothing ->
                    ( model, Cmd.none )

        Just (Routes.EditExisting k l p) ->
            case p of
                Just projectName ->
                    let
                        ( editModel, editCmd ) =
                            EditExisting.init
                                { flags = flags
                                , key = navigationKey
                                , kl = LanguageHelpers.languageFromContentCode languagesModel k
                                , ll = LanguageHelpers.languageFromContentCode languagesModel l
                                , pn = projectName
                                }
                    in
                    ( { model | page = Edit editModel }
                    , Cmd.map EditExistingMsg editCmd )

                Nothing ->
                    ( model, Cmd.none )

        Just (Routes.GenerateAlphabet k l p) ->
            case p of
                Just projectName ->
                    let
                        ( generateModel, generateCmd ) =
                            GenerateAlphabet.init
                                { flags = flags
                                , key = navigationKey
                                , kl = LanguageHelpers.languageFromContentCode languagesModel k
                                , ll = LanguageHelpers.languageFromContentCode languagesModel l
                                , pn = projectName
                                }
                    in
                    ( { model | page = Generate generateModel }
                    , Cmd.map GenerateMsg generateCmd )

                Nothing ->
                    ( model, Cmd.none )

        Just (Routes.GenerateCourseWare k l p) ->
            case p of
                Just projectName ->
                    let
                        ( generateModel, generateCmd ) =
                            GenerateCourseWare.init
                                { flags = flags
                                , key = navigationKey
                                , kl = LanguageHelpers.languageFromContentCode languagesModel k
                                , ll = LanguageHelpers.languageFromContentCode languagesModel l
                                , pn = projectName
                                }
                    in
                    ( { model | page = Generate generateModel }
                    , Cmd.map GenerateMsg generateCmd )

                Nothing ->
                    ( model, Cmd.none )

        Just Routes.Home ->
            let
                startModel = Start.init navigationKey
            in
            ( { model | page = Start startModel }
            , Cmd.none )

        Just Routes.Open ->
            let
                ( openModel, openCmd ) =
                    Open.init navigationKey flags languagesModel
            in
            ( { model | page = Open openModel }
            , Cmd.map OpenMsg openCmd )

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

updateEdit : Edit.Msg -> Edit.Model -> Model -> ( Model, Cmd Msg )
updateEdit editMsg editModel model =
    let
        ( updatedEditModel, editCmd ) =
            Edit.update editMsg editModel
    in
    ( { model | page = Edit updatedEditModel }
    , Cmd.map EditMsg editCmd
    )

updateSeeds : Model -> Edit.Model -> Seeds -> ( Model, Cmd Msg )
updateSeeds ( { flags } as model ) editModel updatedSeeds =
    let
        updatedFlags = Flags.updateSeeds flags updatedSeeds
        updatedEditModel = Edit.updateSeeds editModel updatedSeeds
    in
    ( { model | flags = updatedFlags, page = Edit updatedEditModel }
    , Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( { flags } as model ) =
    case ( msg, model.page ) of
        ( CompletedPreliminaryLoad result, _ ) ->
            case result of
                Ok (languagesModel, version) ->
                    ( { model | languages = Api.Loaded languagesModel, serverVersion = Api.Loaded version }, Cmd.none )

                Err _ ->
                    ( { model | languages = Api.Failed, serverVersion = Api.Failed }, Cmd.none )

        ( ConsoleOut urlStr, _ ) ->
            ( model, consoleLog urlStr )

        ( CreateMsg createMsg, Create createModel ) ->
            let
                ( updatedCreateModel, createCmd ) =
                    Create.update createMsg createModel
            in
            ( { model | page = Create updatedCreateModel }
            , Cmd.map CreateMsg createCmd
            )

        ( DeleteMsg deleteMsg, Delete deleteModel ) ->
            let
                ( updatedDeleteModel, deleteCmd ) =
                    Delete.update deleteMsg deleteModel
            in
            ( { model | page = Delete updatedDeleteModel }
            , Cmd.map DeleteMsg deleteCmd
            )

        ( DeleteMsg _, _ ) ->
            ( model, Cmd.none )

        ( CreateMsg _, _ ) ->
--            Debug.todo "Handle CreateMsg error case"
            ( model, Cmd.none )

        ( Dirty isDirty, Edit editModel ) ->
            let
                updatedEditModel =
                    Edit.processDirtyMessage editModel isDirty
            in
                ( { model | page = Edit updatedEditModel }
                , Cmd.none
                )

        ( Dirty _, _ ) ->
--            Debug.todo "Handle Dirty"
            ( model, Cmd.none )

        ( EditExistingMsg editExistingMsg, Edit editModel ) ->
            case editExistingMsg of
                EditExisting.UpdateSeeds updatedSeeds ->
                    updateSeeds model editModel updatedSeeds

                _ ->
                    let
                        ( updatedEditModel, editCmd ) =
                            EditExisting.update editExistingMsg editModel
                    in
                    ( { model | page = Edit updatedEditModel }
                    , Cmd.map EditExistingMsg editCmd
                    )

        ( EditExistingMsg _, _ ) ->
--            Debug.todo "Handle EditExistingMsg error case"
            ( model, Cmd.none )

        ( EditMsg editMsg, Edit editModel ) ->
            case editMsg of
                Edit.ShowDialog config ->
                    case config of
                        Just c ->
                            ( { model | dialog = Just (Dialog.map EditMsg c) }, Cmd.none )

                        Nothing ->
                            ( { model | dialog = Nothing }, Cmd.none )

                Edit.UpdateCurrentSlideContents nextMsg ->
                    ( model, syncMceEditorProcedure (EditMsg nextMsg) )

                Edit.UpdateDirtyFlag flag ->
                    ( model, updateDirtyFlag flag )

                Edit.UpdateSeeds updatedSeeds ->
                    updateSeeds model editModel updatedSeeds

                _ ->
                    updateEdit editMsg editModel model

        ( EditMsg _, _ ) ->
--            Debug.todo "Handle EditMsg error case"
            ( model, Cmd.none )

        ( EditNewMsg editMsg, Edit editModel ) ->
            case editMsg of
                EditNew.UpdateSeeds seeds ->
                    updateSeeds model editModel seeds

                _ ->
                    let
                        ( updatedEditModel, editCmd ) =
                            EditNew.update editMsg editModel
                    in
                    ( { model | page = Edit updatedEditModel }
                    , Cmd.map EditNewMsg editCmd
                    )

        ( EditNewMsg _, _ ) ->
--            Debug.todo "Handle EditNewMsg error case"
            ( model, Cmd.none )

        ( GenerateMsg generateMsg, Generate generateModel ) ->
            let
                ( updatedGenerateModel, generateCommands ) =
                    Generate.update generateMsg generateModel
            in
            ( { model | page = Generate updatedGenerateModel }
            , Cmd.map GenerateMsg generateCommands
            )

        ( GenerateMsg _, _ ) ->
            ( model, Cmd.none )

        ( NewRoute maybeRoute, _ ) ->
            setNewPage maybeRoute model

        ( OpenMsg openMsg, Open openModel ) ->
            let
                ( updatedOpenModel, openCmd ) =
                    Open.update openMsg openModel
            in
            ( { model | page = Open updatedOpenModel }
            , Cmd.map OpenMsg openCmd
            )

        ( OpenMsg _, _ ) ->
--            Debug.todo "Handle OpenMsg error case"
            ( model, Cmd.none )

        ( PassedSlowLoadThreshold, _ ) ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                updatedLanguages =
                    case model.languages of
                        Api.Loading ls ->
                            Api.LoadingSlowly ls

                        other ->
                            other

                updatedServerVersion =
                    case model.serverVersion of
                        Api.Loading sv ->
                            Api.LoadingSlowly sv

                        other ->
                            other

            in
            ( { model | languages = updatedLanguages, serverVersion = updatedServerVersion }, Cmd.none )

        ( ProcMsg pMsg, _ ) ->
            Procedure.Program.update pMsg model.procModel
                |> Tuple.mapFirst (\updated -> { model | procModel = updated })

        ( ReceivedMceEditorMsg nextMsg slideContents, Edit editModel ) ->
            let
                updatedEditModel =
                    Edit.storeSlideContents slideContents editModel
            in
            update nextMsg { model | page = Edit updatedEditModel }

        ( ReceivedMceEditorMsg _ _, _ ) ->
--            Debug.todo "Handle ReceivedMceEditorMessage error case"
            ( model, Cmd.none )

        ( StartMsg startMsg, Start startModel ) ->
            let
                ( updatedStartModel, startCmd ) =
                    Start.update startMsg startModel
            in
            ( { model | page = Start updatedStartModel }
            , Cmd.map StartMsg startCmd
            )

        ( StartMsg _, _ ) ->
--            Debug.todo "Handle StartMsg error case"
            ( model, Cmd.none )

        ( Visit _, _ ) ->
--            Debug.todo "Handle Visit"
            ( model, Cmd.none )

---- VIEW ---

viewLoading : Flags.Model -> Element Msg
viewLoading flags =
    Element.el
        [ centerX ]
        (Loading.iconElement flags.loadingPath)

viewVersion : Model -> Element Msg
viewVersion { flags, serverVersion } =
    let
        theVersion =
            case serverVersion of
                Api.Loaded v ->
                    v.version

                _ ->
                    "Candor V2 Server Version <Unknown> "
    in
    Element.el
        [ centerX ]
        ( Element.text ( theVersion ++ " Elm Client " ++ Flags.versionString flags ) )

viewHeader : Model -> Element Msg
viewHeader ( { page } as model ) =
    column
        [ spacing 10
        , centerX
        ]
        [
            case page of
                Create createModel ->
                    Create.view createModel
                        |> Element.map CreateMsg

                Delete deleteModel ->
                    Delete.view deleteModel
                        |> Element.map DeleteMsg

                Edit editModel ->
                    Edit.view editModel
                        |> Element.map EditMsg

                Generate generateModel ->
                    Generate.view generateModel
                        |> Element.map GenerateMsg

                Open openModel ->
                    Open.view openModel
                        |> Element.map OpenMsg

                NotFound ->
                    Element.el
                        [ Font.size 30
                        , Font.bold
                        ]
                        ( Element.text "Page Not Found" )

                Start startModel ->
                    Start.view startModel
                        |> Element.map StartMsg
            , viewVersion model
        ]

viewContent : Model -> ( String, Element Msg )
viewContent ( { flags, languages, serverVersion } as model ) =
    let
        contents =
            case (languages, serverVersion) of
                (Api.Loading _, _) ->
                    Element.none

                (_, Api.Loading _) ->
                    Element.none

                (Api.LoadingSlowly _, _) ->
                    viewLoading flags

                (_, Api.LoadingSlowly _) ->
                    viewLoading flags

                (Api.Failed, _) ->
                    Loading.error "languages"

                (_, Api.Failed) ->
                    Loading.error "server version"

                (Api.Loaded _, Api.Loaded _) ->
                    viewHeader model

    in
    ( "Candor HTML", contents )

view : Model -> Document Msg
view ( { dialog } as model ) =
    let
        ( title, content ) =
            viewContent model
    in
    { title = title
    , body =
        [
            layout
                [ Font.size 14
                , padding 5
                , inFront (Dialog.view dialog)
                ]
                content
        ]
    }

---- PROGRAM ----

main : Program Flags Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = Visit
        , onUrlChange = Routes.match >> NewRoute
        }
