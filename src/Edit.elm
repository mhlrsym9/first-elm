module Edit exposing (encodeProject, establishProject, Model, Modified(..), Msg(..), init, initNewProject, processDirtySlideTextMessage, storeSlideContents, update, updateSeeds, view)

import Api
import Browser.Navigation as Navigation
import Data.Project as Project
import Dialog exposing (Config)
import Element exposing (centerX, column, Element, el, inFront, padding, row, spacing, spacingXY)
import Element.Font as Font
import Element.Input as Input
import Flags exposing (Flags)
import Http exposing (jsonBody, stringResolver)
import Json.Decode exposing (Decoder, succeed, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import LanguageHelpers
import Loading
import MessageHelpers exposing (sendCommandMessage)
import Routes
import Task exposing (Task)
import UIHelpers exposing (buttonAttributes)
import Url.Builder as Builder
import UUID exposing (Seeds)

-- MODEL

type Modified a =
    Clean a
    | Dirty a

type alias Model =
    { doesAlreadyExist : Bool
    , flags : Flags.Model
    , knownLanguage : LanguageHelpers.Language
    , learningLanguage : LanguageHelpers.Language
    , navigationKey : Navigation.Key
    , project : Modified (Api.Status Project.Model)
    , projectName : String
    , showHomeScreenSaveWarning : Bool
    }

type alias SaveResult =
    { id : String }

type alias InitParams =
    { flags : Flags.Model
    , kl : LanguageHelpers.Language
    , key : Navigation.Key
    , ll : LanguageHelpers.Language
    , pn : String
    , model : Api.Status Project.Model
    }

init : InitParams -> Model
init { key, kl, ll, pn, model, flags } =
    { doesAlreadyExist = False
    , flags = flags
    , knownLanguage = kl
    , learningLanguage = ll
    , navigationKey = key
    , project = Clean model
    , projectName = pn
    , showHomeScreenSaveWarning = False
    }

initNewProject : Project.InitParams -> ( Project.Model, Cmd Msg )
initNewProject initParams =
    let
        ( projectModel, command ) = Project.initNewProject initParams
    in
    ( projectModel, Cmd.map ProjectMsg command )

establishProject : Project.Model -> (Project.Model, Cmd Msg)
establishProject project =
    let
        (updatedProject, command) = project
            |> Project.establishIndexes
            |> Project.establishSlideUUIDs
    in
    (updatedProject, Cmd.map ProjectMsg command)

updateProjectStatusSeeds : Api.Status Project.Model -> Seeds -> Api.Status Project.Model
updateProjectStatusSeeds sp updatedSeeds =
    case sp of
        Api.Failed ->
            Api.Failed

        Api.Loading p ->
            Api.Loading (Project.updateSeeds p updatedSeeds)

        Api.LoadingSlowly p ->
            Api.LoadingSlowly (Project.updateSeeds p updatedSeeds)

        Api.Loaded p ->
            Api.Loaded (Project.updateSeeds p updatedSeeds)

updateSeeds : Model -> Seeds -> Model
updateSeeds ( { flags, project } as model ) updatedSeeds =
    let
        updatedFlags = Flags.updateSeeds flags updatedSeeds
        updatedProject =
            case project of
                Clean p ->
                    Clean (updateProjectStatusSeeds p updatedSeeds)

                Dirty p ->
                    Dirty (updateProjectStatusSeeds p updatedSeeds)
    in
    { model | flags = updatedFlags, project = updatedProject }

-- UPDATE

type Msg
    = Cancel
    | CloseDialog
    | CompletedProjectSave (Result Http.Error SaveResult)
    | PassedSlowSaveThreshold
    | ProjectMsg Project.Msg
    | Save
    | ShowDialog ( Maybe (Config Msg) )
    | UpdateCurrentSlideContents Msg
    | UpdateDirtySlideTextFlag Bool
    | UpdateSeeds Seeds

saveProjectDecoder : Decoder SaveResult
saveProjectDecoder =
    succeed SaveResult
        |> required "id" string

encodeProject : LanguageHelpers.Language -> LanguageHelpers.Language -> String -> Project.Model -> Encode.Value
encodeProject kl ll projectName project =
    Encode.object
        [ ( "l1", Encode.string ( LanguageHelpers.contentCodeStringFromLanguage kl ) )
        , ( "l2", Encode.string ( LanguageHelpers.contentCodeStringFromLanguage ll ) )
        , ( "project", Encode.string projectName )
        , ( "slides", Project.encodeProject project )
        ]

saveProject : String -> Encode.Value -> Task Http.Error SaveResult
saveProject candorUrl json =
    let
        url = Builder.relative [candorUrl, "update"] []
    in
    Http.task
        { method = "POST"
        , headers = []
        , url = url
        , body = jsonBody json
        , resolver = stringResolver (Api.handleJsonResponse saveProjectDecoder)
        , timeout = Nothing
        }

extractStatusProject : Model -> Api.Status Project.Model
extractStatusProject { project } =
    case project of
        Clean p ->
            p
        Dirty p ->
            p

updateDirtySlideTextFlag : Model -> Bool -> Api.Status Project.Model
updateDirtySlideTextFlag model isDirty =
    let
        statusProject = extractStatusProject model
    in
    case statusProject of
        Api.Failed ->
            statusProject

        Api.Loaded p ->
            Api.Loaded (Project.processDirtySlideTextMessage p isDirty)

        Api.Loading p ->
            Api.Loading (Project.processDirtySlideTextMessage p isDirty)

        Api.LoadingSlowly p ->
            Api.LoadingSlowly (Project.processDirtySlideTextMessage p isDirty)

processDirtySlideTextMessage : Model -> Bool -> Model
processDirtySlideTextMessage ( { project } as model ) isDirty =
    let
        updatedStatusProject = updateDirtySlideTextFlag model isDirty
    in
    case isDirty of
        True ->
            case project of
                Clean _ ->
                    { model | project = Dirty updatedStatusProject }

                Dirty _ ->
                    { model | project = Dirty updatedStatusProject }

        False ->
            case project of
                Clean _ ->
                    { model | project = Clean updatedStatusProject }

-- Once the project is Dirty, the only way to clean it is via a save. The slide text is updated
-- on every slide change and set to clean then but that does not save the project.
                Dirty _ ->
                    { model | project = Dirty updatedStatusProject }

storeSlideContents : String -> Model -> Model
storeSlideContents slideContents ( { project } as model ) =
    case project of
        Dirty (Api.Loaded projectModel) ->
            let
                updatedProject =
                    Project.storeSlideContents slideContents projectModel
            in
            { model | project = Dirty (Api.Loaded updatedProject) }

        Clean (Api.Loaded projectModel) ->
            let
                updatedProject =
                    Project.storeSlideContents slideContents projectModel
            in
            { model | project = Clean (Api.Loaded updatedProject) }

        _ ->
            model

pushCancel : Model -> Cmd Msg
pushCancel model =
    Navigation.pushUrl model.navigationKey (Routes.routeToUrl Routes.Home)

showDialog : Maybe (Config Msg) -> Cmd Msg
showDialog config =
    sendCommandMessage ( ShowDialog config )

update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { knownLanguage, learningLanguage, projectName, project, flags } as model ) =
    case msg of
        Cancel ->
            ( model, pushCancel model )

        CloseDialog ->
            ( model, Cmd.none )

        CompletedProjectSave result ->
            case result of
                Ok _ ->
                    case project of
                        Dirty (Api.Loading pm) ->
                            ( { model | project = Clean (Api.Loaded pm) }, sendCommandMessage (UpdateDirtySlideTextFlag False) )

                        Dirty (Api.LoadingSlowly pm) ->
                            ( { model | project = Clean (Api.Loaded pm) }, sendCommandMessage (UpdateDirtySlideTextFlag False) )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( { model | project = Dirty Api.Failed }
                    , Cmd.none
                    )

        PassedSlowSaveThreshold ->
            case project of
                Dirty (Api.Loading pm) ->
                    ( { model | project = Dirty (Api.LoadingSlowly pm) }
                    , Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ProjectMsg projectMsg ->
            case projectMsg of
                Project.MakeDirty ->
                    case project of
                        Clean p ->
                            ( { model | project = Dirty p }
                            , Cmd.none
                            )

-- If already Dirty, no need to dirty it up again.
                        _ ->
                            ( model, Cmd.none )

                Project.ShowDialog config ->
                    case config of
                        Just c ->
                            ( model, showDialog ( Just (Dialog.map ProjectMsg c) ) )

                        Nothing ->
                            ( model, showDialog Nothing )

                Project.UpdateCurrentSlideContents nextMsg ->
                    ( model, sendCommandMessage (UpdateCurrentSlideContents ( ProjectMsg nextMsg ) ) )

                Project.UpdateSeeds seeds ->
                    ( model, sendCommandMessage (UpdateSeeds seeds) )

                _ ->
                    case project of
                        Clean (Api.Loaded projectModel) ->
                            let
                                ( updatedProjectModel, updatedCommands ) =
                                    Project.update projectMsg projectModel
                            in
                            ( { model | project = Clean (Api.Loaded updatedProjectModel) }
                            , Cmd.map ProjectMsg updatedCommands
                            )

                        Dirty (Api.Loaded projectModel) ->
                            let
                                ( updatedProjectModel, updatedCommands ) =
                                    Project.update projectMsg projectModel
                            in
                            ( { model | project = Dirty (Api.Loaded updatedProjectModel) }
                            , Cmd.map ProjectMsg updatedCommands
                            )

                        _ ->
                            ( model, Cmd.none )

        Save ->
            case project of
                Dirty (Api.Loaded pm) ->
                    ( { model | project = Dirty (Api.Loading pm) }
                    , Cmd.batch
                        [ encodeProject knownLanguage learningLanguage projectName pm
                            |> saveProject flags.candorUrl
                            |> Task.attempt CompletedProjectSave
                        , Task.perform (\_ -> PassedSlowSaveThreshold) Loading.slowThreshold
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

-- Handled by Main module
        ShowDialog _ ->
            ( model, Cmd.none )

        UpdateCurrentSlideContents _ ->
            ( model, Cmd.none )

        UpdateDirtySlideTextFlag _ ->
            ( model, Cmd.none )

        -- Handled by parent
        UpdateSeeds _ ->
            ( model, Cmd.none )

viewEditPageInfo : Model -> Element Msg
viewEditPageInfo { knownLanguage, learningLanguage, projectName } =
    column
        [ centerX
        , spacing 10
        ]
        [ row
            [ spacingXY 200 0
            , centerX
            ]
            [ Element.text ( "Known Language: " ++ knownLanguage.displayName )
            , Element.text ( "Learning Language: " ++ learningLanguage.displayName )
            ]
        , el
            [ Font.size 30
            , Font.bold
            , centerX
            ]
            (Element.text ( "Project Name: " ++ projectName ) )
        ]

viewSaveButton : Model -> Element Msg
viewSaveButton { project } =
    let
        disabledState =
            case project of
                Dirty _ ->
                    False

                _ ->
                    True
    in
    if (disabledState) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = Just (UpdateCurrentSlideContents Save)
            , label = Element.text "Save Project"
            }

viewCancelButton : Element Msg
viewCancelButton =
    Input.button
        (centerX :: buttonAttributes)
        { onPress = Just Cancel
        , label = Element.text "Return to Home Screen" }

viewActionButtons : Model -> Element Msg
viewActionButtons model =
    row
        [ spacing 10
        , centerX
        ]
        [ viewSaveButton model
        , viewCancelButton
        ]

loadedMainView : Model -> Project.Model -> Element Msg
loadedMainView model projectModel =
    column
        [ centerX ]
        [ viewEditPageInfo model
        , viewActionButtons model
        , Project.view projectModel
            |> Element.map ProjectMsg
        ]

loadedView : Model -> Project.Model -> Element Msg
loadedView model projectModel =
    let
        config =
            { closeMessage = Just CloseDialog
            , maskAttributes = []
            , containerAttributes = [ padding 10 ]
            , headerAttributes = []
            , bodyAttributes = []
            , footerAttributes = []
            , header = Just (Element.text "Hello world")
            , body = Nothing
            , footer = Nothing
            }
        dialogConfig =
            if (model.showHomeScreenSaveWarning) then
                Just config
            else
                Nothing
    in
    el
        [ inFront ( Dialog.view dialogConfig ) ]
        (loadedMainView model projectModel)

errorView : Model -> String -> Element Msg
errorView { projectName } errorStr =
    column
        [ spacing 10
        , centerX
        ]
        [ Element.text ("Project " ++ projectName ++ errorStr)
        , viewCancelButton
        ]

loadingSlowlyView : Model -> Element Msg
loadingSlowlyView { flags } =
    Loading.iconElement flags.loadingPath

view : Model -> Element Msg
view ( {  doesAlreadyExist, project, projectName } as model ) =
    let
        element =
            case project of
                Clean Api.Failed ->
                    case doesAlreadyExist of
                        True ->
                            errorView model " already exists."

                        False ->
                            errorView model " could not be created."

                Dirty Api.Failed ->
                    errorView model " could not be saved."

                Clean (Api.Loaded projectModel) ->
                    loadedView model projectModel

                Dirty (Api.Loaded projectModel) ->
                    loadedView model projectModel

                Clean (Api.Loading projectModel) ->
                    loadedView model projectModel

                Dirty (Api.Loading projectModel) ->
                    loadedView model projectModel

                Clean (Api.LoadingSlowly _) ->
                    loadingSlowlyView model

                Dirty (Api.LoadingSlowly projectModel) ->
                    column
                        [ centerX ]
                        [ (loadingSlowlyView model)
                        , (loadedView model projectModel)
                        ]
    in
    el
        [ Font.size 14
        , centerX
        ]
        element
