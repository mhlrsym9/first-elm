module EditExisting exposing (init, Msg, update)

import Api
import Data.Project as Project
import Edit
import Element exposing (Element)
import Http exposing (stringResolver)
import LanguageHelpers
import Loading
import ProjectAccess exposing (ProjectAccess)
import Task exposing (Task)
import Url.Builder as Builder

-- MODEL

type alias Model =
    Edit.Model

init : ProjectAccess -> (Model, Cmd Msg)
init ( { flags, kl, key, ll, pn } as initValues ) =
    let
        projectInitParams =
            { flags = flags
            , knownLanguage = kl
            , learningLanguage = ll
            , projectName = pn
            }
        editModel =
            Edit.init
                { flags = flags
                , kl = kl
                , key = key
                , ll = ll
                , pn = pn
                , model = Api.Loading (Project.initEmptyProject projectInitParams)
                }
    in
    ( editModel
    , Cmd.batch
        [ (fetchProject initValues)
            |> Task.attempt CompletedProjectLoad
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )

fetchProject : ProjectAccess -> Task Http.Error Project.Model
fetchProject { flags, kl, ll, pn } =
    let
        kcc = LanguageHelpers.contentCodeStringFromLanguage kl
        lcc = LanguageHelpers.contentCodeStringFromLanguage ll
        url = Builder.relative [flags.candorUrl, "read", kcc, lcc, pn] []
        projectInitParams =
            { flags = flags
            , knownLanguage = kl
            , learningLanguage = ll
            , projectName = pn
            }
    in
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = stringResolver ( Api.handleJsonResponse ( Project.projectDecoder projectInitParams ) )
        , timeout = Nothing
        }

-- UPDATE

type Msg =
    CompletedProjectLoad (Result Http.Error Project.Model)
    | EditMsg Edit.Msg
    | PassedSlowLoadThreshold

updateProject : Model -> Model
updateProject ( { project } as model ) =
    let
        originalProject =
            case project of
                Edit.Clean (Api.Loaded p) ->
                    Just p

                Edit.Dirty (Api.Loaded p) ->
                    Just p

                _ ->
                    Nothing

        (updatedProject, dirty) =
            case originalProject of
                Just p ->
                    Project.updateProject p

                Nothing ->
                    (Nothing, False)
    in
    case dirty of
        True ->
            case updatedProject of
                Just p ->
                    { model | project = Edit.Dirty (Api.Loaded p) }

                Nothing ->
                    model

        False ->
            model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CompletedProjectLoad result ->
            case result of
                Ok project ->
                    let
                        updatedProject = project
                            |> Project.establishIndexes
                            |> Project.establishSlideUUIDs
                        updatedModel = updateProject { model | project = Edit.Clean (Api.Loaded updatedProject) }
                    in
                    ( updatedModel, Cmd.none )

                Err _ ->
                    ( { model | project = Edit.Clean Api.Failed }
                    , Cmd.none
                    )

        EditMsg editMsg ->
            let
                ( updatedModel, updatedCmd ) =
                    Edit.update editMsg model
            in
            ( updatedModel
            , Cmd.map EditMsg updatedCmd
            )

        PassedSlowLoadThreshold ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                updatedModel =
                    case model.project of
                        Edit.Clean (Api.Loading p) ->
                            { model | project = Edit.Clean (Api.LoadingSlowly p) }

                        Edit.Dirty (Api.Loading p) ->
                            { model | project = Edit.Dirty (Api.LoadingSlowly p) }

                        Edit.Clean _ ->
                            model

                        Edit.Dirty _ ->
                            model
            in
            ( updatedModel , Cmd.none )

view : Model -> Element Msg
view model =
    Edit.view model
        |> Element.map EditMsg
