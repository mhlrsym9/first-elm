module EditExisting exposing (init, Msg, update)

import Api
import Data.Project as Project
import Edit
import Html exposing (Html)
import Http exposing (stringResolver)
import Loading
import ProjectAccess exposing (ProjectAccess)
import Task exposing (Task)
import Url.Builder as Builder

-- MODEL

type alias Model =
    Edit.Model

init : ProjectAccess -> (Model, Cmd Msg)
init ( { flags, kcc, key, lcc, pn } as initValues ) =
    let
        ( editModel, editMsg ) =
            Edit.init
                { flags = flags
                , kcc = kcc
                , key = key
                , lcc = lcc
                , pn = pn
                , model = Api.Loading
                }
    in
    ( editModel
    , Cmd.batch
        [ Cmd.map EditMsg editMsg
        , (fetchProject initValues)
            |> Task.attempt CompletedProjectLoad
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )

fetchProject : ProjectAccess -> Task Http.Error Project.Model
fetchProject { flags, kcc, lcc, pn } =
    let
        url = Builder.relative [flags.candorUrl, "read", kcc, lcc, pn] []
    in
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = stringResolver ( Api.handleJsonResponse ( Project.projectDecoder flags.setupEditorName ) )
        , timeout = Nothing
        }

-- UPDATE

type Msg =
    PassedSlowLoadThreshold
    | CompletedProjectLoad (Result Http.Error Project.Model)
    | EditMsg Edit.Msg

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
                    in
                    ( { model | project = Edit.Clean (Api.Loaded updatedProject) }
                    , Cmd.none
                    )
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
                updatedProjectModel =
                    case model.project of
                        Edit.Clean Api.Loading ->
                            Api.LoadingSlowly

                        Edit.Clean pm ->
                            pm

                        Edit.Dirty pm ->
                            pm
            in
            ( { model | project = Edit.Clean updatedProjectModel }
            , Cmd.none )

view : Model -> Html Msg
view model =
    Edit.view model
        |> Html.map EditMsg
