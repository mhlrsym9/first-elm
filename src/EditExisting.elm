module EditExisting exposing (Init, init, Msg, update)

import Api
import Browser.Navigation as Navigation
import Data.Project as Project
import Edit
import Html exposing (Html)
import Http exposing (stringResolver)
import Loading
import Task exposing (Task)
import Url.Builder as Builder

-- MODEL

type alias Model =
    Edit.Model

type alias Init =
    { key : Navigation.Key
    , kcc : String
    , lcc : String
    , pn : String
    , sen : String
    , candorUrl : String
    }

init : Init -> (Model, Cmd Msg)
init ( { key, kcc, lcc, pn, sen, candorUrl } as flags ) =
    let
        ( editModel, editMsg ) =
            Edit.init { key = key, kcc = kcc, lcc = lcc, pn = pn, candorUrl = candorUrl, model = Api.Loading }
    in
    ( editModel
    , Cmd.batch
        [ Cmd.map EditMsg editMsg
        , (fetchProject flags)
            |> Task.attempt CompletedProjectLoad
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )

fetchProject : Init -> Task Http.Error Project.Model
fetchProject { candorUrl, kcc, lcc, sen, pn } =
    let
        url = Builder.relative [candorUrl, "read", kcc, lcc, pn] []
    in
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = stringResolver ( Api.handleJsonResponse ( Project.projectDecoder sen ) )
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

        CompletedProjectLoad result ->
            case result of
                Ok project ->
                    let
                        updatedProject = Project.establishIndexes project
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

view : Model -> Html Msg
view model =
    Edit.view model
        |> Html.map EditMsg
