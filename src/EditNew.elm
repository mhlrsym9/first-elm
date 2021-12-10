module EditNew exposing (Model, Msg, init, update, view)

import Api
import Data.Project as Project
import Edit
import Element exposing (Element)
import Flags exposing (Flags)
import Http exposing (jsonBody, stringResolver)
import Json.Decode exposing (Decoder, succeed, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Loading
import ProjectAccess exposing (ProjectAccess)
import Task exposing (Task)
import Url.Builder as Builder

-- MODEL

type alias Model =
    Edit.Model

type alias CreateResult =
    { id : String }

init : ProjectAccess -> (Model, Cmd Msg)
init { flags, key, kl, ll, pn } =
    let
        projectInitParams =
            { flags = flags
            , knownLanguage = kl
            , learningLanguage = ll
            , projectName = pn
            }
        projectModel = Project.initNewProject projectInitParams
        editModel = Edit.init
            { flags = flags
            , key = key
            , kl = kl
            , ll = ll
            , model = Api.Loading projectModel
            , pn = pn
            }
    in
    ( editModel
    , Cmd.batch
        [ Edit.encodeProject kl ll pn projectModel
            |> createProject flags.candorUrl
            |> Task.attempt CompletedProjectCreate
        , Task.perform (\_ -> PassedSlowCreateThreshold) Loading.slowThreshold
        ]
    )

-- UPDATE

type Msg
    = CompletedProjectCreate (Result Http.Error CreateResult)
    | EditMsg Edit.Msg
    | PassedSlowCreateThreshold

createProjectDecoder : Decoder CreateResult
createProjectDecoder =
    succeed CreateResult
        |> required "id" string

createProject : String -> Encode.Value -> Task Http.Error CreateResult
createProject candorUrl json =
    let
        url = Builder.relative [candorUrl, "create"] []
    in
    Http.task
        { method = "POST"
        , headers = []
        , url = url
        , body = jsonBody json
        , resolver = stringResolver (Api.handleJsonResponse createProjectDecoder)
        , timeout = Nothing
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { project } as model ) =
    case msg of
        CompletedProjectCreate result ->
            case result of
                Ok _ ->
                    case project of
                        Edit.Clean (Api.Loading projectModel) ->
                            ( { model | project = Edit.Clean (Api.Loaded projectModel) }
                            , Cmd.none )

                        Edit.Clean (Api.LoadingSlowly projectModel) ->
                            ( { model | project = Edit.Clean (Api.Loaded projectModel) }
                            , Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( { model | project = Edit.Clean Api.Failed }
                    , Cmd.none
                    )

        EditMsg editMsg ->
            let
                (updatedEditModel, editCommands) =
                    Edit.update editMsg model
            in
            ( updatedEditModel
            , Cmd.map EditMsg editCommands
            )

        PassedSlowCreateThreshold ->
            case project of
                Edit.Clean (Api.Loading projectModel) ->
                    ( { model | project = Edit.Clean (Api.LoadingSlowly projectModel) }
                    , Cmd.none )

                _ ->
                    ( model, Cmd.none )


view : Model -> Element Msg
view model =
    Edit.view model
        |> Element.map EditMsg
