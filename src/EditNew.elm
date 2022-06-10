module EditNew exposing (Model, Msg(..), init, update, view)

import Api
import Edit
import Element exposing (Element)
import Flags exposing (Flags)
import Http exposing (jsonBody, stringResolver)
import Json.Decode exposing (bool, Decoder, succeed, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Loading
import MessageHelpers exposing (sendCommandMessage)
import ProjectAccess exposing (ProjectAccess)
import Task exposing (Task)
import Url.Builder as Builder
import UUID exposing (Seeds)

-- MODEL

type alias Model =
    Edit.Model

type alias CreateResult =
    { id : String
    , doesAlreadyExist: Bool
    }

init : ProjectAccess -> (Model, Cmd Msg)
init { flags, key, kl, ll, pn } =
    let
        projectInitParams =
            { flags = flags
            , knownLanguage = kl
            , learningLanguage = ll
            , projectName = pn
            }
        ( projectModel, updateSeedCommand ) = Edit.initNewProject projectInitParams

        editInitParams =
            { flags = projectModel.initParams.flags
            , key = key
            , kl = kl
            , ll = ll
            , model = Api.Loading projectModel
            , pn = pn
            }
        editModel = Edit.init editInitParams
    in
    ( editModel
    , Cmd.batch
        [ Cmd.map EditMsg updateSeedCommand
        , Edit.encodeProject kl ll pn projectModel
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
    | UpdateSeeds Seeds

createProjectDecoder : Decoder CreateResult
createProjectDecoder =
    succeed CreateResult
        |> required "id" string
        |> required "doesAlreadyExist" bool

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
                Ok { doesAlreadyExist } ->
                    case doesAlreadyExist of
                        False ->
                            case project of
                                Edit.Clean (Api.Loading projectModel) ->
                                    ( { model | project = Edit.Clean (Api.Loaded projectModel) }
                                    , Cmd.none )

                                Edit.Clean (Api.LoadingSlowly projectModel) ->
                                    ( { model | project = Edit.Clean (Api.Loaded projectModel) }
                                    , Cmd.none )

                                _ ->
                                    ( model, Cmd.none )

                        True ->
                            ( { model | project = Edit.Clean Api.Failed, doesAlreadyExist = True }
                            , Cmd.none
                            )

                Err _ ->
                    ( { model | project = Edit.Clean Api.Failed }
                    , Cmd.none
                    )

        EditMsg editMsg ->
            case editMsg of
                Edit.UpdateSeeds seeds ->
                    ( model, sendCommandMessage (UpdateSeeds seeds) )

                _ ->
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

        -- Handled by parent
        UpdateSeeds _ ->
            ( model, Cmd.none )

view : Model -> Element Msg
view model =
    Edit.view model
        |> Element.map EditMsg
