module Edit exposing (getProjectModel, Model, Modified(..), Msg(..), init, initNewProject, processDirtyMessage, storeSlideContents, update, urlPath, view)

import Api
import Browser.Navigation as Navigation
import Data.Project as Project
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Http exposing (jsonBody, stringResolver)
import Json.Decode exposing (Decoder, succeed, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Loading
import Routes
import Task exposing (Task)
import Url.Builder as Builder

-- MODEL

type Modified a =
    Clean a
    | Dirty a

type alias Model =
    { project : Modified (Api.Status Project.Model)
    , knownContentCode : String
    , learningContentCode : String
    , projectName : String
    , navigationKey : Navigation.Key
    }

type alias SaveResult =
    { id : String }

init : { key : Navigation.Key, kcc : String, lcc : String, pn : String, model : Api.Status Project.Model } -> (Model, Cmd Msg)
init { key, kcc, lcc, pn, model } =
    (
        { project = Clean model
        , knownContentCode = kcc
        , learningContentCode = lcc
        , projectName = pn
        , navigationKey = key
        }
        , Cmd.none
    )

initNewProject : String -> (Project.Model, Cmd Msg)
initNewProject sen =
    let
        (projectModel, projectCommands) =
            Project.init sen
    in
    ( projectModel, Cmd.map ProjectMsg projectCommands)

urlPath : String
urlPath =
    "http://192.168.34.9:8080"

getProjectModel : Model -> Maybe Project.Model
getProjectModel model =
    case model.project of
        Clean a ->
            case a of
                Api.Loaded b ->
                    Just b
                _ ->
                    Nothing

        Dirty a ->
            case a of
                Api.Loaded b ->
                    Just b
                _ ->
                    Nothing

-- UPDATE

type Msg =
    Cancel
    | CompletedProjectSave (Result Http.Error SaveResult)
    | PassedSlowSaveThreshold
    | ProjectMsg Project.Msg
    | Save
    | UpdateCurrentSlideContents Msg

saveProjectDecoder : Decoder SaveResult
saveProjectDecoder =
    succeed SaveResult
        |> required "is" string

encodeProject : String -> String -> String -> Project.Model -> Encode.Value
encodeProject knownContentCode learningContentCode projectName project =
    Encode.object
        [ ( "l1", Encode.string knownContentCode )
        , ( "l2", Encode.string learningContentCode )
        , ( "project", Encode.string projectName )
        , ( "slides", Project.encodeProject project )
        ]

saveProject : Encode.Value -> Task Http.Error SaveResult
saveProject json =
    let
        url = Builder.relative [urlPath, "update"] []
    in
    Http.task
        { method = "POST"
        , headers = []
        , url = url
        , body = jsonBody json
        , resolver = stringResolver (Api.handleJsonResponse saveProjectDecoder)
        , timeout = Nothing
        }

processDirtyMessage : Model -> Bool -> (Model, Cmd Msg)
processDirtyMessage ( { project } as model ) isDirty =
    case isDirty of
        True ->
            case project of
                Clean p ->
                    ( { model | project = Dirty p }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        False ->
            case project of
                Dirty p ->
                    ( { model | project = Clean p }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

storeSlideContents : Project.Msg -> String -> Model -> (Model, Cmd Msg)
storeSlideContents msg slideContents ( { project } as model ) =
    case project of
        Dirty (Api.Loaded projectModel) ->
            let
                (updatedProject, projectCmd) =
                    Project.storeSlideContents msg slideContents projectModel
            in
            ( { model | project = Dirty (Api.Loaded updatedProject) }
            , Cmd.map ProjectMsg projectCmd
            )

        Clean (Api.Loaded projectModel) ->
            let
                (updatedProject, projectCmd) =
                    Project.storeSlideContents msg slideContents projectModel
            in
            ( { model | project = Clean (Api.Loaded updatedProject) }
            , Cmd.map ProjectMsg projectCmd
            )

        _ ->
            ( model, Cmd.none )

update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { knownContentCode, learningContentCode, projectName, project } as model ) =
    case msg of
        Cancel ->
            ( model, Navigation.pushUrl model.navigationKey (Routes.routeToUrl Routes.Home) )

        CompletedProjectSave _ ->
            case project of
                Dirty p ->
                    ( { model | project = Clean p }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PassedSlowSaveThreshold ->
            ( model, Cmd.none )

        ProjectMsg projectMsg ->
            case project of
                Clean (Api.Loaded projectModel) ->
                    let
                        ( updatedProjectModel, projectCommands ) =
                            Project.update projectMsg projectModel
                    in
                    ( { model | project = Dirty (Api.Loaded updatedProjectModel) }
                    , Cmd.map ProjectMsg projectCommands
                    )

                Dirty (Api.Loaded projectModel) ->
                    let
                        ( updatedProjectModel, projectCommands ) =
                            Project.update projectMsg projectModel
                    in
                    ( { model | project = Dirty (Api.Loaded updatedProjectModel) }
                    , Cmd.map ProjectMsg projectCommands
                    )

                _ ->
                    ( model, Cmd.none )

        Save ->
            case project of
                Dirty (Api.Loaded projectModel) ->
                    ( model
                    , Cmd.batch
                        [ encodeProject knownContentCode learningContentCode projectName projectModel
                            |> saveProject
                            |> Task.attempt CompletedProjectSave
                        , Task.perform (\_ -> PassedSlowSaveThreshold) Loading.slowThreshold
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateCurrentSlideContents _ ->
            ( model, Cmd.none )

viewEditPageInfo : Model -> Html Msg
viewEditPageInfo { knownContentCode, learningContentCode, projectName } =
    div
        [ class "edit-page-info"]
        [
            div
                [ class "edit-page-language-info" ]
                [
                    div
                        [ ]
                        [ text ( "Known Language Code: " ++ knownContentCode ) ]
                    ,
                    div
                        [ ]
                        [ text ( "Learning Language Code: " ++ learningContentCode ) ]
                ]
            ,
            h1
                [ class "edit-page-project-name" ]
                [ text ( "Project Name: " ++ projectName ) ]
        ]

viewSaveButton : Model -> Html Msg
viewSaveButton { project } =
    let
        disabledState =
            case project of
                Dirty _ ->
                    False

                _ ->
                    True
    in
    button
        [ onClick Save
        , disabled disabledState ]
        [ text "Save Project" ]

viewActionButtons : Model -> Html Msg
viewActionButtons model =
    div
        [ class "edit-page-action-buttons" ]
        [ viewSaveButton model
        , button
            [ onClick Cancel ]
            [ text "Cancel" ]
        ]

loadedView : Model -> Project.Model -> Html Msg
loadedView model projectModel =
    div
        [ class "edit-page" ]
        [ viewEditPageInfo model
        , viewActionButtons model
        , Project.view projectModel
            |> Html.map ProjectMsg
        ]

view : Model -> Html Msg
view ( {  project } as model ) =
    case project of
        Clean (Api.Loaded projectModel) ->
            loadedView model projectModel

        Dirty (Api.Loaded projectModel) ->
            loadedView model projectModel

        _ ->
            div
                [ ]
                [ ]


