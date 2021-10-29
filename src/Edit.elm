module Edit exposing (encodeProject, getProjectModel, Model, Modified(..), Msg(..), init, processDirtyMessage, storeSlideContents, update, view)

import Api
import Browser.Navigation as Navigation
import Data.Project as Project
import Flags exposing (Flags)
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
    { flags : Flags
    , knownContentCode : String
    , learningContentCode : String
    , navigationKey : Navigation.Key
    , project : Modified (Api.Status Project.Model)
    , projectName : String
    }

type alias SaveResult =
    { id : String }

type alias Init =
    { flags : Flags
    , kcc : String
    , key : Navigation.Key
    , lcc : String
    , pn : String
    , model : Api.Status Project.Model
    }

init : Init -> (Model, Cmd Msg)
init { key, kcc, lcc, pn, model, flags } =
    (
        { flags = flags
        , knownContentCode = kcc
        , learningContentCode = lcc
        , navigationKey = key
        , project = Clean model
        , projectName = pn
        }
        , Cmd.none
    )

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

type Msg
    = Cancel
    | CompletedProjectSave (Result Http.Error SaveResult)
    | PassedSlowSaveThreshold
    | ProjectMsg Project.Msg
    | Save
    | UpdateCurrentSlideContents Msg

saveProjectDecoder : Decoder SaveResult
saveProjectDecoder =
    succeed SaveResult
        |> required "id" string

encodeProject : String -> String -> String -> Project.Model -> Encode.Value
encodeProject knownContentCode learningContentCode projectName project =
    Encode.object
        [ ( "l1", Encode.string knownContentCode )
        , ( "l2", Encode.string learningContentCode )
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

processDirtyMessage : Model -> Bool -> Model
processDirtyMessage ( { project } as model ) isDirty =
    case isDirty of
        True ->
            case project of
                Clean p ->
                    { model | project = Dirty p }

                _ ->
                    model

        False ->
            case project of
                Dirty p ->
                    { model | project = Clean p }

                _ ->
                    model

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

update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { knownContentCode, learningContentCode, projectName, project, flags } as model ) =
    case msg of
        Cancel ->
            ( model, Navigation.pushUrl model.navigationKey (Routes.routeToUrl Routes.Home) )

        CompletedProjectSave result ->
            case result of
                Ok _ ->
                    case project of
                        Dirty (Api.Updating p) ->
                            ( { model | project = Clean (Api.Loaded p) }, Cmd.none )

                        Dirty (Api.UpdatingSlowly p) ->
                            ( { model | project = Clean (Api.Loaded p) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( { model | project = Clean Api.Failed }
                    , Cmd.none
                    )

        PassedSlowSaveThreshold ->
            case project of
                Dirty (Api.Updating projectModel) ->
                    ( { model | project = Dirty (Api.UpdatingSlowly projectModel) }
                    , Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ProjectMsg projectMsg ->
            case projectMsg of
                Project.UpdateCurrentSlideContents nextMsg ->
                    (model, Task.perform (always (UpdateCurrentSlideContents (ProjectMsg nextMsg) ) ) (Task.succeed ()))

                _ ->
                    case project of
                        Clean (Api.Loaded projectModel) ->
                            let
                                updatedProjectModel =
                                    Project.update projectMsg projectModel
                            in
                            ( { model | project = Dirty (Api.Loaded updatedProjectModel) }
                            , Cmd.none
                            )

                        Dirty (Api.Loaded projectModel) ->
                            let
                                updatedProjectModel =
                                    Project.update projectMsg projectModel
                            in
                            ( { model | project = Dirty (Api.Loaded updatedProjectModel) }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

        Save ->
            case project of
                Dirty (Api.Loaded projectModel) ->
                    ( { model | project = Dirty (Api.Updating projectModel) }
                    , Cmd.batch
                        [ encodeProject knownContentCode learningContentCode projectName projectModel
                            |> saveProject flags.candorUrl
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
        [ onClick (UpdateCurrentSlideContents Save)
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
view ( {  project, flags } as model ) =
    case project of
        Clean (Api.Loaded projectModel) ->
            loadedView model projectModel

        Dirty (Api.Loaded projectModel) ->
            loadedView model projectModel

        Dirty (Api.Updating projectModel) ->
            loadedView model projectModel

        Dirty (Api.UpdatingSlowly projectModel) ->
            loadedView model projectModel

        Clean Api.LoadingSlowly ->
            div
                [ ]
                [ (Loading.icon flags.loadingPath) ]

        Dirty Api.LoadingSlowly ->
            div
                [ ]
                [ (Loading.icon flags.loadingPath) ]

        Clean (Api.CreatingSlowly _) ->
            div
                [ ]
                [ (Loading.icon flags.loadingPath) ]

        _ ->
            div
                [ ]
                [ ]


