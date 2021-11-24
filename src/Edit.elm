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
import LanguageHelpers
import Loading
import Routes
import Task exposing (Task)
import Url.Builder as Builder

-- MODEL

type Modified a =
    Clean a
    | Dirty a

type alias Model =
    { flags : Flags.Model
    , knownLanguage : LanguageHelpers.Language
    , learningLanguage : LanguageHelpers.Language
    , navigationKey : Navigation.Key
    , project : Modified (Api.Status Project.Model)
    , projectName : String
    }

type alias SaveResult =
    { id : String }

type alias Init =
    { flags : Flags.Model
    , kl : LanguageHelpers.Language
    , key : Navigation.Key
    , ll : LanguageHelpers.Language
    , pn : String
    , model : Api.Status Project.Model
    }

init : Init -> Model
init { key, kl, ll, pn, model, flags } =
    { flags = flags
    , knownLanguage = kl
    , learningLanguage = ll
    , navigationKey = key
    , project = Clean model
    , projectName = pn
    }

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
update msg ( { knownLanguage, learningLanguage, projectName, project, flags } as model ) =
    case msg of
        Cancel ->
            ( model, Navigation.pushUrl model.navigationKey (Routes.routeToUrl Routes.Home) )

        CompletedProjectSave result ->
            case result of
                Ok _ ->
                    case project of
                        Dirty (Api.Loading pm) ->
                            ( { model | project = Clean (Api.Loaded pm) }, Cmd.none )

                        Dirty (Api.LoadingSlowly pm) ->
                            ( { model | project = Clean (Api.Loaded pm) }, Cmd.none )

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

                Project.UpdateCurrentSlideContents nextMsg ->
                    (model, Task.perform (always (UpdateCurrentSlideContents (ProjectMsg nextMsg) ) ) (Task.succeed ()))

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

        UpdateCurrentSlideContents _ ->
            ( model, Cmd.none )

viewEditPageInfo : Model -> Html Msg
viewEditPageInfo { knownLanguage, learningLanguage, projectName } =
    div
        [ class "edit-page-info"]
        [
            div
                [ class "edit-page-language-info" ]
                [
                    div
                        [ ]
                        [ text ( "Known Language: " ++ knownLanguage.displayName ) ]
                    ,
                    div
                        [ ]
                        [ text ( "Learning Language: " ++ learningLanguage.displayName ) ]
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

viewCancelButton : Html Msg
viewCancelButton =
    button
        [ onClick Cancel ]
        [ text "Return to Home Screen" ]

viewActionButtons : Model -> Html Msg
viewActionButtons model =
    div
        [ class "edit-page-action-buttons" ]
        [ viewSaveButton model
        , viewCancelButton
        ]

loadedView : Model -> Project.Model -> List (Html Msg)
loadedView model projectModel =
    [ viewEditPageInfo model
    , viewActionButtons model
    , Project.view projectModel
        |> Html.map ProjectMsg
    ]

errorView : Model -> String -> List (Html Msg)
errorView { projectName } errorStr =
    [ div
        [ ]
        [ text ("Project " ++ projectName ++ errorStr) ]
    , div
        [ ]
        [ viewCancelButton ]
    ]

loadingSlowlyView : Model -> List (Html Msg)
loadingSlowlyView { flags } =
    [ Loading.icon flags.loadingPath ]

view : Model -> Html Msg
view ( {  project, projectName, flags } as model ) =
    let
        elements =
            case project of
                Clean Api.Failed ->
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
                    List.concat [(loadingSlowlyView model), (loadedView model projectModel)]
    in
    div
        [ class "edit-page "]
        elements
