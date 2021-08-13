module Edit exposing (Model, Msg, init, update, view)

import Api
import Browser.Navigation as Navigation
import Data.Project as Project
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Routes

-- MODEL

type alias Model =
    { project : Api.Status Project.Model
    , knownContentCode : String
    , learningContentCode : String
    , projectName : String
    , navigationKey : Navigation.Key
    }

init : Navigation.Key -> String -> String -> String -> Api.Status Project.Model -> (Model, Cmd Msg)
init navigationKey knownContentCode learningContentCode projectName project =
    (
        { project = project
        , knownContentCode = knownContentCode
        , learningContentCode = learningContentCode
        , projectName = projectName
        , navigationKey = navigationKey
        }
        , Cmd.none
    )

-- UPDATE

type Msg =
    Cancel
    | ProjectMsg Project.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { project } as model ) =
    case msg of
        Cancel ->
            ( model, Navigation.pushUrl model.navigationKey (Routes.routeToUrl Routes.Home) )

        ProjectMsg projectMsg ->
            case project of
                Api.Loaded projectModel ->
                    let
                        ( updatedProjectModel, projectCommands ) =
                            Project.update projectMsg projectModel
                    in
                    ( { model | project = Api.Loaded updatedProjectModel }
                    , Cmd.map ProjectMsg projectCommands
                    )

                _ ->
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

viewActionButtons : Model -> Html Msg
viewActionButtons _ =
    div
        [ class "edit-page-action-buttons" ]
        [ button
            [ onClick Cancel ]
            [ text "Cancel" ]
        ]

view : Model -> Html Msg
view ( {  project } as model ) =
    case project of
        Api.Loaded projectModel ->
            div
                [ class "edit-page" ]
                [ viewEditPageInfo model
                , Project.view projectModel
                    |> Html.map ProjectMsg
                , viewActionButtons model
                ]

        _ ->
            div
                [ ]
                [ ]


