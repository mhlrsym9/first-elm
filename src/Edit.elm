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
    { slideIndex : Int
    , project : Api.Status Project.Model
    , knownContentCode : String
    , learningContentCode : String
    , projectName : String
    , navigationKey : Navigation.Key
    }

init : Navigation.Key -> String -> String -> String -> Api.Status Project.Model -> (Model, Cmd Msg)
init navigationKey knownContentCode learningContentCode projectName project =
    (
        { slideIndex = 0
        , project = project
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Cancel ->
            ( model, Navigation.pushUrl model.navigationKey (Routes.routeToUrl Routes.Home) )

view : Model -> Html Msg
view { knownContentCode, learningContentCode, projectName, project } =
    case project of
        Api.Loaded _ ->
            div
                [ class "edit-page" ]
                [
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
                    ,
                    div
                        [ class "edit-page-action-buttons" ]
                        [ button
                            [ onClick Cancel ]
                            [ text "Cancel" ]
                        ]
                ]

        _ ->
            div
                [ ]
                [ ]


