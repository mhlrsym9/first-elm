module Edit exposing (Model, Msg, init, update, view)

import Array exposing (Array)
import Browser.Navigation as Navigation
import Data.Slide as Slide
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Routes

-- MODEL

type alias ModelData =
    { slideIndex : Int
    , slides : Array Slide.Model
    , knownContentCode : String
    , learningContentCode : String
    , projectName : String
    , navigationKey : Navigation.Key
    }

type Model =
    Loading
    | Success ModelData

init : Navigation.Key -> String -> String -> String -> Array Slide.Model -> (Model, Cmd Msg)
init navigationKey knownContentCode learningContentCode projectName slides =
    ( Success
        { slideIndex = 0
        , slides = slides
        , knownContentCode = knownContentCode
        , learningContentCode = learningContentCode
        , projectName = projectName
        , navigationKey = navigationKey
        }
    , Cmd.none
    )

-- UPDATE

type Msg =
    GotSlides (Result Http.Error ModelData)
    | Cancel

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case ( msg, model ) of
        ( Cancel, Success modelData ) ->
            ( model, Navigation.pushUrl modelData.navigationKey (Routes.routeToUrl Routes.Home) )

        ( Cancel, _ ) ->
            ( model, Cmd.none )

        ( GotSlides _, _ ) ->
            ( model, Cmd.none )

view : Model -> Html Msg
view model =
    case model of
        Loading ->
            div
                [ ]
                [ ]

        Success { knownContentCode, learningContentCode, projectName } ->
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


