module EditExisting exposing (..)

import Api
import Array exposing (Array)
import Browser.Navigation as Navigation
import Data.Slide as Slide exposing (slidesDecoder)
import Edit
import Html exposing (Html)
import Http exposing (stringResolver)
import Loading
import Data.Slide
import Task exposing (Task)

-- MODEL

type alias Model =
    Edit.Model

init : Navigation.Key -> String -> String -> String -> (Model, Cmd Msg)
init key k l p =
    let
        ( editModel, editMsg ) =
            Edit.init key k l p Api.Loading
    in
    ( editModel
    , Cmd.batch
        [ Cmd.map EditMsg editMsg
        , fetchProject
            |> Task.attempt CompletedProjectLoad
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )

fetchProject : Task Http.Error (Array Slide.Model)
fetchProject =
    Http.task
        { method = "GET"
        , headers = []
        , url = "http://192.168.34.9:8080/catalog"
        , body = Http.emptyBody
        , resolver = stringResolver (Api.handleJsonResponse slidesDecoder)
        , timeout = Nothing
        }

-- UPDATE

type Msg =
    PassedSlowLoadThreshold
    | CompletedProjectLoad (Result Http.Error (Array Data.Slide.Model))
    | EditMsg Edit.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PassedSlowLoadThreshold ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                updatedSlides =
                    case model.slides of
                        Api.Loading ->
                            Api.LoadingSlowly

                        other ->
                            other
            in
            ( { model | slides = updatedSlides }
            , Cmd.none )

        CompletedProjectLoad result ->
            case result of
                Ok slides ->
                    ( { model | slides = Api.Loaded slides }
                    , Cmd.none
                    )
                Err _ ->
                    ( { model | slides = Api.Failed }
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
