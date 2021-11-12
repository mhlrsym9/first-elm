module Delete exposing (init, Model, Msg, update, view)

import Api
import Browser.Navigation as Navigation
import Flags exposing (Flags)
import Html exposing (button, div, Html, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http exposing (stringResolver)
import Json.Decode exposing (Decoder, succeed, string)
import Json.Decode.Pipeline exposing (required)
import LanguageHelpers
import Loading
import ProjectAccess exposing (ProjectAccess)
import Routes
import Task exposing (Task)
import Url.Builder as Builder

type Status
    = Deleted
    | Deleting
    | DeletingSlowly
    | Failed

type alias Model =
    { flags : Flags.Model
    , kl : LanguageHelpers.Language
    , ll : LanguageHelpers.Language
    , navigationKey : Navigation.Key
    , projectName : String
    , status : Status
    }

type alias DeleteResult =
    { id : String }

deleteProject : ProjectAccess -> Task Http.Error DeleteResult
deleteProject { flags, kl, ll, pn } =
    let
        kcc = LanguageHelpers.contentCodeStringFromLanguage kl
        lcc = LanguageHelpers.contentCodeStringFromLanguage ll
        url = Builder.relative [flags.candorUrl, "delete", kcc, lcc, pn] []
    in
    Http.task
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = stringResolver ( Api.handleJsonResponse deleteProjectDecoder )
        , timeout = Nothing
        }

init : ProjectAccess -> (Model, Cmd Msg)
init ( { flags, key, kl, ll, pn } as initValues ) =
    (
        { flags = flags
        , kl = kl
        , ll = ll
        , navigationKey = key
        , projectName = pn
        , status = Deleting
        }
    , Cmd.batch
        [ (deleteProject initValues)
            |> Task.attempt CompletedProjectDelete
        , Task.perform (\_ -> PassedSlowDeleteThreshold) Loading.slowThreshold
        ]
    )

-- UPDATE

type Msg
    = Cancel
    | CompletedProjectDelete (Result Http.Error DeleteResult)
    | PassedSlowDeleteThreshold

deleteProjectDecoder : Decoder DeleteResult
deleteProjectDecoder =
    succeed DeleteResult
        |> required "id" string

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Cancel ->
            ( model, Navigation.pushUrl model.navigationKey (Routes.routeToUrl Routes.Home) )

        CompletedProjectDelete result ->
            case result of
                Ok _ ->
                    ( { model | status = Deleted } , Cmd.none )

                Err _ ->
                    ( { model | status = Failed } , Cmd.none )

        PassedSlowDeleteThreshold ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                updatedStatus =
                    case model.status of
                        Deleting ->
                            DeletingSlowly

                        _ ->
                            model.status
            in
            ( { model | status = updatedStatus } , Cmd.none )

viewHomeButton : Html Msg
viewHomeButton =
    div
        [ ]
        [
            button
                [ onClick Cancel ]
                [ text "Return to Home Page" ]
        ]

view : Model -> Html Msg
view { flags, projectName, status } =
    case status of
        Deleted ->
            div
                [ class "delete-page-deleted" ]
                [ text ( projectName ++ " successfully deleted." )
                , viewHomeButton
                ]

        Deleting ->
            div
                [ class "delete-page-deleting" ]
                [ text ( projectName ++ " is being deleted..." ) ]

        DeletingSlowly ->
            div
                [ class "delete-page-deleting-slowly" ]
                [ text ( projectName ++ " is being deleted..." )
                , (Loading.icon flags.loadingPath)
                ]

        Failed ->
            div
                [ class "delete-page-failed" ]
                [ text ( projectName ++ " could not be deleted." )
                , viewHomeButton
                ]
