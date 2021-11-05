module Generate exposing (init, Model, Msg, update, view)

import Api
import Browser.Navigation as Navigation
import Flags exposing (Flags)
import Html exposing (Html, button, div,  text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http exposing (stringResolver)
import Json.Decode exposing (Decoder, succeed, string)
import Json.Decode.Pipeline exposing (required)
import Loading
import Routes
import Task exposing (Task)
import Url.Builder as Builder

-- MODEL

type Status
    = Failure
    | Generated
    | Generating
    | GeneratingSlowly

type alias Model =
    { loadingPath : String
    , navigationKey : Navigation.Key
    , pn : String
    , status : Status
    }

type alias GenerationResult =
    { id : String }

type alias Init =
    { flags : Flags.Model
    , imageRepository : String
    , kcc : String
    , key : Navigation.Key
    , lcc : String
    , pn : String
    }

init : Init -> (Model, Cmd Msg)
init ( { flags, key, pn } as initValues ) =
    ( { loadingPath = flags.loadingPath, navigationKey = key, pn = pn, status = Generating }
    , Cmd.batch
        [ (postGenerationRequest initValues)
            |> Task.attempt CompletedGeneration
        , Task.perform (\_ -> PassedSlowGenerationThreshold) Loading.slowThreshold
        ]
    )

generationDecoder : Decoder GenerationResult
generationDecoder =
    succeed GenerationResult
        |> required "id" string

postGenerationRequest : Init -> Task Http.Error GenerationResult
postGenerationRequest { flags, imageRepository, kcc, lcc, pn } =
    let
        url = Builder.relative [flags.candorUrl, "generate", imageRepository, kcc, lcc, pn] []
    in
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = stringResolver ( Api.handleJsonResponse generationDecoder )
        , timeout = Nothing
        }

-- UPDATE

type Msg =
    Cancel
    | CompletedGeneration (Result Http.Error GenerationResult)
    | PassedSlowGenerationThreshold

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Cancel ->
            ( model, Navigation.pushUrl model.navigationKey (Routes.routeToUrl Routes.Home) )

        CompletedGeneration r ->
            case r of
                Ok _ ->
                    ( { model | status = Generated }, Cmd.none )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )

        PassedSlowGenerationThreshold ->
            case model.status of
                Generating ->
                    ( { model | status = GeneratingSlowly }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

-- VIEW

viewMessage : String -> Html Msg
viewMessage pn =
    div
        [ ]
        [ text (pn ++ " failed to generate!") ]

viewActionButtons : Html Msg
viewActionButtons =
    div
        [ ]
        [
            button
                [ onClick Cancel ]
                [ text "Return to Home Screen" ]
        ]

viewFailure : String -> Html Msg
viewFailure pn =
    div
        [ ]
        [ viewMessage pn
        , viewActionButtons
        ]

viewGenerated : String -> Html Msg
viewGenerated pn =
    div
        [ ]
        [ viewMessage pn
        , viewActionButtons
        ]

viewGenerating : String -> Html Msg
viewGenerating pn =
    div
        [ ]
        [ viewMessage pn ]

viewGeneratingSlowly : String -> String -> Html Msg
viewGeneratingSlowly pn loadingPath =
    div
        [ ]
        [ viewMessage pn
        , div
            [ ]
            [ Loading.icon loadingPath ]
        ]

view : Model -> Html Msg
view { loadingPath, pn, status } =
    div
        [ class "generate-page" ]
        [
            case status of
                Failure ->
                    viewFailure pn

                Generated ->
                    viewGenerated pn

                Generating ->
                    viewGenerating loadingPath

                GeneratingSlowly ->
                    viewGeneratingSlowly pn loadingPath
        ]

