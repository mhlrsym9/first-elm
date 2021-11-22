module Generate exposing (init, Model, Msg, update, view)

import Api
import Browser.Navigation as Navigation
import Bytes exposing (Bytes)
import File.Download as Download
import Flags exposing (Flags)
import Html exposing (Html, button, div,  text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http exposing (bytesResolver)
import LanguageHelpers
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

type alias Init =
    { flags : Flags.Model
    , imageRepository : String
    , kl : LanguageHelpers.Language
    , key : Navigation.Key
    , ll : LanguageHelpers.Language
    , pn : String
    }

init : Init -> (Model, Cmd Msg)
init ( { flags, key, pn } as initValues ) =
    ( { loadingPath = flags.loadingPath, navigationKey = key, pn = pn, status = Generating }
    , Cmd.batch
        [ (postGenerationRequest initValues)
            |> Task.map (\{bytes} -> bytes)
            |> Task.attempt CompletedGeneration
        , Task.perform (\_ -> PassedSlowGenerationThreshold) Loading.slowThreshold
        ]
    )

postGenerationRequest : Init -> Task Http.Error Api.BytesWithHeaders
postGenerationRequest { flags, imageRepository, kl, ll, pn } =
    let
        kcc = LanguageHelpers.contentCodeStringFromLanguage kl
        lcc = LanguageHelpers.contentCodeStringFromLanguage ll
        url = Builder.relative [flags.candorUrl, "generate", imageRepository, kcc, lcc, pn] []
    in
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = bytesResolver ( Api.handleBytesResponse Ok )
        , timeout = Nothing
        }

-- UPDATE

type Msg =
    Cancel
    | CompletedGeneration (Result Http.Error Bytes)
    | PassedSlowGenerationThreshold

downloadGeneratedFile : Model -> Bytes -> Cmd msg
downloadGeneratedFile { pn } response =
    Download.bytes (pn ++ ".xml") "application/xml" response

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Cancel ->
            ( model, Navigation.pushUrl model.navigationKey (Routes.routeToUrl Routes.Home) )

        CompletedGeneration r ->
            case r of
                Ok response ->
                    ( { model | status = Generated }, downloadGeneratedFile model response )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )

        PassedSlowGenerationThreshold ->
            case model.status of
                Generating ->
                    ( { model | status = GeneratingSlowly }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

-- VIEW

viewActionButtons : Html Msg
viewActionButtons =
    div
        [ ]
        [
            button
                [ onClick Cancel ]
                [ text "Return to Home Screen" ]
        ]

viewFailureMessage : String -> Html Msg
viewFailureMessage pn =
    div
        [ ]
        [ text (pn ++ " failed to generate!") ]

viewFailure : String -> Html Msg
viewFailure pn =
    div
        [ ]
        [ viewFailureMessage pn
        , viewActionButtons
        ]

viewGeneratedMessage : String -> Html Msg
viewGeneratedMessage pn =
    div
        [ ]
        [ text (pn ++ " is ready to be downloaded. Follow the browser's instructions.") ]

viewGenerated : String -> Html Msg
viewGenerated pn =
    div
        [ ]
        [ viewGeneratedMessage pn
        , viewActionButtons
        ]

viewGeneratingMessage : String -> Html Msg
viewGeneratingMessage pn =
    div
        [ ]
        [ text (pn ++ " is being generated. Please wait...") ]

viewGenerating : String -> Html Msg
viewGenerating pn =
    div
        [ ]
        [ viewGeneratingMessage pn ]

viewGeneratingSlowly : String -> String -> Html Msg
viewGeneratingSlowly pn loadingPath =
    div
        [ ]
        [ viewGeneratingMessage pn
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

