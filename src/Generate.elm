module Generate exposing (init, Model, Msg, update, view)

import Api
import Browser.Navigation as Navigation
import Bytes exposing (Bytes)
import Element exposing (centerX, column, Element, spacing)
import Element.Input as Input
import File.Download as Download
import Flags exposing (Flags)
import Http exposing (bytesResolver)
import LanguageHelpers
import Loading
import Routes
import Task exposing (Task)
import UIHelpers exposing (buttonAttributes)
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
        zip = List.singleton ( Http.header "Content-Type" "application/zip" )
    in
    Http.task
        { method = "GET"
        , headers = zip
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

viewActionButtons : Element Msg
viewActionButtons =
    Input.button
        (centerX :: buttonAttributes)
        { onPress = Just Cancel
        , label = Element.text "Return to Home Screen"
        }

viewFailureMessage : String -> Element Msg
viewFailureMessage pn =
    Element.text (pn ++ " failed to generate!")

viewFailure : String -> Element Msg
viewFailure pn =
    column
        [ spacing 10
        , centerX
        ]
        [ viewFailureMessage pn
        , viewActionButtons
        ]

viewGeneratedMessage : String -> Element Msg
viewGeneratedMessage pn =
    Element.text (pn ++ " is ready to be downloaded. Follow the browser's instructions.")

viewGenerated : String -> Element Msg
viewGenerated pn =
    column
        [ spacing 10
        , centerX
        ]
        [ viewGeneratedMessage pn
        , viewActionButtons
        ]

viewGeneratingMessage : String -> Element Msg
viewGeneratingMessage pn =
    Element.text (pn ++ " is being generated. Please wait...")

viewGenerating : String -> Element Msg
viewGenerating pn =
    Element.el
        [ centerX ]
        (viewGeneratingMessage pn)

viewGeneratingSlowly : String -> String -> Element Msg
viewGeneratingSlowly pn loadingPath =
    column
        [ spacing 10
        , centerX
        ]
        [ viewGeneratingMessage pn
        , Loading.iconElement loadingPath
        ]

view : Model -> Element Msg
view { loadingPath, pn, status } =
    let
        element =
            case status of
                Failure ->
                    viewFailure pn

                Generated ->
                    viewGenerated pn

                Generating ->
                    viewGenerating loadingPath

                GeneratingSlowly ->
                    viewGeneratingSlowly pn loadingPath
    in
    Element.el
        [ centerX ]
        element

