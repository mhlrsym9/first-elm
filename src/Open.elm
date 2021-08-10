module Open exposing (Model, Msg, init, update, view)

import Api
import Browser.Navigation as Navigation
import Html exposing (Html, button, div, label, option, select, text)
import Html.Attributes exposing (attribute, class, disabled, id, name, size, value)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onChange)
import Http exposing (stringResolver)
import Json.Decode exposing (Decoder, decodeString, list, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import LanguageSelect
import List.Extra
import Loading
import Routes
import Task exposing (Task)
import ViewHelpers

-- MODEL

type alias ProjectDescription =
    { l1: String
    , l2: String
    , project: String
    }

encodeProjectDescription : ProjectDescription -> Encode.Value
encodeProjectDescription { l1, l2, project } =
    Encode.object
        [ ( "l1", Encode.string l1 )
        , ( "l2", Encode.string l2 )
        , ( "project" , Encode.string project )
        ]

projectDescriptionDecoder : Decoder ProjectDescription
projectDescriptionDecoder =
    succeed ProjectDescription
        |> required "l1" string
        |> required "l2" string
        |> required "project" string

type alias ProjectDescriptions =
    List ProjectDescription

projectDescriptionsDecoder : Decoder ProjectDescriptions
projectDescriptionsDecoder =
    (list projectDescriptionDecoder)

type alias Data =
    { knownLanguageModel : LanguageSelect.Model
    , learningLanguageModel : LanguageSelect.Model
    , projectDescriptions : Api.Status ProjectDescriptions
    , displayedProjectDescriptions : ProjectDescriptions
    , chosenProject : Maybe String
    , navigationKey : Navigation.Key
    }

type Model
    = Success Data

initialData : Navigation.Key -> LanguageSelect.Model -> LanguageSelect.Model -> Data
initialData navigationKey knownLanguageModel learningLanguageModel =
    { knownLanguageModel = knownLanguageModel
    , learningLanguageModel = learningLanguageModel
    , projectDescriptions = Api.Loading
    , displayedProjectDescriptions = [ ]
    , chosenProject = Nothing
    , navigationKey = navigationKey
    }

init : Navigation.Key -> LanguageSelect.Languages -> (Model, Cmd Msg)
init key languages =
    let
        ( knownLanguageModel, knownLanguageCmd ) =
            LanguageSelect.init languages

        ( learningLanguageModel, learningLanguageCmd ) =
            LanguageSelect.init languages
    in
    ( Success (initialData key knownLanguageModel learningLanguageModel)
    , Cmd.batch
        [ Cmd.map KnownLanguageMsg knownLanguageCmd
        , Cmd.map LearningLanguageMsg learningLanguageCmd
        , fetchProjects
            |> Task.attempt CompletedProjectDescriptorsLoad
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )

fetchProjects : Task Http.Error ProjectDescriptions
fetchProjects =
    Http.task
        { method = "GET"
        , headers = []
        , url = "http://192.168.34.9:8080/catalog"
        , body = Http.emptyBody
        , resolver = stringResolver (Api.handleJsonResponse projectDescriptionsDecoder)
        , timeout = Nothing
        }

-- UPDATE

type Msg
    = KnownLanguageMsg LanguageSelect.Msg
    | LearningLanguageMsg LanguageSelect.Msg
    | PassedSlowLoadThreshold
    | CompletedProjectDescriptorsLoad (Result Http.Error ProjectDescriptions)
    | UpdateProject String
    | Open
    | Cancel

extractContentCodes : ProjectDescriptions -> (ProjectDescription -> String) -> List String
extractContentCodes pds f =
    pds
        |> List.map f
        |> List.Extra.unique

getProjectDescriptions : Model -> ProjectDescriptions
getProjectDescriptions model =
    case model of
        Success { projectDescriptions } ->
            case projectDescriptions of
                Api.Loaded a ->
                    a
                _ ->
                    [ ]

matchedProjectDescriptions : Api.Status ProjectDescriptions -> (String -> String -> ProjectDescription -> Bool) -> String -> String -> ProjectDescriptions
matchedProjectDescriptions projectDescriptions f kcc lcc =
    case projectDescriptions of
        Api.Loaded pds ->
            List.filter (f kcc lcc) pds
        _ ->
            [ ]

matchingProjectDescription : String -> String -> ProjectDescription -> Bool
matchingProjectDescription kcc lcc { l1, l2 } =
    ((kcc == "") || (kcc == l1)) && ((lcc == "") || (lcc == l2))

updateKnownLanguagesFilter : LanguageSelect.Msg -> Data -> (Model, Cmd Msg)
updateKnownLanguagesFilter learningLanguageMsg data =
    let
        ( updatedModel, updatedCmd ) =
            LanguageSelect.update learningLanguageMsg data.learningLanguageModel
        kcc = LanguageSelect.getContentCode data.knownLanguageModel
        lcc = LanguageSelect.getContentCode updatedModel
        updatedProjectDescriptions = matchedProjectDescriptions data.projectDescriptions matchingProjectDescription kcc lcc
        uniqueKnownContentCodes = extractContentCodes updatedProjectDescriptions (\{ l1 } -> l1)
    in
    ( Success
        { data | learningLanguageModel = updatedModel
        , displayedProjectDescriptions = updatedProjectDescriptions
        }
    , Cmd.batch
        [ Cmd.map LearningLanguageMsg updatedCmd
        , Cmd.map KnownLanguageMsg (LanguageSelect.setAvailableLanguages uniqueKnownContentCodes data.knownLanguageModel)
        ]
    )

justPassthroughKnownLanguagesFilter : LanguageSelect.Msg -> Data -> (Model, Cmd Msg)
justPassthroughKnownLanguagesFilter knownLanguageMsg data =
    let
        ( updatedModel, updatedCmd ) =
            LanguageSelect.update knownLanguageMsg data.knownLanguageModel
    in
    ( Success { data | knownLanguageModel = updatedModel }
    , Cmd.map KnownLanguageMsg updatedCmd
    )

updateLearningLanguagesFilter : LanguageSelect.Msg -> Data -> (Model, Cmd Msg)
updateLearningLanguagesFilter knownLanguageMsg data =
    let
        ( updatedModel, updatedCmd ) =
            LanguageSelect.update knownLanguageMsg data.knownLanguageModel
        kcc = LanguageSelect.getContentCode updatedModel
        lcc = LanguageSelect.getContentCode data.learningLanguageModel
        updatedProjectDescriptions = matchedProjectDescriptions data.projectDescriptions matchingProjectDescription kcc lcc
        uniqueLearningContentCodes = extractContentCodes updatedProjectDescriptions (\{ l2 } -> l2)
    in
    ( Success
        { data | knownLanguageModel = updatedModel
        , displayedProjectDescriptions = updatedProjectDescriptions
        }
    , Cmd.batch
        [ Cmd.map KnownLanguageMsg updatedCmd
        , Cmd.map LearningLanguageMsg (LanguageSelect.setAvailableLanguages uniqueLearningContentCodes data.learningLanguageModel)
        ]
    )

justPassthroughLearningLanguagesFilter : LanguageSelect.Msg -> Data -> (Model, Cmd Msg)
justPassthroughLearningLanguagesFilter learningLanguageMsg data =
    let
        ( updatedModel, updatedCmd ) =
            LanguageSelect.update learningLanguageMsg data.learningLanguageModel
    in
    ( Success { data | learningLanguageModel = updatedModel }
    , Cmd.map LearningLanguageMsg updatedCmd
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case ( msg, model ) of
        ( KnownLanguageMsg knownLanguageMsg, Success data ) ->
            case data.projectDescriptions of
                Api.Loaded _ ->
                    case knownLanguageMsg of
                        LanguageSelect.UpdateLanguage _ ->
                            updateLearningLanguagesFilter knownLanguageMsg data

                        _ ->
                            justPassthroughKnownLanguagesFilter knownLanguageMsg data

                _ ->
                    justPassthroughKnownLanguagesFilter knownLanguageMsg data

        ( LearningLanguageMsg learningLanguageMsg, Success data ) ->
            case data.projectDescriptions of
                Api.Loaded _ ->
                    case learningLanguageMsg of
                        LanguageSelect.UpdateLanguage _ ->
                            updateKnownLanguagesFilter learningLanguageMsg data

                        _ ->
                            justPassthroughLearningLanguagesFilter learningLanguageMsg data

                _ ->
                    justPassthroughLearningLanguagesFilter learningLanguageMsg data

        ( CompletedProjectDescriptorsLoad result, Success data ) ->
            case result of
                Ok projectDescriptions ->
                    let
                        uniqueKnownContentCodes =
                            extractContentCodes projectDescriptions (\{ l1 } -> l1)
                        uniqueLearningContentCodes =
                            extractContentCodes projectDescriptions (\{ l2 } -> l2)
                    in
                    ( Success
                        { data | projectDescriptions = Api.Loaded projectDescriptions
                        , displayedProjectDescriptions = projectDescriptions
                        }
                    , Cmd.batch
                        [ Cmd.map KnownLanguageMsg (LanguageSelect.alwaysSetAvailableLanguages uniqueKnownContentCodes data.knownLanguageModel)
                        , Cmd.map LearningLanguageMsg (LanguageSelect.alwaysSetAvailableLanguages uniqueLearningContentCodes data.learningLanguageModel)
                        ]
                    )

                Err _ ->
                    ( Success { data | projectDescriptions = Api.Failed }, Cmd.none )

        ( PassedSlowLoadThreshold, Success data ) ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                updatedProjectDescriptions =
                    case data.projectDescriptions of
                        Api.Loading ->
                            Api.LoadingSlowly

                        other ->
                            other
            in
            ( Success { data | projectDescriptions = updatedProjectDescriptions }, Cmd.none )

        ( UpdateProject project, Success data ) ->
            ( Success { data | chosenProject = Just project }, Cmd.none )

        ( Open, Success { chosenProject, navigationKey } ) ->
            case chosenProject of
                Just chosenProjectStr ->
                    let
                        r = decodeString projectDescriptionDecoder chosenProjectStr
                    in
                    case r of
                        Ok { l1, l2, project } ->
                            ( model
                            , Navigation.pushUrl
                                navigationKey
                                ( Routes.routeToUrl
                                    ( Routes.EditExisting l1 l2 (Just project) )
                                )
                            )

                        Err _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ( Cancel, Success { navigationKey } ) ->
            ( model, Navigation.pushUrl navigationKey (Routes.routeToUrl Routes.Home) )

-- VIEW

isOpenButtonDisabled : Maybe String -> Bool
isOpenButtonDisabled projectName =
    case projectName of
        Just _ ->
            False
        Nothing ->
            True

viewProjectDescription : ProjectDescription -> Html Msg
viewProjectDescription ( { project } as pd ) =
    let
        key = Encode.encode 0 ( encodeProjectDescription pd )
    in
    option
        [ (attribute "key" key)
        , value key
        ]
        [ text project ]

view : Model -> Html Msg
view model =
    case model of
        Success { knownLanguageModel, learningLanguageModel, chosenProject, projectDescriptions, displayedProjectDescriptions } ->
            case projectDescriptions of
                Api.Loading ->
                    div [] []

                Api.LoadingSlowly ->
                    div [] [ Loading.icon ]

                Api.Failed ->
                    div [] [ Loading.error "project descriptions" ]

                Api.Loaded _ ->
                    div
                        [ class "create-page" ]
                        [ ViewHelpers.viewLanguageSelect "Filter Projects by L1" KnownLanguageMsg knownLanguageModel
                        , ViewHelpers.viewLanguageSelect "Filter Projects by L2" LearningLanguageMsg learningLanguageModel
                        , div
                            [ class "language-select" ]
                            [
                              label
                                  [ ]
                                  [ text ("Projects:")
                                  , select
                                        [ name "projects"
                                        , id "projects"
                                        , size 5
                                        , onChange UpdateProject
                                        ]
                                        (List.map viewProjectDescription displayedProjectDescriptions)
                                  ]
                            ]
                        , div
                            [ ]
                            [ button
                                [ disabled (isOpenButtonDisabled chosenProject)
                                , onClick Open ]
                                [ text "Open" ]
                            , button
                                [ onClick Cancel ]
                                [ text "Cancel" ]
                            ]
                         ]

