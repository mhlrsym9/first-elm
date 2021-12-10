module Open exposing (Model, Msg, init, update, view)

import Api
import Browser.Navigation as Navigation
import Element exposing (centerX, centerY, Column, column, Element, fill, shrink, spacing)
import Element.Font as Font
import Element.Input as Input
import Flags exposing (Flags)
import Http exposing (stringResolver)
import Json.Decode exposing (Decoder, decodeString, list, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import LanguageHelpers
import LanguageSelect
import List.Extra
import Loading
import Routes
import Task exposing (Task)
import UIHelpers exposing (buttonAttributes)
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
    { chosenProject : Maybe String
    , displayedProjectDescriptions : ProjectDescriptions
    , knownLanguageModel : LanguageSelect.Model
    , learningLanguageModel : LanguageSelect.Model
    , loadingPath : String
    , navigationKey : Navigation.Key
    , projectDescriptions : Api.Status ProjectDescriptions
    }

type Model
    = Success Data

initialData : Navigation.Key -> LanguageSelect.Model -> LanguageSelect.Model -> String -> Data
initialData navigationKey knownLanguageModel learningLanguageModel loadingPath =
    { chosenProject = Nothing
    , displayedProjectDescriptions = [ ]
    , knownLanguageModel = knownLanguageModel
    , learningLanguageModel = learningLanguageModel
    , loadingPath = loadingPath
    , projectDescriptions = Api.Loading []
    , navigationKey = navigationKey
    }

init : Navigation.Key -> Flags.Model -> LanguageHelpers.Model -> (Model, Cmd Msg)
init key { loadingPath, candorUrl } languages =
    let
        knownLanguageModel = LanguageSelect.init languages
        learningLanguageModel = LanguageSelect.init languages
    in
    ( Success (initialData key knownLanguageModel learningLanguageModel loadingPath)
    , Cmd.batch
        [ fetchProjects candorUrl
            |> Task.attempt CompletedProjectDescriptorsLoad
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )

fetchProjects : String -> Task Http.Error ProjectDescriptions
fetchProjects candorUrl =
    Http.task
        { method = "GET"
        , headers = []
        , url = candorUrl ++ "/catalog"
        , body = Http.emptyBody
        , resolver = stringResolver (Api.handleJsonResponse projectDescriptionsDecoder)
        , timeout = Nothing
        }

-- UPDATE

type ImageRepository
    = Alphabet
    | CourseWare

type Msg
    = Cancel
    | CompletedProjectDescriptorsLoad (Result Http.Error ProjectDescriptions)
    | Delete String
    | Generate ImageRepository String
    | KnownLanguageMsg LanguageSelect.Msg
    | LearningLanguageMsg LanguageSelect.Msg
    | Open String
    | PassedSlowLoadThreshold
    | UpdateProject String

extractContentCodes : ProjectDescriptions -> (ProjectDescription -> String) -> List String
extractContentCodes pds f =
    pds
        |> List.map f
        |> List.Extra.unique

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
        kcc = LanguageSelect.getChosenContentCodeString data.knownLanguageModel
        lcc = LanguageSelect.getChosenContentCodeString updatedModel
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

justPassThroughKnownLanguagesFilter : LanguageSelect.Msg -> Data -> (Model, Cmd Msg)
justPassThroughKnownLanguagesFilter knownLanguageMsg data =
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
        kcc = LanguageSelect.getChosenContentCodeString updatedModel
        lcc = LanguageSelect.getChosenContentCodeString data.learningLanguageModel
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

justPassThroughLearningLanguagesFilter : LanguageSelect.Msg -> Data -> (Model, Cmd Msg)
justPassThroughLearningLanguagesFilter learningLanguageMsg data =
    let
        ( updatedModel, updatedCmd ) =
            LanguageSelect.update learningLanguageMsg data.learningLanguageModel
    in
    ( Success { data | learningLanguageModel = updatedModel }
    , Cmd.map LearningLanguageMsg updatedCmd
    )

processProjectKey : String -> Navigation.Key -> (String -> String -> Maybe String -> Routes.Route) -> Cmd Msg
processProjectKey projectKey navigationKey route =
    let
        r = decodeString projectDescriptionDecoder projectKey
    in
    case r of
        Ok { l1, l2, project } ->
            Navigation.pushUrl
                navigationKey <|
                Routes.routeToUrl <|
                    route l1 l2 (Just project)

        Err _ ->
            Cmd.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case ( msg, model ) of
        ( Cancel, Success { navigationKey } ) ->
            ( model, Navigation.pushUrl navigationKey (Routes.routeToUrl Routes.Home) )

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
                        [ Cmd.map KnownLanguageMsg (LanguageSelect.setAvailableLanguages uniqueKnownContentCodes data.knownLanguageModel)
                        , Cmd.map LearningLanguageMsg (LanguageSelect.setAvailableLanguages uniqueLearningContentCodes data.learningLanguageModel)
                        ]
                    )

                Err _ ->
                    ( Success { data | projectDescriptions = Api.Failed }, Cmd.none )

        ( Delete projectKey, Success { navigationKey } ) ->
            ( model , processProjectKey projectKey navigationKey Routes.Delete )

        ( Generate imageRepository projectKey, Success { navigationKey } ) ->
            let
                f = processProjectKey projectKey navigationKey
            in
            case imageRepository of
                Alphabet ->
                    ( model, f Routes.GenerateAlphabet )

                CourseWare ->
                    (model, f Routes.GenerateCourseWare )

        ( KnownLanguageMsg knownLanguageMsg, Success data ) ->
            case data.projectDescriptions of
                Api.Loaded _ ->
                    case knownLanguageMsg of
                        LanguageSelect.UpdateLanguage _ ->
                            updateLearningLanguagesFilter knownLanguageMsg data

                        _ ->
                            justPassThroughKnownLanguagesFilter knownLanguageMsg data

                _ ->
                    justPassThroughKnownLanguagesFilter knownLanguageMsg data

        ( LearningLanguageMsg learningLanguageMsg, Success data ) ->
            case data.projectDescriptions of
                Api.Loaded _ ->
                    case learningLanguageMsg of
                        LanguageSelect.UpdateLanguage _ ->
                            updateKnownLanguagesFilter learningLanguageMsg data

                        _ ->
                            justPassThroughLearningLanguagesFilter learningLanguageMsg data

                _ ->
                    justPassThroughLearningLanguagesFilter learningLanguageMsg data

        ( Open projectKey, Success { navigationKey } ) ->
            ( model, processProjectKey projectKey navigationKey Routes.EditExisting )

        ( PassedSlowLoadThreshold, Success data ) ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                updatedProjectDescriptions =
                    case data.projectDescriptions of
                        Api.Loading p ->
                            Api.LoadingSlowly p

                        other ->
                            other
            in
            ( Success { data | projectDescriptions = updatedProjectDescriptions }, Cmd.none )

        ( UpdateProject project, Success data ) ->
            ( Success { data | chosenProject = Just project }, Cmd.none )

-- VIEW

viewProjectsHeader : Element Msg
viewProjectsHeader =
    Element.el
        [ Font.size 24
        , centerX
        ]
        ( Element.text "Projects" )

prepareProjectText : Column ProjectDescription Msg
prepareProjectText =
    { header = Element.text ""
    , width = fill
    , view =
        \{ project } ->
            Element.el [ centerY ] (Element.text project)
    }

viewButton : Msg -> String -> Element Msg
viewButton msg label =
    Input.button
        buttonAttributes
        { onPress = Just msg
        , label = Element.text label
        }

viewOpenButton : String -> Element Msg
viewOpenButton key =
    viewButton (Open key) "Edit"

viewDeleteButton : String -> Element Msg
viewDeleteButton key =
    viewButton (Delete key) "Delete"

viewGenerateAlphabetButton : String -> Element Msg
viewGenerateAlphabetButton key =
    viewButton (Generate Alphabet key) "Generate Alphabet Slides"

viewGenerateCourseWareButton : String -> Element Msg
viewGenerateCourseWareButton key =
    viewButton (Generate CourseWare key) "Generate CourseWare Slides"

prepareButton : (String -> Element Msg) -> Column ProjectDescription Msg
prepareButton fnc =
    { header = Element.text ""
    , width = shrink
    , view =
        \pd ->
            let
                key = Encode.encode 0 ( encodeProjectDescription pd )
            in
            fnc key
    }

viewProjectsTable : ProjectDescriptions -> Element Msg
viewProjectsTable displayedProjectDescriptions =
    Element.table
        [ spacing 10 ]
        { data = displayedProjectDescriptions
        , columns =
            [ prepareProjectText
            , prepareButton viewOpenButton
            , prepareButton viewDeleteButton
            , prepareButton viewGenerateAlphabetButton
            , prepareButton viewGenerateCourseWareButton
            ]
        }

view : Model -> Element Msg
view model =
    case model of
        Success { knownLanguageModel, learningLanguageModel, loadingPath, chosenProject, projectDescriptions, displayedProjectDescriptions } ->
            case projectDescriptions of
                Api.Failed ->
                    Element.el
                        [ ]
                        ( Loading.error "project descriptions" )

                Api.Loaded _ ->
                    column
                        [ spacing 10
                        , centerX
                        ]
                        [ ViewHelpers.viewLanguageSelect "Filter Projects by L1" KnownLanguageMsg knownLanguageModel
                        , ViewHelpers.viewLanguageSelect "Filter Projects by L2" LearningLanguageMsg learningLanguageModel
                        , viewProjectsHeader
                        , viewProjectsTable displayedProjectDescriptions
                        , Input.button
                            ( centerX :: buttonAttributes )
                            { onPress = Just Cancel
                            , label = Element.text "Cancel"
                            }
                        ]

                Api.Loading _ ->
                    Element.none

                Api.LoadingSlowly _ ->
                    Loading.iconElement loadingPath
