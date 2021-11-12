module Create exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Navigation
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (class, disabled, type_, value)
import Html.Events exposing (onClick, onInput)
import LanguageHelpers
import LanguageSelect
import Routes
import ViewHelpers

-- MODEL

type alias Data =
    { projectName : String
    , navigationKey : Navigation.Key
    }

type Model
    = Success Data LanguageSelect.Model LanguageSelect.Model

initialData : Navigation.Key -> Data
initialData navigationKey =
    { projectName = ""
    , navigationKey = navigationKey
    }

init : Navigation.Key -> LanguageHelpers.Model -> Model
init key languages =
    let
        knownLanguageModel = LanguageSelect.init languages
        learningLanguageModel = LanguageSelect.init languages
    in
    Success (initialData key) knownLanguageModel learningLanguageModel

-- UPDATE

type Msg
    = KnownLanguageMsg LanguageSelect.Msg
    | LearningLanguageMsg LanguageSelect.Msg
    | ProjectNameInput String
    | Create
    | Cancel

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case ( msg, model ) of
        ( KnownLanguageMsg knownLanguageMsg, Success data knownLanguageModel learningLanguageModel ) ->
            let
                ( updatedModel, updatedCmd ) =
                    LanguageSelect.update knownLanguageMsg knownLanguageModel
            in
            ( Success data updatedModel learningLanguageModel
            , Cmd.map KnownLanguageMsg updatedCmd
            )

        ( LearningLanguageMsg learningLanguageMsg, Success data knownLanguageModel learningLanguageModel ) ->
            let
                ( updatedModel, updatedCmd ) =
                    LanguageSelect.update learningLanguageMsg learningLanguageModel
            in
            ( Success data knownLanguageModel updatedModel
            , Cmd.map LearningLanguageMsg updatedCmd
            )

        ( ProjectNameInput projectName, Success data knownLanguageModel learningLanguageModel ) ->
            ( Success { data | projectName = projectName } knownLanguageModel learningLanguageModel
            , Cmd.none
            )

        ( Create, Success { projectName, navigationKey } knownLanguageModel learningLanguageModel ) ->
            ( model
            , Navigation.pushUrl
                navigationKey
                (Routes.routeToUrl
                    (
                        Routes.EditNew
                            (LanguageSelect.getChosenContentCodeString knownLanguageModel)
                            (LanguageSelect.getChosenContentCodeString learningLanguageModel)
                            (Just projectName)
                    )
                )
            )

        ( Cancel, Success data _ _ ) ->
            ( model, Navigation.pushUrl data.navigationKey (Routes.routeToUrl Routes.Home) )


isCreateButtonDisabled : Model -> Bool
isCreateButtonDisabled model =
    case model of
        Success { projectName } klm llm ->
            let
                knownLanguage = LanguageSelect.getChosenDisplayLanguage klm
                learningLanguage = LanguageSelect.getChosenDisplayLanguage llm
            in
                String.isEmpty projectName
                || String.isEmpty knownLanguage
                || String.isEmpty learningLanguage

view : Model -> Html Msg
view model =
    case model of
        Success { projectName } knownLanguage learningLanguage ->
            div
                [ class "create-page" ]
                [ ViewHelpers.viewLanguageSelect "Known Language: " KnownLanguageMsg knownLanguage
                , ViewHelpers.viewLanguageSelect "Learning Language: " LearningLanguageMsg learningLanguage
                , div
                    [ class "text-input" ]
                    [ label
                        [ ]
                        [ text "Project Name: "
                        , input
                            [ type_ "text"
                            , value projectName
                            , onInput ProjectNameInput
                            ]
                            [ ]
                        ]
                    ]
                , div
                    [ ]
                    [ button
                        [ onClick Create
                        , disabled (isCreateButtonDisabled model)
                        ]
                        [ text "Create" ]
                    , button
                        [ onClick Cancel ]
                        [ text "Cancel" ]
                    ]
                 ]
