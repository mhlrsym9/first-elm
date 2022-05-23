module Create exposing (Model, Msg(..), init, update, view)

import Browser.Navigation as Navigation
import Element exposing (centerX, column, Element, row, spacing)
import Element.Input as Input
import LanguageHelpers
import LanguageSelect
import Routes
import UIHelpers exposing (buttonAttributes)
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
    = Cancel
    | Create
    | KnownLanguageMsg LanguageSelect.Msg
    | LearningLanguageMsg LanguageSelect.Msg
    | ProjectNameInput String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case ( msg, model ) of
        ( Cancel, Success data _ _ ) ->
            ( model, Navigation.pushUrl data.navigationKey (Routes.routeToUrl Routes.Home) )

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

viewCreateButton : Model -> Element Msg
viewCreateButton model =
    case model of
        Success { projectName } klm llm ->
            let
                knownLanguage = LanguageSelect.getChosenDisplayLanguage klm
                learningLanguage = LanguageSelect.getChosenDisplayLanguage llm
                isHidden =
                    String.isEmpty projectName
                    || String.isEmpty knownLanguage
                    || String.isEmpty learningLanguage
            in
            if (isHidden) then
                Element.none
            else
                Input.button
                    buttonAttributes
                    { onPress = Just Create
                    , label = Element.text "Create"
                    }

view : Model -> Element Msg
view model =
    case model of
        Success { projectName } knownLanguage learningLanguage ->
            column
                [ centerX
                , spacing 10
                ]
            [ ViewHelpers.viewLanguageSelect "Known Language " KnownLanguageMsg knownLanguage
            , ViewHelpers.viewLanguageSelect "Learning Language " LearningLanguageMsg learningLanguage
            , Input.text
                [ ]
                { onChange = ProjectNameInput
                , text = projectName
                , placeholder = Just (Input.placeholder [ ] ( Element.text "Please supply the name of this project" ) )
                , label = Input.labelLeft [ ] (Element.text "Project Name:")
                }
            , row
                [ centerX
                , spacing 10
                ]
                [ viewCreateButton model
                , Input.button
                    buttonAttributes
                    { onPress = Just Cancel
                    , label = Element.text "Cancel"
                    }
                ]
            ]
