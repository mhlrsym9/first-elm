module LanguageSelect exposing (getChosenDisplayLanguage, getChosenContentCodeString, init, Model, Msg(..), setAvailableLanguages, update, view)

import Html exposing (Html, option, select, text)
import Html.Attributes exposing (attribute, value)
import Html.Events.Extra exposing (onChange)
import LanguageHelpers exposing (ContentCode, Language, Languages)
import Task exposing (Task)

-- MODEL

type alias Model =
    { chosenLanguage : Maybe Language
    , displayedLanguages : Languages
    , languageModel : LanguageHelpers.Model
    }

-- INIT

initialData : LanguageHelpers.Model -> Model
initialData ( { languages } as model ) =
    let
        chosenLanguage =
            case languages of
                f::_ ->
                    Just f
                [] ->
                    Nothing
    in
    { chosenLanguage = chosenLanguage
    , displayedLanguages = languages
    , languageModel = model
    }

init : LanguageHelpers.Model -> Model
init languageModel =
    initialData languageModel

getChosenDisplayLanguage : Model -> String
getChosenDisplayLanguage { chosenLanguage }  =
    case chosenLanguage of
        Just { displayName } ->
            displayName
        Nothing ->
            ""

getChosenContentCodeString : Model -> String
getChosenContentCodeString { chosenLanguage } =
    case chosenLanguage of
        Just language ->
            LanguageHelpers.contentCodeStringFromLanguage language

        Nothing ->
            ""

setAvailableLanguages : List String -> Model -> Cmd Msg
setAvailableLanguages ccs { languageModel } =
    Task.perform UpdateLanguages
        (Task.succeed
            (LanguageHelpers.emptyLanguage :: List.sortBy
                (\l -> LanguageHelpers.displayNameFromLanguage l)
                (LanguageHelpers.contentCodesToLanguages languageModel ccs)))

-- UPDATE

type Msg
    = UpdateLanguage String
    | UpdateLanguages Languages

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case ( msg, model ) of
        ( UpdateLanguage language, { chosenLanguage, languageModel } ) ->
            let
                dictLanguage = LanguageHelpers.findLanguageFromContentCode languageModel language
            in
            case dictLanguage of
                Just _ ->
                    ( { model | chosenLanguage = dictLanguage }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ( UpdateLanguages languages, _ ) ->
            ( { model | displayedLanguages = languages }
            , Cmd.none
            )

viewOption : Language -> Html Msg
viewOption language =
    let
        cc = LanguageHelpers.contentCodeStringFromLanguage language
        displayName = LanguageHelpers.displayNameFromLanguage language
    in
    option
        [ (attribute "key" cc)
        , value cc
        ]
        [ text displayName ]

view : Model -> Html Msg
view { chosenLanguage, displayedLanguages } =
    case chosenLanguage of
        Nothing ->
            text "Failure!"

        Just { contentCode } ->
            select
                [ value (LanguageHelpers.fromContentCode contentCode)
                , onChange UpdateLanguage
                ]
                (List.map viewOption displayedLanguages)

