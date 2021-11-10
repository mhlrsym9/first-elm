module LanguageSelect exposing (alwaysSetAvailableLanguages, ContentCode, contentCodeFromLanguage, fetchLanguages, fromContentCode, getChosenLanguage, getContentCode, init, Language, languageFromContentCode, Languages, Model, Msg(..), setAvailableLanguages, toContentCode, update, view)

import Api
import Dict exposing (Dict)
import Html exposing (Html, option, select, text)
import Html.Attributes exposing (attribute, value)
import Html.Events.Extra exposing (onChange)
import Http exposing (stringResolver)
import Json.Decode exposing (Decoder, field, list, map, string)
import Json.Decode.Pipeline exposing (custom, required)
import Task exposing (Task)

-- MODEL

type ContentCode =
    ContentCode String

type alias Language =
    { displayName : String
    , contentCode : ContentCode
    }

type alias Languages =
    List Language

type alias Model =
    { chosenLanguage : Maybe Language
    , languages : Languages
    , displayedLanguages : Languages
    , dictLanguages : Dict String Language
    }

languageDecoder : Decoder Language
languageDecoder =
    Json.Decode.succeed Language
        |> required "display" string
        |> custom contentCodeDecoder

contentCodeDecoder : Decoder ContentCode
contentCodeDecoder =
    map ContentCode (field "content_code" string)

languagesDecoder : Decoder Languages
languagesDecoder =
    (list languageDecoder)

createDictTuple : Language -> ( String, Language )
createDictTuple ({ contentCode } as language) =
    let
        s = case contentCode of
            ContentCode str ->
                str
    in
    ( s, language )

emptyLanguage : Language
emptyLanguage =
    { displayName = "", contentCode = ( ContentCode "" ) }

initialData : Languages -> Model
initialData languages =
    let
        paddedLanguages =
            case languages of
                _ :: _ ->
                    (emptyLanguage)
                    :: languages
                [] ->
                    languages

        chosenLanguage =
            case paddedLanguages of
                f::_ ->
                    Just f
                [] ->
                    Nothing
    in
    { chosenLanguage = chosenLanguage
    , languages = paddedLanguages
    , displayedLanguages = paddedLanguages
    , dictLanguages = Dict.fromList (List.map createDictTuple paddedLanguages)
    }

init : Languages -> Model
init languages =
    initialData languages

getChosenLanguage : Model -> String
getChosenLanguage { chosenLanguage }  =
    case chosenLanguage of
        Just { displayName } ->
            displayName
        Nothing ->
            ""

getContentCode : Model -> String
getContentCode { chosenLanguage } =
    case chosenLanguage of
        Just { contentCode } ->
            case contentCode of
                ContentCode contentCodeStr ->
                    contentCodeStr
        Nothing ->
            ""

toContentCode : String -> ContentCode
toContentCode s =
    ContentCode s

fromContentCode : ContentCode -> String
fromContentCode cc =
    case cc of
        ContentCode s ->
            s

contentCodeFromLanguage : Language -> String
contentCodeFromLanguage language =
    fromContentCode language.contentCode

languageFromContentCode : Model -> String -> Language
languageFromContentCode model cc =
    let
        l = Dict.get cc model.dictLanguages
    in
    case l of
        Just language ->
            language
        Nothing ->
            { displayName = "", contentCode = toContentCode cc }

findLanguageFromContentCode : Dict String Language -> String -> Language
findLanguageFromContentCode dict cc =
    let
        ml = Dict.get cc dict
    in
    case ml of
        Just l ->
            l
        Nothing ->
            (emptyLanguage)

setAvailableLanguages : List String -> Model -> Cmd Msg
setAvailableLanguages ccs { dictLanguages } =
    Task.perform UpdateLanguages (Task.succeed ((emptyLanguage) :: (List.map (findLanguageFromContentCode dictLanguages) ccs)))

alwaysSetAvailableLanguages : List String -> Model -> Cmd Msg
alwaysSetAvailableLanguages ccs { dictLanguages } =
    Task.perform UpdateLanguages (Task.succeed ((emptyLanguage) :: (List.map (findLanguageFromContentCode dictLanguages) ccs)))

fetchLanguages : Task Http.Error (List Language)
fetchLanguages =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://lds.transparent.com/languages/all/simtir?json=true"
        , body = Http.emptyBody
        , resolver = stringResolver (Api.handleJsonResponse languagesDecoder)
        , timeout = Nothing
        }

-- UPDATE

type Msg
    = UpdateLanguage String
    | UpdateLanguages Languages

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case ( msg, model ) of
        ( UpdateLanguage language, { chosenLanguage, dictLanguages } ) ->
            let
                dictLanguage = Dict.get language dictLanguages
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
viewOption { displayName, contentCode } =
    case contentCode of
        ContentCode s ->
            option
                [ (attribute "key" s)
                , value s
                ]
                [ text displayName ]

view : Model -> Html Msg
view { chosenLanguage, displayedLanguages } =
    case chosenLanguage of
        Nothing ->
            text "Failure!"

        Just { contentCode } ->
            select
                [ value (fromContentCode contentCode)
                , onChange UpdateLanguage
                ]
                (List.map viewOption displayedLanguages)

