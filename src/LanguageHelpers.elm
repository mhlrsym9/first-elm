module LanguageHelpers exposing (..)

import Api
import Dict exposing (Dict)
import Http exposing (stringResolver)
import Json.Decode exposing (Decoder, field, list, map, string)
import Json.Decode.Pipeline exposing (custom, required)
import Task exposing (Task)

type ContentCode =
    ContentCode String

toContentCode : String -> ContentCode
toContentCode s =
    ContentCode s

fromContentCode : ContentCode -> String
fromContentCode cc =
    case cc of
        ContentCode s ->
            s

type alias Language =
    { displayName : String
    , contentCode : ContentCode
    }

contentCodeStringFromLanguage : Language -> String
contentCodeStringFromLanguage language =
    fromContentCode language.contentCode

displayNameFromLanguage : Language -> String
displayNameFromLanguage language =
    language.displayName

type alias Languages =
    List Language

type alias Model =
    { languages : Languages
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
createDictTuple ( { contentCode } as language ) =
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
    in
    { languages = paddedLanguages
    , dictLanguages = Dict.fromList (List.map createDictTuple paddedLanguages)
    }

initEmptyLanguageHelpers : Model
initEmptyLanguageHelpers =
    initialData []

init : Languages -> Model
init languages =
    initialData languages

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

findLanguageFromContentCode : Model -> String -> Maybe Language
findLanguageFromContentCode { dictLanguages } cc =
    Dict.get cc dictLanguages

contentCodesToLanguages : Model -> List String -> Languages
contentCodesToLanguages model ccs =
    List.filterMap (findLanguageFromContentCode model) ccs

fetchLanguages : Task Http.Error Languages
fetchLanguages =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://lds.transparent.com/languages/all/simtir?json=true"
        , body = Http.emptyBody
        , resolver = stringResolver (Api.handleJsonResponse languagesDecoder)
        , timeout = Nothing
        }

