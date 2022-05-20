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

type LegacyCode =
    LegacyCode String

toLegacyCode : String -> LegacyCode
toLegacyCode s =
    LegacyCode s

fromLegacyCode : LegacyCode -> String
fromLegacyCode lc =
    case lc of
        LegacyCode s ->
            s

type alias Language =
    { contentCode : ContentCode
    , displayName : String
    , legacyCode : LegacyCode
    }

contentCodeStringFromLanguage : Language -> String
contentCodeStringFromLanguage language =
    fromContentCode language.contentCode

displayNameFromLanguage : Language -> String
displayNameFromLanguage language =
    language.displayName

legacyCodeStringFromLanguage : Language -> String
legacyCodeStringFromLanguage language =
    fromLegacyCode language.legacyCode

type alias Languages =
    List Language

type alias Model =
    { languages : Languages
    , dictLanguages : Dict String Language
    }

contentCodeDecoder : Decoder ContentCode
contentCodeDecoder =
    map ContentCode (field "content_code" string)

legacyCodeDecoder : Decoder LegacyCode
legacyCodeDecoder =
    map LegacyCode (field "legacy_code" string)

languageDecoder : Decoder Language
languageDecoder =
    Json.Decode.succeed Language
        |> custom contentCodeDecoder
        |> required "display" string
        |> custom legacyCodeDecoder

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
    { contentCode = ( ContentCode "" ), displayName = "", legacyCode = ( LegacyCode "" ) }

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
            { contentCode = toContentCode cc, displayName = "", legacyCode = toLegacyCode "" }

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
