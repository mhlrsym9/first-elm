module FontHelpers exposing (..)

import Api
import Http exposing (stringResolver)
import Json.Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)
import LanguageHelpers
import Task exposing (Task)
import Url.Builder as Builder

type alias Font =
    { maxSize : Int
    , minSize : Int
    , ttf : String
    }

type alias LanguageFonts =
    { known : Font
    , learning : Font
    }

languageFontDecoder : Decoder Font
languageFontDecoder =
    Json.Decode.succeed Font
        |> required "max_size" int
        |> required "min_size" int
        |> required "ttf" string

languageFontsDecoder : Decoder LanguageFonts
languageFontsDecoder =
    Json.Decode.succeed LanguageFonts
        |> required "known" languageFontDecoder
        |> required "learn" languageFontDecoder

fetchLanguageFonts : LanguageHelpers.Language -> LanguageHelpers.Language -> Task Http.Error LanguageFonts
fetchLanguageFonts kl ll =
    let
        kcc = LanguageHelpers.legacyCodeStringFromLanguage kl
        lcc = LanguageHelpers.legacyCodeStringFromLanguage ll
        url = Builder.relative ["https://lds.transparent.com/", "font", "info", kcc, lcc] []
    in
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = stringResolver (Api.handleJsonResponse languageFontsDecoder)
        , timeout = Nothing
        }
