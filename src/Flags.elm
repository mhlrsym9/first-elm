module Flags exposing (Flags, init, Model, versionString)

import Json.Decode exposing (decodeValue, Decoder, Error, int, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Random

type alias Flags =
    { setupEditorName : String
    , candorUrl : String
    , loadingPath : String
    , metadata : Json.Decode.Value
    , seeds : List Int
    }

type alias DecodedMetadata =
    { buildMajor: Int
    , buildMinor : Int
    , buildRevision : Int
    , buildTag : String
    }

type alias Model =
    { setupEditorName : String
    , candorUrl : String
    , loadingPath : String
    , decodedMetadata : Result Error DecodedMetadata
    , seeds : List Random.Seed
    }

metadataDecoder : Decoder DecodedMetadata
metadataDecoder =
    succeed DecodedMetadata
        |> required "buildMajor" int
        |> required "buildMinor" int
        |> required "buildRevision" int
        |> required "buildTag" string

init : Flags -> Model
init { setupEditorName, candorUrl, loadingPath, metadata, seeds } =
    { setupEditorName = setupEditorName
    , candorUrl = candorUrl
    , loadingPath = loadingPath
    , decodedMetadata = decodeValue metadataDecoder metadata
    , seeds = List.map Random.initialSeed seeds
    }

versionString : Model -> String
versionString { decodedMetadata } =
    case decodedMetadata of
        Ok { buildMajor, buildMinor, buildRevision, buildTag } ->
            String.fromInt buildMajor
            ++ "."
            ++ String.fromInt buildMinor
            ++ "."
            ++ String.fromInt buildRevision
            ++ " "
            ++ buildTag
        Err _ ->
            "Version information unavailable"
