module Flags exposing (Flags, init, Model, updateSeeds, versionString)

import Json.Decode exposing (decodeValue, Decoder, Error, int, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Random
import UUID exposing (Seeds)

type alias Flags =
    { setupEditorName : String
    , editorConfigName : String
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
    , editorConfigName : String
    , candorUrl : String
    , loadingPath : String
    , decodedMetadata : Result Error DecodedMetadata
    , seeds : Seeds
    }

metadataDecoder : Decoder DecodedMetadata
metadataDecoder =
    succeed DecodedMetadata
        |> required "buildMajor" int
        |> required "buildMinor" int
        |> required "buildRevision" int
        |> required "buildTag" string

initialSeeds : List Int -> Seeds
initialSeeds l =
    case l of
        a :: b :: c :: d :: _ ->
            (Seeds
                (Random.initialSeed a)
                (Random.initialSeed b)
                (Random.initialSeed c)
                (Random.initialSeed d)
            )

        _ ->
            (Seeds
                (Random.initialSeed 12345)
                (Random.initialSeed 23456)
                (Random.initialSeed 34567)
                (Random.initialSeed 45678)
            )

init : Flags -> Model
init { setupEditorName, editorConfigName, candorUrl, loadingPath, metadata, seeds } =
    { setupEditorName = setupEditorName
    , editorConfigName = editorConfigName
    , candorUrl = candorUrl
    , loadingPath = loadingPath
    , decodedMetadata = decodeValue metadataDecoder metadata
    , seeds = initialSeeds seeds
    }

updateSeeds : Model -> Seeds -> Model
updateSeeds model updatedSeeds =
    { model | seeds = updatedSeeds }

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
