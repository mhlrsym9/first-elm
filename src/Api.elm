module Api exposing (handleBytesResponse, handleJsonResponse, Status(..))

import Bytes exposing (Bytes)
import Http
import Json.Decode exposing (Decoder)

type Status a
    = Creating a
    | CreatingSlowly a
    | Failed
    | Loading
    | LoadingSlowly
    | Loaded a
    | Updating a
    | UpdatingSlowly a

handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Err err ->
                    Err (Http.BadBody (Json.Decode.errorToString err))

                Ok result ->
                    Ok result

handleBytesResponse : (Bytes -> Result String a) -> Http.Response Bytes -> Result Http.Error a
handleBytesResponse toResult response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            Result.mapError Http.BadBody (toResult body)

