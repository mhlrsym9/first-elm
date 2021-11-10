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

handleResponse : (body -> Result Http.Error a) -> Http.Response body -> Result Http.Error a
handleResponse fnc response =
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
            fnc body

handleTheJsonResponse : Decoder a -> String -> Result Http.Error a
handleTheJsonResponse decoder body =
    case Json.Decode.decodeString decoder body of
        Err err ->
            Err (Http.BadBody (Json.Decode.errorToString err))

        Ok result ->
            Ok result

handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    handleResponse (handleTheJsonResponse decoder) response

handleTheBytesResponse : (Bytes -> Result String a) -> Bytes -> Result Http.Error a
handleTheBytesResponse toResult body =
    Result.mapError Http.BadBody (toResult body)

handleBytesResponse : (Bytes -> Result String a) -> Http.Response Bytes -> Result Http.Error a
handleBytesResponse toResult response =
    handleResponse (handleTheBytesResponse toResult) response

