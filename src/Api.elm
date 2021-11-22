module Api exposing (BytesWithHeaders, handleBytesResponse, handleJsonResponse, Status(..))

import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Http
import Json.Decode exposing (Decoder)

type Status a
    = Failed
    | Loading a
    | LoadingSlowly a
    | Loaded a

handleResponse : (Http.Metadata -> body -> Result Http.Error a) -> Http.Response body -> Result Http.Error a
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

        Http.GoodStatus_ metadata body ->
            fnc metadata body

handleTheJsonResponse : Decoder a -> Http.Metadata -> String -> Result Http.Error a
handleTheJsonResponse decoder _ body =
    case Json.Decode.decodeString decoder body of
        Err err ->
            Err (Http.BadBody (Json.Decode.errorToString err))

        Ok result ->
            Ok result

handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    handleResponse (handleTheJsonResponse decoder) response

type alias BytesWithHeaders =
    { headers : Dict String String
    , bytes : Bytes
    }

handleTheBytesResponse : (BytesWithHeaders -> Result String a) -> Http.Metadata -> Bytes -> Result Http.Error a
handleTheBytesResponse toResult { headers } bytes =
    Result.mapError Http.BadBody (toResult { headers = headers, bytes = bytes } )

handleBytesResponse : (BytesWithHeaders -> Result String a) -> Http.Response Bytes -> Result Http.Error a
handleBytesResponse toResult response =
    handleResponse (handleTheBytesResponse toResult) response

