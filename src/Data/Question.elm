module Data.Question exposing (encodeQuestion, Model, questionDecoder)

import Data.AnswersArea as AnswersArea
import Json.Decode exposing (Decoder, field, map, string, succeed)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode

-- MODEL

type Text =
    Text String

type alias Model =
    { text : Text
    , answersArea : AnswersArea.Model
    }

questionDecoder : Decoder Model
questionDecoder =
    succeed Model
        |> custom questionTextDecoder
        |> required "answersarea" AnswersArea.answersAreaDecoder

questionTextDecoder : Decoder Text
questionTextDecoder =
    map Text (field "question" string)

encodeQuestion : Model -> Encode.Value
encodeQuestion { text, answersArea } =
    Encode.object
        [ ( "question", Encode.string (textToString text) )
        , ( "answersArea", AnswersArea.encodeAnswersArea answersArea )
        ]

textToString: Text -> String
textToString (Text val) =
    val