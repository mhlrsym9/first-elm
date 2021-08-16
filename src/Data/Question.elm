module Data.Question exposing (encodeQuestion, init, Model, questionDecoder)

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
        [ ( "display", Encode.string "none" )
        , ( "question", Encode.string (textToString text) )
        , ( "answersarea", AnswersArea.encodeAnswersArea answersArea )
        ]

textToString: Text -> String
textToString (Text val) =
    val

init : Model
init =
    { text = Text "This is a sample question"
    , answersArea = AnswersArea.init
    }