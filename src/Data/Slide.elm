module Data.Slide exposing (encodeSlide, init, Model, slideDecoder, textToString)

import Data.QuestionsArea as QuestionsArea
import Json.Decode exposing (Decoder, field, map, string, succeed)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode

-- MODEL

type Text =
    Text String

type alias Model =
    { text : Text
    , questionsArea : QuestionsArea.Model
    }

slideDecoder : Decoder Model
slideDecoder =
    succeed Model
        |> custom slideTextDecoder
        |> required "questionsarea" QuestionsArea.questionsAreaDecoder

encodeSlide : Model -> Encode.Value
encodeSlide { text, questionsArea } =
    Encode.object
        [ ( "slide", Encode.string (textToString text) )
        , ( "questionsArea", QuestionsArea.encodeQuestionsArea questionsArea )
        ]

slideTextDecoder : Decoder Text
slideTextDecoder =
    map Text (field "slide" string)

textToString : Text -> String
textToString (Text val) =
    val

init : Model
init =
    { text = Text "This is a test"
    , questionsArea = QuestionsArea.init
    }