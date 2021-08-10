module Data.QuestionsArea exposing (encodeQuestionsArea, Model, questionsAreaDecoder)

import Array exposing (Array)
import Data.Question as Question
import Json.Decode exposing (array, Decoder, int, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode

type alias Model =
    { questionIndex : Int
    , questions : Array Question.Model
    }

questionsAreaDecoder : Decoder Model
questionsAreaDecoder =
    succeed Model
        |> required "questionIndex" int
        |> required "questions" (array Question.questionDecoder)

encodeQuestionsArea : Model -> Encode.Value
encodeQuestionsArea { questionIndex, questions } =
    Encode.object
        [ ( "questionIndex", Encode.int questionIndex )
        , ( "questions", Encode.array Question.encodeQuestion questions )
        ]