module Data.AnswersArea exposing (answersAreaDecoder, encodeAnswersArea, establishIndexes, init, Model)

import Array exposing (Array)
import Json.Decode exposing (array, Decoder, field, map, string, succeed)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import Json.Encode as Encode

type OptRadio =
    OptRadio String

type Answer =
    Answer String

type alias Model =
    { slideIndex : Int
    , questionIndex : Int
    , optRadio : OptRadio
    , answers : Array Answer
    }

answersAreaDecoder : Decoder Model
answersAreaDecoder =
    succeed Model
        |> hardcoded 0
        |> hardcoded 0
        |> custom optRadioDecoder
        |> required "answers" (array answerDecoder)

optRadioDecoder : Decoder OptRadio
optRadioDecoder =
    map OptRadio (field "optradio" string)

answerDecoder : Decoder Answer
answerDecoder =
    map Answer string

encodeAnswersArea : Model -> Encode.Value
encodeAnswersArea { optRadio, answers } =
    Encode.object
        [ ( "optradio", Encode.string (optRadioToString optRadio) )
        , ( "answers", encodeAnswers answers)
        ]

optRadioToString : OptRadio -> String
optRadioToString (OptRadio val) =
    val

encodeAnswers : Array Answer -> Encode.Value
encodeAnswers answers =
    Encode.array answerToString answers

answerToString : Answer -> Encode.Value
answerToString (Answer val) =
    Encode.string val

init : (Model, Cmd msg)
init =
    (
        { slideIndex = 0
        , questionIndex = 0
        , optRadio = OptRadio "0_0_0"
        , answers = Array.repeat 1 (Answer "This is a sample answer")
        }
        , Cmd.none
    )

establishIndexes : Int -> Int -> Model -> Model
establishIndexes slideIndex questionIndex ( { answers } as model ) =
    {
        model
            | slideIndex = slideIndex
            , questionIndex = questionIndex
    }