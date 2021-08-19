module Data.AnswersArea exposing (answersAreaDecoder, encodeAnswersArea, establishIndexes, init, Model, Msg, update, view)

import Array exposing (Array)
import Html exposing (Html, button, div, h3, input, table, text, textarea, tr)
import Html.Attributes exposing (class, disabled, type_, value)
import Html.Events exposing (onClick, onInput)
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
    Encode.array encodeAnswer answers

encodeAnswer : Answer -> Encode.Value
encodeAnswer answer =
    Encode.string (answerToString answer)

answerToString : Answer -> String
answerToString (Answer val) =
   val

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

-- UPDATE

type Msg =
    Delete

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    ( model, Cmd.none )

-- VIEW

viewHeader : Html Msg
viewHeader =
    h3
        [ class "edit-page-answers-header" ]
        [ text "Answers" ]

viewActionButtons : Html Msg
viewActionButtons =
    div
        [ ]
        [ ]

viewAnswerTableRowEntry : Int -> Answer -> List (Html Msg) -> List (Html Msg)
viewAnswerTableRowEntry numberAnswers answer l =
    let
        entry =
            tr
                [ ]
                [ text (answerToString answer) ]
    in
    List.append l (List.singleton entry)

viewAnswersTable : Model -> Html Msg
viewAnswersTable { answers } =
    Array.foldl (viewAnswerTableRowEntry (Array.length answers)) [ ] answers
        |> table [ class "edit-page-questions-table" ]

view : Model -> Html Msg
view model =
    div
        [ class "edit-page-answers" ]
        [ viewHeader
        , viewActionButtons
        , viewAnswersTable model
        ]
