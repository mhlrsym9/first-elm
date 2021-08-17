module Data.QuestionsArea exposing (encodeQuestionsArea, establishIndexes, init, Model, Msg, questionsAreaDecoder, update, view)

import Array exposing (Array)
import Data.Question as Question
import Html exposing (button, Html, table, text, tr)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Json.Decode exposing (array, Decoder, int, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode

type alias Model =
    { questionIndex : Int
    , slideIndex : Int
    , questions : Array Question.Model
    }

questionsAreaDecoder : Decoder Model
questionsAreaDecoder =
    succeed Model
        |> required "questionIndex" int
        |> hardcoded 0
        |> required "questions" (array Question.questionDecoder)

encodeQuestionsArea : Model -> Encode.Value
encodeQuestionsArea { questionIndex, questions } =
    Encode.object
        [ ( "questionIndex", Encode.int questionIndex )
        , ( "questions", Encode.array Question.encodeQuestion questions )
        ]

init : (Model, Cmd Msg)
init =
    let
        (questionModel, questionCommands) =
            Question.init { questionIndex = 0, slideIndex = 0 }
    in
    (
        { questionIndex = 0
        , slideIndex = 0
        , questions = Array.repeat 1 questionModel
        }
        , Cmd.map QuestionMsg questionCommands
    )

establishIndexes : Int -> Model -> Model
establishIndexes slideIndex ( { questions } as model ) =
    {
        model
            | slideIndex = slideIndex
            , questions = Array.indexedMap (Question.establishIndexes slideIndex) questions
    }

-- UPDATE

type Direction =
    Up
    | Down

type Msg =
    Move Int Direction
    | QuestionMsg Question.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { questions } as model ) =
    case msg of
        QuestionMsg questionMsg ->
            case questionMsg of
                Question.Update index _ ->
                    let
                        maybeQuestion = Array.get index questions
                        (updatedModel, commands) =
                            case maybeQuestion of
                                Just question ->
                                    let
                                        (updatedQuestion, questionCommands) =
                                            Question.update questionMsg question
                                    in
                                    ( { model | questions = Array.set index updatedQuestion questions }
                                    , Cmd.map QuestionMsg questionCommands
                                    )

                                Nothing ->
                                    (model, Cmd.none)
                    in
                        ( updatedModel, commands )

        Move index direction ->
            ( model, Cmd.none )

viewMoveQuestionUpButton : Int -> Html Msg
viewMoveQuestionUpButton index =
    button
        [ onClick (Move index Up)
        , disabled (0 == index)
        ]
        [ text "Move Question Up" ]

viewMoveQuestionDownButton : Int -> Int -> Html Msg
viewMoveQuestionDownButton index numberQuestions =
    button
        [ onClick (Move index Down)
        , disabled (index == (numberQuestions - 1) )
        ]
        [ text "Move Question Down" ]

viewQuestionTableRowEntry : Int -> Int -> Question.Model -> Html Msg
viewQuestionTableRowEntry numberQuestions index question =
    tr
        [ ]
        [ viewMoveQuestionUpButton index
        , viewMoveQuestionDownButton index numberQuestions
        , Question.view question
            |> Html.map QuestionMsg
        ]

view : Model -> Html Msg
view { questions } =
    questions
        |> Array.indexedMap (questions |> Array.length |> viewQuestionTableRowEntry)
        |> Array.toList
        |> table [ class "edit-page-questions-table" ]
