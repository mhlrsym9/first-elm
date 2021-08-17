module Data.QuestionsArea exposing (encodeQuestionsArea, establishIndexes, init, Model, Msg, questionsAreaDecoder, update, view)

import Array exposing (Array)
import Data.Question as Question
import Html exposing (button, div, Html, table, text, tr)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Json.Decode exposing (array, Decoder, int, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode

type alias Model =
    { questionIndex : Int
    , slideIndex : Int
    , questionPositions : Array Int
    , questions : Array Question.Model
    }

questionsAreaDecoder : Decoder Model
questionsAreaDecoder =
    succeed Model
        |> required "questionIndex" int
        |> hardcoded 0
        |> hardcoded (Array.repeat 1 0)
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
        , questionPositions = Array.repeat 1 0
        , questions = Array.repeat 1 questionModel
        }
        , Cmd.map QuestionMsg questionCommands
    )

establishIndexes : Int -> Model -> Model
establishIndexes slideIndex ( { questions } as model ) =
    {
        model
            | slideIndex = slideIndex
            , questionPositions =
                ( Array.length questions - 1 )
                    |> List.range 0
                    |> Array.fromList
            , questions = Array.indexedMap (Question.establishIndexes slideIndex) questions
    }

-- UPDATE

type Direction =
    Up
    | Down

type Msg =
    Move Int Direction
    | QuestionMsg Question.Msg

shiftIndexes : Int -> Int -> Model -> (Model, Cmd Msg)
shiftIndexes atIndex otherIndex ( { questions, questionPositions } as model ) =
    let
        maybeOther = Array.get otherIndex questionPositions
    in
    case maybeOther of
        Just other ->
            let
                maybeAt = Array.get atIndex questionPositions
            in
            case maybeAt of
                Just at ->
                    (
                        { model
                            | questionPositions =
                                questionPositions
                                    |> Array.set otherIndex at
                                    |> Array.set atIndex other
                        }
                        , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )

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

        Move index Up ->
            shiftIndexes index (index - 1) model

        Move index Down ->
            shiftIndexes index (index + 1) model

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

viewQuestionTableRowEntry : Int -> Model -> Html Msg
viewQuestionTableRowEntry index { questionPositions, questions } =
    let
        maybePosition = Array.get index questionPositions
    in
    case maybePosition of
        Just position ->
            let
                numberQuestions = Array.length questions
                maybeQuestion = Array.get position questions
            in
            case maybeQuestion of
                Just question ->
                    tr
                        [ ]
                        [ viewMoveQuestionUpButton index
                        , viewMoveQuestionDownButton index numberQuestions
                        , Question.view question
                            |> Html.map QuestionMsg
                        ]

                Nothing ->
                    div [ ] [ ]

        Nothing ->
            div [ ] [ ]

view : Model -> Html Msg
view ( { questions } as model ) =
    let
        length = (Array.length questions) - 1
    in
    List.map (\i -> viewQuestionTableRowEntry i model)  ( List.range 0 length )
        |> table [ class "edit-page-questions-table" ]
