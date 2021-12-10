module Data.QuestionsArea exposing (encodeQuestionsArea, establishIndexes, init, Model, Msg(..), questionsAreaDecoder, update, updateSlideIndexes, view)

import Data.ProjectHelpers as ProjectHelpers
import Data.Question as Question
import Dict exposing (Dict)
import Element exposing (centerX, Column, column, el, Element, fill, IndexedColumn, indexedTable, padding, spacing, table)
import Element.Font as Font
import Element.Input as Input
import Json.Decode exposing (Decoder, int, succeed)
import Json.Decode.Extra exposing (indexedList)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode
import Task exposing (Task)
import UIHelpers exposing (buttonAttributes)

type alias Model =
    { questionIndex : Int
    , slideIndex : Int
    , questions : Dict Int Question.Model
    }

questionDecoder : Int -> Decoder (Int, Question.Model)
questionDecoder index =
    Question.questionDecoder
        |> Json.Decode.map (\q -> (index, q))

questionsDecoder : Decoder (Dict Int Question.Model)
questionsDecoder =
    (indexedList questionDecoder)
        |> Json.Decode.map (\l -> Dict.fromList l)

questionsAreaDecoder : Decoder Model
questionsAreaDecoder =
    succeed Model
        |> required "questionIndex" int
        |> hardcoded 0
        |> required "questions" questionsDecoder

encodeQuestionsArea : Model -> Encode.Value
encodeQuestionsArea { questionIndex, questions } =
    Encode.object
        [ ( "questionIndex", Encode.int questionIndex )
        , ( "questions", questions |> Dict.values |> Encode.list Question.encodeQuestion )
        ]

init : { slideIndex : Int } -> Model
init ( { slideIndex } ) =
    let
        questionModel = Question.init { questionIndex = 0, slideIndex = slideIndex }
    in
    { questionIndex = 0
    , slideIndex = slideIndex
    , questions = Dict.singleton 0 questionModel
    }

updateQuestionIndexes : Dict Int Question.Model -> Dict Int Question.Model
updateQuestionIndexes questions =
    Dict.map Question.updateQuestionIndex questions

updateSlideIndexes : Int -> Model -> Model
updateSlideIndexes slideIndex ( { questions } as model ) =
    {
        model
            | slideIndex = slideIndex
            , questions = Dict.map (Question.updateSlideIndex slideIndex) questions
    }

establishIndexes : Int -> Model -> Model
establishIndexes slideIndex ( { questions } as model ) =
    {
        model
            | slideIndex = slideIndex
            , questions = Dict.map (Question.establishIndexes slideIndex) questions
    }

-- UPDATE

type Msg =
    Add
    | Delete Int
    | MakeDirty
    | Move Int ProjectHelpers.Direction
    | QuestionMsg Int Question.Msg

updateQuestion : Int -> Question.Msg -> Model -> (Model, Cmd Msg)
updateQuestion index questionMsg ( { questions } as model ) =
    let
        maybeQuestion = Dict.get index questions
        (updatedModel, commands) =
            case maybeQuestion of
                Just question ->
                    let
                        (updatedQuestion, questionCommands)
                            = Question.update questionMsg question
                    in
                    ( { model | questions = Dict.insert index updatedQuestion questions }
                    , questionCommands
                    )

                Nothing ->
                    ( model, Cmd.none )
    in
    ( updatedModel, Cmd.map (QuestionMsg index) commands )

makeProjectDirty : Cmd Msg
makeProjectDirty =
    Task.perform ( always MakeDirty ) ( Task.succeed () )

update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { slideIndex, questions } as model ) =
    case msg of
        Add ->
            let
                position = Dict.size questions
                questionModel = Question.init { questionIndex = position, slideIndex = slideIndex }
                updatedModel = { model | questions = Dict.insert (Dict.size questions) questionModel questions }
            in
            ( updatedModel, makeProjectDirty )

        Delete index ->
            ( { model | questions = ProjectHelpers.deleteEntry index updateQuestionIndexes questions }
            , makeProjectDirty
            )

-- Handled in Slide module
        MakeDirty ->
            ( model, Cmd.none )

        Move index ProjectHelpers.Top ->
            let
                updatedQuestions = ProjectHelpers.moveEntry
                    index ProjectHelpers.Increment 0
                    updateQuestionIndexes questions
            in
            ( { model | questions = updatedQuestions }
            , makeProjectDirty
            )

        Move index ProjectHelpers.Up ->
            let
                updatedQuestions = ProjectHelpers.flipAdjacentEntries
                    index ProjectHelpers.Decrement updateQuestionIndexes questions
            in
            ( { model | questions = updatedQuestions }
            , makeProjectDirty
            )

        Move index ProjectHelpers.Down ->
            let
                updatedQuestions = ProjectHelpers.flipAdjacentEntries
                    index ProjectHelpers.Increment updateQuestionIndexes questions
            in
            ( { model | questions = updatedQuestions }
            , makeProjectDirty
            )

        Move index ProjectHelpers.Bottom ->
            let
                finalIndex = ((Dict.size questions) - 1)
                updatedQuestions = ProjectHelpers.moveEntry
                    index ProjectHelpers.Decrement finalIndex
                    updateQuestionIndexes questions
            in
            ( { model | questions = updatedQuestions }
            , makeProjectDirty
            )

        QuestionMsg index questionMsg ->
            case questionMsg of
                Question.MakeDirty ->
                    ( model, makeProjectDirty )

                _ ->
                    updateQuestion index questionMsg model

-- VIEW

viewHeader : Element Msg
viewHeader =
    el
        [ Font.size 24
        , centerX
        ]
        (Element.text "Questions")

viewActionButtons : Element Msg
viewActionButtons =
    Input.button
        (centerX :: buttonAttributes)
        { onPress = Just Add
        , label = Element.text "Add Another Question"
        }

prepareColumnButton : (Int -> Element Msg) -> Column Int Msg
prepareColumnButton fnc =
    { header = Element.text ""
    , width = fill
    , view =
        \index ->
            fnc index
    }

viewMoveQuestionTopButton : Int -> Element Msg
viewMoveQuestionTopButton index =
    if (0 == index) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = Just (Move index ProjectHelpers.Top)
            , label = Element.text "Move Question to Top"
            }

viewMoveQuestionUpButton : Int -> Element Msg
viewMoveQuestionUpButton index =
    if (0 == index) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = Just (Move index ProjectHelpers.Up)
            , label = Element.text "Move Question Up"
            }

viewMoveQuestionDownButton : Int -> Int -> Element Msg
viewMoveQuestionDownButton numberQuestions index =
    if (index == (numberQuestions - 1)) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = Just (Move index ProjectHelpers.Down)
            , label = Element.text "Move Question Down"
            }

viewMoveQuestionBottomButton : Int -> Int -> Element Msg
viewMoveQuestionBottomButton numberQuestions index =
    if (index == (numberQuestions - 1)) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = Just (Move index ProjectHelpers.Bottom)
            , label = Element.text "Move Question to Bottom"
            }

viewDeleteButton : Int -> Int -> Element Msg
viewDeleteButton numberQuestions index =
    if (1 == numberQuestions) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = Just (Delete index)
            , label = Element.text "Delete This Question"
            }

viewQuestionsTableMoveButtons : Int -> Int -> Element Msg
viewQuestionsTableMoveButtons numberQuestions index =
    table
        [ spacing 10 ]
        { data = List.singleton index
        , columns =
            [ prepareColumnButton viewMoveQuestionTopButton
            , prepareColumnButton viewMoveQuestionUpButton
            , prepareColumnButton (viewMoveQuestionDownButton numberQuestions)
            , prepareColumnButton (viewMoveQuestionBottomButton numberQuestions)
            , prepareColumnButton (viewDeleteButton numberQuestions)
            ]
        }

viewQuestionsTableColumnArea : Int -> Int -> Question.Model -> Element Msg
viewQuestionsTableColumnArea numberQuestions index question =
    column
        [ ]
        [ (viewQuestionsTableMoveButtons numberQuestions index)
        , Question.view question
            |> Element.map (QuestionMsg index)
        ]

prepareQuestionsTableColumn : Int -> IndexedColumn Question.Model Msg
prepareQuestionsTableColumn numberQuestions =
    { header = Element.text ""
    , width = fill
    , view =
        \index question ->
            viewQuestionsTableColumnArea numberQuestions index question
    }

viewQuestionsTable : Model -> Element Msg
viewQuestionsTable { questions } =
    let
        numberQuestions = Dict.size questions
    in
    indexedTable
        [ spacing 10 ]
        { data = Dict.values questions
        , columns =
            [ prepareQuestionsTableColumn numberQuestions ]
        }

view : Model -> Element Msg
view ( { questions } as model ) =
    column
        [ Font.size 14
        , padding 10
        , spacing 10
        , centerX
        ]
        [ viewHeader
        , viewActionButtons
        , viewQuestionsTable model
        ]
