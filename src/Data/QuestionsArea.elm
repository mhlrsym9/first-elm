module Data.QuestionsArea exposing (encodeQuestionsArea, establishIndexes, init, Model, Msg, questionsAreaDecoder, update, view)

import Data.Question as Question
import Dict exposing (Dict)
import Html exposing (button, div, h2, Html, table, text, tr)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, int, succeed)
import Json.Decode.Extra exposing (indexedList)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode

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

init : (Model, Cmd Msg)
init =
    let
        (questionModel, questionCommands) =
            Question.init { questionIndex = 0, slideIndex = 0 }
    in
    (
        { questionIndex = 0
        , slideIndex = 0
        , questions = Dict.singleton 0 questionModel
        }
        , Cmd.map (QuestionMsg 0) questionCommands
    )

establishIndexes : Int -> Model -> Model
establishIndexes slideIndex ( { questions } as model ) =
    {
        model
            | slideIndex = slideIndex
            , questions = Dict.map (Question.establishIndexes slideIndex) questions
    }

-- UPDATE

type Direction =
    Up
    | Down
    | Top
    | Bottom

type Msg =
    Add
    | Delete Int
    | Move Int Direction
    | QuestionMsg Int Question.Msg

shiftIndexes : Int -> Int -> Model -> (Model, Cmd Msg)
shiftIndexes atIndex otherIndex ( { questions } as model ) =
    let
        maybeOther = Dict.get otherIndex questions
    in
    case maybeOther of
        Just other ->
            let
                maybeAt = Dict.get atIndex questions
            in
            case maybeAt of
                Just at ->
                    (
                        { model
                            | questions =
                                questions
                                    |> Dict.insert otherIndex at
                                    |> Dict.insert atIndex other
                        }
                        , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )

updateIndexes : Int -> Int -> Int -> Model -> (Model, Cmd Msg)
updateIndexes index shiftBy finalIndex ( { questions } as model ) =
    let
        maybeAt = Dict.get index questions
    in
    case maybeAt of
        Just at ->
            (
                { model
                    | questions =
                        questions
                            |> Dict.remove index
                            |> Dict.toList
                            |> List.map (\(i, q) -> ( (i + shiftBy), q ))
                            |> Dict.fromList
                            |> Dict.insert finalIndex at
                }
                , Cmd.none
            )

        Nothing ->
            ( model, Cmd.none )

updateQuestion : Int -> Question.Msg -> Model -> (Model, Cmd Msg)
updateQuestion index questionMsg ( { questions } as model ) =
    let
        maybeQuestion = Dict.get index questions
        (updatedModel, commands) =
            case maybeQuestion of
                Just question ->
                    let
                        (updatedQuestion, questionCommands) =
                            Question.update questionMsg question
                    in
                    ( { model | questions = Dict.insert index updatedQuestion questions }
                    , Cmd.map (QuestionMsg index) questionCommands
                    )

                Nothing ->
                    ( model, Cmd.none )
    in
        ( updatedModel, commands )


update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { slideIndex, questions } as model ) =
    case msg of
        Add ->
            let
                position = Dict.size questions
                (questionModel, questionCommands) =
                    Question.init { questionIndex = position, slideIndex = slideIndex }
            in
            ( { model | questions = Dict.insert (Dict.size questions) questionModel questions }
            , Cmd.map (QuestionMsg position) questionCommands
            )

        Delete _ ->
            ( model, Cmd.none )

        QuestionMsg index questionMsg ->
            updateQuestion index questionMsg model

        Move index Up ->
            shiftIndexes index (index - 1) model

        Move index Down ->
            shiftIndexes index (index + 1) model

        Move index Top ->
            updateIndexes index 1 0 model

        Move index Bottom ->
            updateIndexes index -1 ((Dict.size questions) - 1) model

-- VIEW

viewHeader : Html Msg
viewHeader =
    h2
        [ class "edit-page-questions-header" ]
        [ text "Questions" ]

viewActionButtons : Html Msg
viewActionButtons =
    button
        [ class "edit-page-questions-action-buttons"
        , onClick Add
        ]
        [ text "Add Another Question" ]

viewMoveQuestionTopButton : Int -> Html Msg
viewMoveQuestionTopButton index =
    button
        [ onClick (Move index Top)
        , disabled (0 == index)
        ]
        [ text "Move Question to Top" ]

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
        , disabled ( index == (numberQuestions - 1) )
        ]
        [ text "Move Question Down" ]

viewMoveQuestionBottomButton : Int -> Int -> Html Msg
viewMoveQuestionBottomButton index numberQuestions =
    button
        [ onClick (Move index Bottom)
        , disabled ( index == (numberQuestions - 1) )
        ]
        [ text "Move Question to Bottom" ]

viewDeleteButton : Int -> Int -> Html Msg
viewDeleteButton index numberQuestions =
    button
        [ onClick (Delete index)
        , disabled ( 1 == numberQuestions )
        ]
        [ text "Delete This Question" ]

viewQuestionTableRowEntry : Int -> Int -> Question.Model -> List (Html Msg) -> List (Html Msg)
viewQuestionTableRowEntry numberQuestions index question l =
    let
        entry =
            tr
                [ ]
                [ viewMoveQuestionTopButton index
                , viewMoveQuestionUpButton index
                , viewMoveQuestionDownButton index numberQuestions
                , viewMoveQuestionBottomButton index numberQuestions
                , viewDeleteButton index numberQuestions
                , Question.view question
                    |> Html.map (QuestionMsg index)
                ]
    in
    List.append l (List.singleton entry)

viewQuestionsTable : Model -> Html Msg
viewQuestionsTable { questions } =
    Dict.foldl (viewQuestionTableRowEntry (Dict.size questions)) [ ] questions
        |> table [ class "edit-page-questions-table" ]

view : Model -> Html Msg
view ( { questions } as model ) =
    div
        [ class "edit-page-questions-area" ]
        [ viewHeader
        , viewActionButtons
        , viewQuestionsTable model
        ]
