module Data.AnswersArea exposing (answersAreaDecoder, encodeAnswersArea, establishIndexes, init, Model, Msg, parseOptRadio, update, updateQuestionIndexes, updateSlideIndexes, view)

import Data.ProjectHelpers as ProjectHelpers
import Dict exposing (Dict)
import Html exposing (Html, button, div, h3, input, label, table, text, tr)
import Html.Attributes exposing (checked, class, disabled, for, id, name, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, field, map, string, succeed)
import Json.Decode.Extra exposing (indexedList)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import Json.Encode as Encode
import Parser exposing ((|.), (|=), end, int, Parser, run, symbol, token)

type OptRadio =
    OptRadio String

type Answer =
    Answer String

type alias Model =
    { slideIndex : Int
    , questionIndex : Int
    , optRadio : OptRadio
    , answers : Dict Int Answer
    }

answerDecoder : Int -> Decoder (Int, Answer)
answerDecoder index =
    map Answer string
        |> Json.Decode.map (\q -> (index, q))

answersDecoder : Decoder (Dict Int Answer)
answersDecoder =
    (indexedList answerDecoder)
        |> Json.Decode.map (\l -> Dict.fromList l)

answersAreaDecoder : Decoder Model
answersAreaDecoder =
    succeed Model
        |> hardcoded 0
        |> hardcoded 0
        |> custom optRadioDecoder
        |> required "answers" answersDecoder

optRadioDecoder : Decoder OptRadio
optRadioDecoder =
    map OptRadio (field "optradio" string)

encodeAnswer : Answer -> Encode.Value
encodeAnswer answer =
    Encode.string (answerToString answer)

encodeAnswersArea : Model -> Encode.Value
encodeAnswersArea { optRadio, answers } =
    Encode.object
        [ ( "optradio", Encode.string (optRadioToString optRadio) )
        , ( "answers", answers |> Dict.values |> Encode.list encodeAnswer)
        ]

optRadioToString : OptRadio -> String
optRadioToString (OptRadio val) =
    val

answerToString : Answer -> String
answerToString (Answer val) =
   val

sampleAnswer : Answer
sampleAnswer =
    Answer "This is a sample answer"

getCandorAnswerHeaderFromIndexes : Indexes -> String
getCandorAnswerHeaderFromIndexes { slideIndex, questionIndex } =
    "candor_answer_" ++ (String.fromInt slideIndex) ++ "_" ++ (String.fromInt questionIndex)

getCandorAnswerHeader : Model -> String
getCandorAnswerHeader { slideIndex, questionIndex } =
    getCandorAnswerHeaderFromIndexes { slideIndex = slideIndex, questionIndex = questionIndex, answerIndex = 0 }

getCandorAnswerFromIndexes : Indexes -> String
getCandorAnswerFromIndexes ( { answerIndex } as indexes ) =
    (getCandorAnswerHeaderFromIndexes indexes) ++ "_" ++ (String.fromInt answerIndex)

getCandorAnswer: Model -> Int -> String
getCandorAnswer model answerIndex =
    (getCandorAnswerHeader model) ++ "_" ++ (String.fromInt answerIndex)

getOptRadio : Model -> Int -> OptRadio
getOptRadio model answerIndex =
    OptRadio (getCandorAnswer model answerIndex)

init : { slideIndex : Int, questionIndex : Int } -> Model
init ( { slideIndex, questionIndex } ) =
    { slideIndex = slideIndex
    , questionIndex = questionIndex
    , optRadio = OptRadio (getCandorAnswerFromIndexes { slideIndex = slideIndex, questionIndex = questionIndex, answerIndex = 0} )
    , answers = Dict.singleton 0 sampleAnswer
    }

type alias Indexes = { slideIndex : Int, questionIndex : Int, answerIndex : Int }

parseOptRadio : Parser Indexes
parseOptRadio =
    Parser.succeed Indexes
        |. token "candor"
        |. symbol "_"
        |. token "answer"
        |. symbol "_"
        |= int
        |. symbol "_"
        |= int
        |. symbol "_"
        |= int
        |. end

establishOptRadio : Int -> Int -> Model -> OptRadio
establishOptRadio updatedSlideIndex updatedQuestionIndex { optRadio } =
    let
        possibleIndexes = run parseOptRadio (optRadioToString optRadio)
    in
    case possibleIndexes of
        Ok index ->
            OptRadio
                ( getCandorAnswerFromIndexes
                    { slideIndex = updatedSlideIndex
                    , questionIndex = updatedQuestionIndex
                    , answerIndex = index.answerIndex
                    }
                )
        Err _ ->
            optRadio

updateQuestionIndexes : Int -> Model -> Model
updateQuestionIndexes questionIndex ( { slideIndex } as model ) =
    {
        model
            | questionIndex = questionIndex
            , optRadio = establishOptRadio slideIndex questionIndex model
    }

updateSlideIndexes : Int -> Model -> Model
updateSlideIndexes slideIndex ( { questionIndex } as model ) =
    {
        model
            | slideIndex = slideIndex
            , optRadio = establishOptRadio slideIndex questionIndex model
    }

establishIndexes : Int -> Int -> Model -> Model
establishIndexes slideIndex questionIndex ( { optRadio, answers } as model ) =
    {
        model
            | slideIndex = slideIndex
            , questionIndex = questionIndex
            , optRadio = establishOptRadio slideIndex questionIndex model
    }

-- UPDATE

type Msg =
    Add
    | Delete Int
    | Move Int ProjectHelpers.Direction
    | Update Int String

matchAnswerIndexToUpdatedOptRadio : OptRadio -> Dict Int OptRadio -> OptRadio
matchAnswerIndexToUpdatedOptRadio ((OptRadio or) as originalOptRadio) updatedOptRadiosDict =
    let
        maybeAnswer = run parseOptRadio or
    in
    case maybeAnswer of
        Ok answer ->
            let
                maybeOptRadio = Dict.get answer.answerIndex updatedOptRadiosDict
            in
            case maybeOptRadio of
                Just updatedOptRadio ->
                    updatedOptRadio
                Nothing ->
                    originalOptRadio
        Err _ ->
            originalOptRadio

updateOptRadioDuringDelete : Model -> Int -> OptRadio
updateOptRadioDuringDelete ( { answers, optRadio } as model ) answerIndex =
    let
        getThisCandorAnswer = getOptRadio model
        (keepSameIndex, decrementIndex) =
            answers
                |> Dict.remove answerIndex
                |> Dict.partition (\i _ -> (i < answerIndex))
        updatedOptRadios = List.foldl Dict.union Dict.empty
            [ Dict.singleton answerIndex (getThisCandorAnswer 0)
            , Dict.foldl (\i _ d -> Dict.insert i (getThisCandorAnswer i) d) Dict.empty keepSameIndex
            , Dict.foldl (\i _ d -> Dict.insert i (getThisCandorAnswer (i - 1)) d) Dict.empty decrementIndex
            ]
    in
    matchAnswerIndexToUpdatedOptRadio optRadio updatedOptRadios

updateOptRadioDuringFlip : Model -> Int -> ProjectHelpers.IndexAdjustment -> OptRadio
updateOptRadioDuringFlip ( { answers, optRadio } as model ) answerIndex adjustment =
    let
        getThisCandorAnswer = getOptRadio model
        otherAnswerIndex = ProjectHelpers.adjustIndex adjustment answerIndex
        updatedOptRadios = List.foldl Dict.union Dict.empty
            [ Dict.singleton answerIndex (getThisCandorAnswer otherAnswerIndex)
            , Dict.singleton otherAnswerIndex (getThisCandorAnswer answerIndex)
            , answers
                |> Dict.remove answerIndex
                |> Dict.remove otherAnswerIndex
                |> Dict.foldl (\i _ d -> Dict.insert i (getThisCandorAnswer i) d) Dict.empty
            ]
    in
    matchAnswerIndexToUpdatedOptRadio optRadio updatedOptRadios

updateOptRadioDuringMove : Model -> Int -> ProjectHelpers.IndexAdjustment -> Int -> OptRadio
updateOptRadioDuringMove ( { answers, optRadio } as model ) answerIndex adjustment finalIndex =
    let
        getThisCandorAnswer = getOptRadio model
        (beforeIndex, afterIndex) =
            answers
                |> Dict.remove answerIndex
                |> Dict.partition (\i _ -> (i < answerIndex))
        adjustFnc = (\i _ d -> Dict.insert i (getThisCandorAnswer (ProjectHelpers.adjustIndex adjustment i)) d)
        identityFnc = (\i _ d -> Dict.insert i (getThisCandorAnswer i) d)
        beforeFnc =
            case adjustment of
                ProjectHelpers.Increment ->
                    adjustFnc

                ProjectHelpers.Decrement ->
                    identityFnc

        afterFnc =
            case adjustment of
                ProjectHelpers.Increment ->
                    identityFnc

                ProjectHelpers.Decrement ->
                    adjustFnc

        updatedOptRadios = List.foldl Dict.union Dict.empty
            [ Dict.singleton answerIndex (getThisCandorAnswer finalIndex)
            , Dict.foldl beforeFnc Dict.empty beforeIndex
            , Dict.foldl afterFnc Dict.empty afterIndex
            ]
    in
    matchAnswerIndexToUpdatedOptRadio optRadio updatedOptRadios

update : Msg -> Model -> Model
update msg ( { slideIndex, questionIndex, answers } as model ) =
    case msg of
        Add ->
            { model | answers = Dict.insert (Dict.size answers) sampleAnswer answers }

        Delete answerIndex ->
            { model
                | answers = ProjectHelpers.deleteEntry answerIndex identity answers
                , optRadio = updateOptRadioDuringDelete model answerIndex
            }

        Move index ProjectHelpers.Top ->
            let
                updatedAnswers = ProjectHelpers.moveEntry
                    index ProjectHelpers.Increment 0
                    identity answers
                updatedOptRadio = updateOptRadioDuringMove
                    model index ProjectHelpers.Increment 0
            in
            { model
                | answers = updatedAnswers
                , optRadio = updatedOptRadio
            }

        Move answerIndex ProjectHelpers.Up ->
            { model
                | answers = ProjectHelpers.flipAdjacentEntries
                    answerIndex ProjectHelpers.Decrement identity answers
                , optRadio = updateOptRadioDuringFlip model answerIndex ProjectHelpers.Decrement
            }

        Move answerIndex ProjectHelpers.Down ->
            { model
                | answers = ProjectHelpers.flipAdjacentEntries
                    answerIndex ProjectHelpers.Increment identity answers
                , optRadio = updateOptRadioDuringFlip model answerIndex ProjectHelpers.Increment
            }

        Move index ProjectHelpers.Bottom ->
            let
                finalIndex = (Dict.size answers) - 1
                updatedAnswers = ProjectHelpers.moveEntry
                    index ProjectHelpers.Decrement finalIndex
                    identity answers
                updatedOptRadio = updateOptRadioDuringMove
                    model index ProjectHelpers.Decrement finalIndex
            in
            { model
                | answers = updatedAnswers
                , optRadio = updatedOptRadio
            }

        Update index s ->
            { model | answers = Dict.insert index (Answer s) answers }

-- VIEW

viewHeader : Html Msg
viewHeader =
    h3
        [ class "edit-page-answers-header" ]
        [ text "Answers" ]

viewActionButtons : Html Msg
viewActionButtons =
    button
        [ class "edit-page-answers-action-buttons"
        , onClick Add
        ]
        [ text "Add Another Answer" ]

viewMoveAnswerTopButton : Int -> Html Msg
viewMoveAnswerTopButton index =
    button
        [ onClick (Move index ProjectHelpers.Top)
        , disabled (0 == index)
        ]
        [ text "Move Answer to Top" ]

viewMoveAnswerUpButton : Int -> Html Msg
viewMoveAnswerUpButton index =
    button
        [ onClick (Move index ProjectHelpers.Up)
        , disabled (0 == index)
        ]
        [ text "Move Answer Up" ]

viewMoveAnswerDownButton : Int -> Int -> Html Msg
viewMoveAnswerDownButton index numberAnswers =
    button
        [ onClick (Move index ProjectHelpers.Down)
        , disabled ( index == (numberAnswers - 1) )
        ]
        [ text "Move Answer Down" ]

viewMoveAnswerBottomButton : Int -> Int -> Html Msg
viewMoveAnswerBottomButton index numberAnswers =
    button
        [ onClick (Move index ProjectHelpers.Bottom)
        , disabled ( index == (numberAnswers - 1) )
        ]
        [ text "Move Answer to Bottom" ]

viewDeleteButton : Int -> Int -> Html Msg
viewDeleteButton index numberAnswers =
    button
        [ onClick (Delete index)
        , disabled ( 1 == numberAnswers )
        ]
        [ text "Delete This Answer" ]

viewIsCorrectRadioButton : Model -> Int -> Html Msg
viewIsCorrectRadioButton ( { optRadio } as model ) answerIndex =
    let
        nameValue = getCandorAnswerHeader model
        idValue = getCandorAnswer model answerIndex
    in
    label
        [ for idValue ]
        [
            input
                [ type_ "radio"
                , id idValue
                , name nameValue
                , value idValue
                , checked (optRadio == (OptRadio idValue))
                ]
                [ ]
            , text "is Correct?"
        ]

viewAnswerTableRowEntry : Model -> Int -> Answer -> List (Html Msg) -> List (Html Msg)
viewAnswerTableRowEntry ( { answers } as model ) answerIndex answer l =
    let
        numberAnswers = Dict.size answers
        entry =
            tr
                [ ]
                [ viewMoveAnswerTopButton answerIndex
                , viewMoveAnswerUpButton answerIndex
                , viewMoveAnswerDownButton answerIndex numberAnswers
                , viewMoveAnswerBottomButton answerIndex numberAnswers
                , viewDeleteButton answerIndex numberAnswers
                , viewIsCorrectRadioButton model answerIndex
                , input
                    [ type_ "text"
                    , value (answerToString answer)
                    , onInput (Update answerIndex)
                    ]
                    [ ]
                ]
    in
    List.append l (List.singleton entry)

viewAnswersTable : Model -> Html Msg
viewAnswersTable ( { answers } as model ) =
    Dict.foldl (viewAnswerTableRowEntry model) [ ] answers
        |> table [ class "edit-page-answers-table" ]

view : Model -> Html Msg
view model =
    div
        [ class "edit-page-answers" ]
        [ viewHeader
        , viewActionButtons
        , viewAnswersTable model
        ]
