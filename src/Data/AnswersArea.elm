module Data.AnswersArea exposing (answersAreaDecoder, encodeAnswersArea, establishIndexes, init, Model, Msg, parseOptRadio, update, view)

import Data.ProjectHelpers as ProjectHelpers
import Dict exposing (Dict)
import Html exposing (Html, button, div, h3, input, label, table, text, tr)
import Html.Attributes exposing (checked, class, disabled, for, id, name, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, field, map, string, succeed)
import Json.Decode.Extra exposing (indexedList)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import Json.Encode as Encode
import Parser exposing ((|.), (|=), end, int, keyword, Parser, run, symbol, token)

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

getCandorAnswer: Int -> Int -> Int -> String
getCandorAnswer slideIndex questionIndex answerIndex =
    (getCandorAnswerHeader slideIndex questionIndex) ++ "_" ++ (String.fromInt answerIndex)

getOptRadio : Int -> Int -> Int -> OptRadio
getOptRadio slideIndex questionIndex answerIndex =
    OptRadio (getCandorAnswer slideIndex questionIndex answerIndex)

getCandorAnswerHeader : Int -> Int -> String
getCandorAnswerHeader slideIndex questionIndex =
    "candor_answer_" ++ (String.fromInt slideIndex) ++ "_" ++ (String.fromInt questionIndex)

getCandorAnswerHeaderFromModel : Model -> String
getCandorAnswerHeaderFromModel { slideIndex, questionIndex } =
    getCandorAnswerHeader slideIndex questionIndex

init : (Model, Cmd msg)
init =
    (
        { slideIndex = 0
        , questionIndex = 0
        , optRadio = OptRadio "0_0_0"
        , answers = Dict.singleton 0 sampleAnswer
        }
        , Cmd.none
    )

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
establishOptRadio updatedSlideIndex updatedQuestionIndex { slideIndex, questionIndex, optRadio } =
    let
        possibleIndexes = run parseOptRadio (optRadioToString optRadio)
    in
    case possibleIndexes of
        Ok index ->
            OptRadio (getCandorAnswer updatedSlideIndex updatedQuestionIndex index.answerIndex)
        Err _ ->
            optRadio

establishAnswerIndexes : Int -> Int -> Dict Int Answer -> Dict Int Answer
establishAnswerIndexes _ _ answers =
    answers

establishIndexes : Int -> Int -> Model -> Model
establishIndexes slideIndex questionIndex ( { optRadio, answers } as model ) =
    {
        model
            | slideIndex = slideIndex
            , questionIndex = questionIndex
            , optRadio = establishOptRadio slideIndex questionIndex model
            , answers = establishAnswerIndexes slideIndex questionIndex answers
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

updateOptRadioDuringDelete : Int -> Int -> Int -> OptRadio -> Dict Int b -> OptRadio
updateOptRadioDuringDelete slideIndex questionIndex answerIndex originalOptRadio answers =
    let
        getThisCandorAnswer = getOptRadio slideIndex questionIndex
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
    matchAnswerIndexToUpdatedOptRadio originalOptRadio updatedOptRadios

updateOptRadioDuringFlip : Int -> Int -> Int -> ProjectHelpers.IndexAdjustment -> OptRadio -> Dict Int b -> OptRadio
updateOptRadioDuringFlip slideIndex questionIndex answerIndex adjustment originalOptRadio answers =
    let
        getThisCandorAnswer = getOptRadio slideIndex questionIndex
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
    matchAnswerIndexToUpdatedOptRadio originalOptRadio updatedOptRadios

updateOptRadioDuringMove : Int -> Int -> Int -> ProjectHelpers.IndexAdjustment -> Int -> OptRadio -> Dict Int b -> OptRadio
updateOptRadioDuringMove slideIndex questionIndex answerIndex adjustment finalIndex originalOptRadio answers =
    let
        getThisCandorAnswer = getOptRadio slideIndex questionIndex
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
    matchAnswerIndexToUpdatedOptRadio originalOptRadio updatedOptRadios

update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { slideIndex, questionIndex, optRadio, answers } as model ) =
    let
        establishIndexesFnc = establishAnswerIndexes slideIndex questionIndex
    in
    case msg of
        Add ->
            ( { model | answers = Dict.insert (Dict.size answers) sampleAnswer answers }
            , Cmd.none
            )

        Delete index ->
            (
                { model
                    | answers = ProjectHelpers.deleteEntry index establishIndexesFnc answers
                    , optRadio = updateOptRadioDuringDelete slideIndex questionIndex index optRadio answers
                }
                , Cmd.none
            )

        Move index ProjectHelpers.Top ->
            let
                updatedAnswers = ProjectHelpers.moveEntry
                    index ProjectHelpers.Increment 0
                    establishIndexesFnc answers
                updatedOptRadio = updateOptRadioDuringMove
                    slideIndex questionIndex index ProjectHelpers.Increment 0
                    optRadio answers
            in
            (
                { model
                    | answers = updatedAnswers
                    , optRadio = updatedOptRadio
                }
                , Cmd.none
            )

        Move index ProjectHelpers.Up ->
            (
                { model
                    | answers = ProjectHelpers.flipAdjacentEntries
                        index ProjectHelpers.Decrement establishIndexesFnc answers
                    , optRadio = updateOptRadioDuringFlip slideIndex questionIndex index ProjectHelpers.Decrement optRadio answers
                }
                , Cmd.none
            )

        Move index ProjectHelpers.Down ->
            (
                { model
                    | answers = ProjectHelpers.flipAdjacentEntries
                        index ProjectHelpers.Increment establishIndexesFnc answers
                    , optRadio = updateOptRadioDuringFlip slideIndex questionIndex index ProjectHelpers.Increment optRadio answers
                }
                , Cmd.none
            )

        Move index ProjectHelpers.Bottom ->
            let
                finalIndex = (Dict.size answers) - 1
                updatedAnswers = ProjectHelpers.moveEntry
                    index ProjectHelpers.Decrement finalIndex
                    establishIndexesFnc answers
                updatedOptRadio = updateOptRadioDuringMove
                    slideIndex questionIndex index ProjectHelpers.Decrement finalIndex
                    optRadio answers
            in
            (
                { model
                    | answers = updatedAnswers
                    , optRadio = updatedOptRadio
                }
                , Cmd.none
            )

        Update index s ->
            ( { model | answers = Dict.insert index (Answer s) answers }
            , Cmd.none
            )

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

viewIsCorrectRadioButton : Int -> Int -> Int -> OptRadio -> Html Msg
viewIsCorrectRadioButton slideIndex questionIndex answerIndex correctAnswer =
    let
        nameValue = getCandorAnswerHeader slideIndex questionIndex
        idValue = getCandorAnswer slideIndex questionIndex answerIndex
    in
    label
        [ for idValue ]
        [
            input
                [ type_ "radio"
                , id idValue
                , name nameValue
                , value idValue
                , checked (correctAnswer == (OptRadio idValue))
                ]
                [ ]
            , text "is Correct?"
        ]

viewAnswerTableRowEntry : Int -> OptRadio -> Int -> Int -> Int -> Answer -> List (Html Msg) -> List (Html Msg)
viewAnswerTableRowEntry numberAnswers correctAnswer slideIndex questionIndex answerIndex answer l =
    let
        entry =
            tr
                [ ]
                [ viewMoveAnswerTopButton answerIndex
                , viewMoveAnswerUpButton answerIndex
                , viewMoveAnswerDownButton answerIndex numberAnswers
                , viewMoveAnswerBottomButton answerIndex numberAnswers
                , viewDeleteButton answerIndex numberAnswers
                , viewIsCorrectRadioButton slideIndex questionIndex answerIndex correctAnswer
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
viewAnswersTable { slideIndex, questionIndex, optRadio, answers } =
    Dict.foldl (viewAnswerTableRowEntry (Dict.size answers) optRadio slideIndex questionIndex) [ ] answers
        |> table [ class "edit-page-answers-table" ]

view : Model -> Html Msg
view model =
    div
        [ class "edit-page-answers" ]
        [ viewHeader
        , viewActionButtons
        , viewAnswersTable model
        ]
