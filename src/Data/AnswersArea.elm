module Data.AnswersArea exposing (answersAreaDecoder, encodeAnswersArea, establishIndexes, init, Model, Msg(..), parseOptRadio, update, updateQuestionIndexes, updateSlideIndexes, view)

import Data.ProjectHelpers as ProjectHelpers
import Dict exposing (Dict)
import Element exposing (Attribute, centerX, centerY, column, Element, el, fill, IndexedColumn, padding, paragraph, rgb255, shrink, spacing)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Decode exposing (Decoder, field, map, string, succeed)
import Json.Decode.Extra exposing (indexedList)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import Json.Encode as Encode
import Parser exposing ((|.), (|=), end, int, Parser, run, symbol, token)
import Task exposing (Task)

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
    | IsCorrectAnswer OptRadio Bool
    | MakeDirty
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

makeProjectDirty : Cmd Msg
makeProjectDirty =
    Task.perform ( always MakeDirty ) ( Task.succeed () )

update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { slideIndex, questionIndex, answers } as model ) =
    case msg of
        Add ->
            ( { model | answers = Dict.insert (Dict.size answers) sampleAnswer answers }
            , makeProjectDirty
            )

        Delete answerIndex ->
            (
                { model
                    | answers = ProjectHelpers.deleteEntry answerIndex identity answers
                    , optRadio = updateOptRadioDuringDelete model answerIndex
                }
                , makeProjectDirty
            )

        IsCorrectAnswer optRadio _ ->
            ( { model | optRadio = optRadio }
            , makeProjectDirty
            )

-- Handled by Question module
        MakeDirty ->
            ( model, Cmd.none )

        Move index ProjectHelpers.Top ->
            let
                updatedAnswers = ProjectHelpers.moveEntry
                    index ProjectHelpers.Increment 0
                    identity answers
                updatedOptRadio = updateOptRadioDuringMove
                    model index ProjectHelpers.Increment 0
            in
            (
                { model
                    | answers = updatedAnswers
                    , optRadio = updatedOptRadio
                }
                , makeProjectDirty
            )

        Move answerIndex ProjectHelpers.Up ->
            (
                { model
                    | answers = ProjectHelpers.flipAdjacentEntries
                        answerIndex ProjectHelpers.Decrement identity answers
                    , optRadio = updateOptRadioDuringFlip model answerIndex ProjectHelpers.Decrement
                }
                , makeProjectDirty
            )

        Move answerIndex ProjectHelpers.Down ->
            (
                { model
                    | answers = ProjectHelpers.flipAdjacentEntries
                        answerIndex ProjectHelpers.Increment identity answers
                    , optRadio = updateOptRadioDuringFlip model answerIndex ProjectHelpers.Increment
                }
                , makeProjectDirty
            )

        Move index ProjectHelpers.Bottom ->
            let
                finalIndex = (Dict.size answers) - 1
                updatedAnswers = ProjectHelpers.moveEntry
                    index ProjectHelpers.Decrement finalIndex
                    identity answers
                updatedOptRadio = updateOptRadioDuringMove
                    model index ProjectHelpers.Decrement finalIndex
            in
            (
                { model
                    | answers = updatedAnswers
                    , optRadio = updatedOptRadio
                }
                , makeProjectDirty
            )

        Update index s ->
            ( { model | answers = Dict.insert index (Answer s) answers }
            , makeProjectDirty
            )

-- VIEW

viewHeader : Element Msg
viewHeader =
    el
        [ Font.size 24
        , centerX
        ]
        (Element.text "Answers")

viewActionButtons : Element Msg
viewActionButtons =
    Input.button
        [ centerX ]
        { onPress = Just Add
        , label = Element.text "Add Another Answer"
        }

lightGrey : Element.Color
lightGrey =
    rgb255 211 211 211

black : Element.Color
black =
    rgb255 0 0 0

green : Element.Color
green =
    rgb255 0 128 0

buttonAttributes : List (Attribute Msg)
buttonAttributes =
    [ centerY
    , padding 5
    , Background.color lightGrey
    , Border.rounded 3
    , Border.color black
    , Border.width 1
    ]

viewMoveAnswerTopButton : Int -> Element Msg
viewMoveAnswerTopButton index =
    if (0 == index) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = Just (Move index ProjectHelpers.Top)
            , label = Element.text "Move Answer to Top"
            }

viewMoveAnswerUpButton : Int -> Element Msg
viewMoveAnswerUpButton index =
    if (0 == index) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = Just (Move index ProjectHelpers.Up)
            , label = Element.text "Move Answer Up"
            }

viewMoveAnswerDownButton : Int -> Int -> Element Msg
viewMoveAnswerDownButton numberAnswers index =
    if ((numberAnswers - 1) == index) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = Just (Move index ProjectHelpers.Down)
            , label = Element.text "Move Answer Down"
            }

viewMoveAnswerBottomButton : Int -> Int -> Element Msg
viewMoveAnswerBottomButton numberAnswers index =
    if ((numberAnswers - 1) == index) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = Just (Move index ProjectHelpers.Bottom)
            , label = Element.text "Move Answer to Bottom"
            }

viewDeleteButton : Int -> Int -> Element Msg
viewDeleteButton numberAnswers index =
    if (1 == numberAnswers) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = Just (Delete index)
            , label = Element.text "Delete This Answer"
            }

viewIsCorrectCheckbox : Model -> Int -> Element Msg
viewIsCorrectCheckbox ( { optRadio } as model ) answerIndex =
    let
        idValue = OptRadio (getCandorAnswer model answerIndex)
    in
    if (optRadio == idValue) then
        paragraph
            [ centerX
            , centerY
            , Font.color green
            ]
            [ Element.text "Correct Answer" ]
    else
    Input.checkbox
        [ centerY ]
        { onChange = IsCorrectAnswer idValue
        , icon = Input.defaultCheckbox
        , checked = False
        , label =
            Input.labelRight
                [ ]
                (Element.text "Is Correct?")
        }

prepareButton : (Int -> Element Msg) -> IndexedColumn Answer Msg
prepareButton fnc =
    { header = Element.text ""
    , width = shrink
    , view =
        \index _ ->
            fnc index
    }

prepareIsCorrect : (Int -> Element Msg) -> IndexedColumn Answer Msg
prepareIsCorrect fnc =
    { header = Element.text "Is Correct Answer?"
    , width = shrink
    , view =
        \index _ ->
            fnc index
    }

prepareAnswerInput : IndexedColumn Answer Msg
prepareAnswerInput =
    { header = Element.text "Answer"
    , width = fill
    , view =
        \index answer ->
            case answer of
                Answer s ->
                    Input.text
                        [ centerY ]
                        { onChange = Update index
                        , text = s
                        , placeholder = Just (Input.placeholder [ ] (Element.text "Supply an answer here."))
                        , label = Input.labelHidden "Input this answer here."
                        }
    }

viewAnswersTable : Model -> Element Msg
viewAnswersTable ( { answers } as model ) =
    let
        numberAnswers = Dict.size answers
    in
    Element.indexedTable
        [ spacing 10 ]
        { data = Dict.values answers
        , columns =
            [ prepareButton viewMoveAnswerTopButton
            , prepareButton viewMoveAnswerUpButton
            , prepareButton (viewMoveAnswerDownButton numberAnswers)
            , prepareButton (viewMoveAnswerBottomButton numberAnswers)
            , prepareButton (viewDeleteButton numberAnswers)
            , prepareIsCorrect (viewIsCorrectCheckbox model)
            , prepareAnswerInput
            ]
        }

view : Model -> Element Msg
view model =
    column
        [ Font.size 14
        , padding 10
        ]
        [ viewHeader
        , viewActionButtons
        , viewAnswersTable model]
