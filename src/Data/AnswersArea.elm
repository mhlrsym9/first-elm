module Data.AnswersArea exposing (answersAreaDecoder, encodeAnswersArea, establishIndexes, init, Model, Msg, update, view)

import Data.ProjectHelpers as ProjectHelpers
import Dict exposing (Dict)
import Html exposing (Html, button, div, h3, input, table, text, tr)
import Html.Attributes exposing (class, disabled, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, field, map, string, succeed)
import Json.Decode.Extra exposing (indexedList)
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

init : (Model, Cmd msg)
init =
    (
        { slideIndex = 0
        , questionIndex = 0
        , optRadio = OptRadio "0_0_0"
        , answers = Dict.singleton 0 (Answer "This is a sample answer")
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

type Direction =
    Up
    | Down
    | Top
    | Bottom

type Msg =
    Delete Int
    | Move Int Direction
    | Update Int String

update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { answers } as model ) =
    case msg of
        Delete _ ->
            ( model, Cmd.none )

        Move index Up ->
            let
                updatedAnswers = ProjectHelpers.shiftIndexes index (index - 1) answers
            in
            ( { model | answers = updatedAnswers }
            , Cmd.none
            )

        Move index Down ->
            let
                updatedAnswers = ProjectHelpers.shiftIndexes index (index + 1) answers
            in
            ( { model | answers = updatedAnswers }
            , Cmd.none
            )


        Move index Top ->
            let
                updatedAnswers = ProjectHelpers.updateIndexes index 1 0 answers
            in
            ( { model | answers = updatedAnswers }
            , Cmd.none
            )

        Move index Bottom ->
            let
                updatedAnswers = ProjectHelpers.updateIndexes index -1 ((Dict.size answers) - 1) answers
            in
            ( { model | answers = updatedAnswers }
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
    div
        [ ]
        [ ]

viewMoveAnswerTopButton : Int -> Html Msg
viewMoveAnswerTopButton index =
    button
        [ onClick (Move index Top)
        , disabled (0 == index)
        ]
        [ text "Move Answer to Top" ]

viewMoveAnswerUpButton : Int -> Html Msg
viewMoveAnswerUpButton index =
    button
        [ onClick (Move index Up)
        , disabled (0 == index)
        ]
        [ text "Move Answer Up" ]

viewMoveAnswerDownButton : Int -> Int -> Html Msg
viewMoveAnswerDownButton index numberAnswers =
    button
        [ onClick (Move index Down)
        , disabled ( index == (numberAnswers - 1) )
        ]
        [ text "Move Answer Down" ]

viewMoveAnswerBottomButton : Int -> Int -> Html Msg
viewMoveAnswerBottomButton index numberAnswers =
    button
        [ onClick (Move index Bottom)
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

viewAnswerTableRowEntry : Int -> Int -> Answer -> List (Html Msg) -> List (Html Msg)
viewAnswerTableRowEntry numberAnswers index answer l =
    let
        entry =
            tr
                [ ]
                [ viewMoveAnswerTopButton index
                , viewMoveAnswerUpButton index
                , viewMoveAnswerDownButton index numberAnswers
                , viewMoveAnswerBottomButton index numberAnswers
                , viewDeleteButton index numberAnswers
                , input
                    [ type_ "text"
                    , value (answerToString answer)
                    , onInput (Update index)
                    ]
                    [ ]
                ]
    in
    List.append l (List.singleton entry)

viewAnswersTable : Model -> Html Msg
viewAnswersTable { answers } =
    Dict.foldl (viewAnswerTableRowEntry (Dict.size answers)) [ ] answers
        |> table [ class "edit-page-answers-table" ]

view : Model -> Html Msg
view model =
    div
        [ class "edit-page-answers" ]
        [ viewHeader
        , viewActionButtons
        , viewAnswersTable model
        ]
