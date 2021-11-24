module Data.Question exposing (encodeQuestion, establishIndexes, init, Model, Msg(..), questionDecoder, update, updateQuestionIndex, updateSlideIndex, view)

import Data.AnswersArea as AnswersArea
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, field, map, string, succeed)
import Json.Decode.Pipeline exposing (custom, hardcoded)
import Json.Encode as Encode
import Task exposing (Task)

-- MODEL

type Text =
    Text String

type Visibility =
    Hidden AnswersArea.Model
    | Visible AnswersArea.Model

type alias Model =
    { questionIndex : Int
    , slideIndex : Int
    , questionText : Text
    , answersArea : Visibility
    }

questionTextDecoder : Decoder Text
questionTextDecoder =
    map Text (field "question" string)

answersAreaDecoder : Decoder Visibility
answersAreaDecoder =
    map Hidden (field "answersarea" AnswersArea.answersAreaDecoder)

questionDecoder : Decoder Model
questionDecoder =
    succeed Model
        |> hardcoded 0
        |> hardcoded 0
        |> custom questionTextDecoder
        |> custom answersAreaDecoder

extractAnswersAreaModel : Model -> AnswersArea.Model
extractAnswersAreaModel { answersArea } =
    case answersArea of
        Hidden m ->
            m

        Visible m ->
            m

encodeAnswersArea : Model -> Encode.Value
encodeAnswersArea model =
    AnswersArea.encodeAnswersArea (extractAnswersAreaModel model)

encodeQuestion : Model -> Encode.Value
encodeQuestion ( { questionText } as model ) =
    Encode.object
        [ ( "display", Encode.string "none" )
        , ( "question", Encode.string (textToString questionText) )
        , ( "answersarea", encodeAnswersArea model)
        ]

textToString: Text -> String
textToString (Text val) =
    val

init : { questionIndex : Int, slideIndex : Int } -> Model
init ( { questionIndex, slideIndex } as flags ) =
    let
        answersAreaModel = AnswersArea.init flags
    in
    { questionIndex = questionIndex
    , slideIndex = slideIndex
    , questionText = Text "This is a sample question"
    , answersArea = Hidden answersAreaModel
    }

establishAnswersAreaIndexes : Int -> Int -> Model -> Visibility
establishAnswersAreaIndexes slideIndex questionIndex { answersArea } =
    case answersArea of
        Hidden m ->
            Hidden (AnswersArea.establishIndexes slideIndex questionIndex m)

        Visible m ->
            Visible (AnswersArea.establishIndexes slideIndex questionIndex m)

updateQuestionIndex : Int -> Model -> Model
updateQuestionIndex questionIndex ( { answersArea } as model )  =
    let
        updatedAnswersArea =
            case answersArea of
                Hidden m ->
                    Hidden (AnswersArea.updateQuestionIndexes questionIndex m)

                Visible m ->
                    Visible (AnswersArea.updateQuestionIndexes questionIndex m)
    in
    {
        model
            | questionIndex = questionIndex
            , answersArea = updatedAnswersArea
    }

updateSlideIndex : Int -> Int -> Model -> Model
updateSlideIndex slideIndex _ ( { answersArea } as model ) =
    let
        updatedAnswersArea =
            case answersArea of
                Hidden m ->
                    Hidden (AnswersArea.updateSlideIndexes slideIndex m)

                Visible m ->
                    Visible (AnswersArea.updateSlideIndexes slideIndex m)
    in
    {
        model
            | slideIndex = slideIndex
            , answersArea = updatedAnswersArea
    }

establishIndexes : Int -> Int -> Model -> Model
establishIndexes slideIndex questionIndex model =
    {
        model
            | slideIndex = slideIndex
            , questionIndex = questionIndex
            , answersArea = establishAnswersAreaIndexes slideIndex questionIndex model
    }

-- UPDATE

type Msg
    = AnswersAreaMsg AnswersArea.Msg
    | MakeDirty
    | Update String
    | UpdateVisibility Visibility

makeProjectDirty : Cmd Msg
makeProjectDirty =
    Task.perform ( always MakeDirty ) ( Task.succeed () )

update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { questionIndex, answersArea } as model ) =
    case msg of
        AnswersAreaMsg answersAreaMsg ->
            case answersAreaMsg of
                AnswersArea.MakeDirty ->
                    ( model, makeProjectDirty )

                _ ->
                    case answersArea of
                        Hidden _ ->
                            ( model, Cmd.none )

                        Visible m ->
                            let
                                (updatedAnswersAreaModel, commands) =
                                    AnswersArea.update answersAreaMsg m
                            in
                            ( { model | answersArea = Visible updatedAnswersAreaModel }
                            , Cmd.map AnswersAreaMsg commands
                            )

-- Handled in QuestionsArea module
        MakeDirty ->
            ( model, Cmd.none )

        Update s ->
            ( { model | questionText = Text s }
            , makeProjectDirty
            )

        UpdateVisibility updatedAnswersArea ->
            ( { model | answersArea = updatedAnswersArea }
            , Cmd.none
            )

-- VIEW

viewQuestion : Model -> Html Msg
viewQuestion { questionText } =
    input
        [ onInput Update
        , type_ "text"
        , value (textToString questionText)
        ]
        [ ]

viewAnswersButton : Model -> Html Msg
viewAnswersButton ( { answersArea } ) =
    case answersArea of
        Hidden m ->
            button
                [ onClick ( UpdateVisibility (Visible m) ) ]
                [ text "View Answers" ]

        Visible m ->
            button
                [ onClick ( UpdateVisibility (Hidden m) ) ]
                [ text "Hide Answers" ]

viewAnswers : Model -> Html Msg
viewAnswers ( { questionIndex, answersArea } ) =
    case answersArea of
        Hidden _ ->
            div [ ] [ ]

        Visible m ->
            AnswersArea.view m
                |> Html.map AnswersAreaMsg

view : Model -> Html Msg
view model =
    div
        [ class "edit-page-question" ]
        [ viewQuestion model
        , viewAnswersButton model
        , viewAnswers model
        ]
