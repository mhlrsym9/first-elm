module Data.Question exposing (encodeQuestion, establishIndexes, init, Model, Msg(..), questionDecoder, update, updateQuestionIndex, updateSlideIndex, view)

import Data.AnswersArea as AnswersArea
import Element exposing (centerX, column, Element, padding, spacing)
import Element.Font as Font
import Element.Input as Input
import Json.Decode exposing (Decoder, field, map, string, succeed)
import Json.Decode.Pipeline exposing (custom, hardcoded)
import Json.Encode as Encode
import Task exposing (Task)
import UIHelpers exposing (buttonAttributes)

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

viewQuestion : Model -> Element Msg
viewQuestion { questionText } =
    Input.text
        [ centerX ]
        { onChange = Update
        , text = (textToString questionText)
        , placeholder = Just (Input.placeholder [ ] (Element.text "Supply a question here."))
        , label = Input.labelHidden "Input this question here."
        }

viewAnswersButton : Model -> Element Msg
viewAnswersButton ( { answersArea } ) =
    case answersArea of
        Hidden m ->
            Input.button
                (centerX :: buttonAttributes)
                { onPress = Just ( UpdateVisibility (Visible m) )
                , label = Element.text "View Answers"
                }

        Visible m ->
            Input.button
                (centerX :: buttonAttributes)
                { onPress = Just ( UpdateVisibility (Hidden m) )
                , label = Element.text "Hide Answers"
                }

viewAnswers : Model -> Element Msg
viewAnswers ( { questionIndex, answersArea } ) =
    case answersArea of
        Hidden _ ->
            Element.none

        Visible m ->
            AnswersArea.view m
                |> Element.map AnswersAreaMsg

view : Model -> Element Msg
view model =
    column
        [ Font.size 14
        , padding 10
        , spacing 10
        , centerX
        ]
        [ viewQuestion model
        , viewAnswersButton model
        , viewAnswers model
        ]
