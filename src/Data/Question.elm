module Data.Question exposing (encodeQuestion, establishIndexes, init, Model, Msg(..), questionDecoder, update, view)

import Data.AnswersArea as AnswersArea
import Html exposing (Html, button, div, input, text, textarea, tr)
import Html.Attributes exposing (class, disabled, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, field, map, string, succeed)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import Json.Encode as Encode

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

init : { questionIndex : Int, slideIndex : Int } -> (Model, Cmd Msg)
init { questionIndex, slideIndex } =
    let
        (answersAreaModel, answersAreaCommands) =
            AnswersArea.init
    in
    (
        { questionIndex = questionIndex
        , slideIndex = slideIndex
        , questionText = Text "This is a sample question"
        , answersArea = Hidden answersAreaModel
        }
        , answersAreaCommands
    )

establishAnswersAreaIndexes : Int -> Int -> Model -> Visibility
establishAnswersAreaIndexes slideIndex questionIndex ( { answersArea } as model ) =
    case answersArea of
        Hidden m ->
            Hidden (AnswersArea.establishIndexes slideIndex questionIndex m)

        Visible m ->
            Visible (AnswersArea.establishIndexes slideIndex questionIndex m)

establishIndexes : Int -> Int -> Model -> Model
establishIndexes slideIndex questionIndex model =
    {
        model
            | slideIndex = slideIndex
            , questionIndex = questionIndex
            , answersArea = establishAnswersAreaIndexes slideIndex questionIndex model
    }

-- UPDATE

type Msg =
    Update String
    | UpdateVisibility Visibility
    | AnswersAreaMsg AnswersArea.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { questionIndex, answersArea } as model ) =
    case msg of
        Update s ->
            ( { model | questionText = Text s }
            , Cmd.none
            )

        UpdateVisibility updatedAnswersArea ->
            ( { model | answersArea = updatedAnswersArea }
            , Cmd.none
            )

        AnswersAreaMsg answersAreaMsg ->
            case answersArea of
                Hidden _ ->
                    ( model, Cmd.none )

                Visible m ->
                    let
                        (updatedAnswersAreaModel, answersAreaCommands) =
                            AnswersArea.update answersAreaMsg m
                    in
                    ( { model | answersArea = Visible updatedAnswersAreaModel }
                    , Cmd.map AnswersAreaMsg answersAreaCommands
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
