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

type alias Model =
    { questionIndex : Int
    , slideIndex : Int
    , questionText : Text
    , answersArea : AnswersArea.Model
    }

questionDecoder : Decoder Model
questionDecoder =
    succeed Model
        |> hardcoded 0
        |> hardcoded 0
        |> custom questionTextDecoder
        |> required "answersarea" AnswersArea.answersAreaDecoder

questionTextDecoder : Decoder Text
questionTextDecoder =
    map Text (field "question" string)

encodeQuestion : Model -> Encode.Value
encodeQuestion { questionText, answersArea } =
    Encode.object
        [ ( "display", Encode.string "none" )
        , ( "question", Encode.string (textToString questionText) )
        , ( "answersarea", AnswersArea.encodeAnswersArea answersArea )
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
        , answersArea = answersAreaModel
        }
        , answersAreaCommands
    )

establishIndexes : Int -> Int -> Model -> Model
establishIndexes slideIndex questionIndex ( { answersArea } as model ) =
    {
        model
            | slideIndex = slideIndex
            , questionIndex = questionIndex
            , answersArea = AnswersArea.establishIndexes slideIndex questionIndex answersArea
    }

-- UPDATE

type Msg =
    Update Int String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Update _ s ->
            ( { model | questionText = Text s }
            , Cmd.none
            )

-- VIEW

view : Model -> Html Msg
view { questionIndex, questionText } =
    input
        [ onInput (Update questionIndex)
        , type_ "text"
        , value (textToString questionText)
        ]
        [ ]
