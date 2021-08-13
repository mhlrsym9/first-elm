module Data.Slide exposing (encodeSlide, init, Model, Msg, slideDecoder, textToString, update, view)

import Data.QuestionsArea as QuestionsArea
import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, field, map, string, succeed)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode

-- MODEL

type Text =
    Text String

type alias Model =
    { slideText : Text
    , questionsArea : QuestionsArea.Model
    }

slideDecoder : Decoder Model
slideDecoder =
    succeed Model
        |> custom slideTextDecoder
        |> required "questionsarea" QuestionsArea.questionsAreaDecoder

encodeSlide : Model -> Encode.Value
encodeSlide { slideText, questionsArea } =
    Encode.object
        [ ( "slide", Encode.string (textToString slideText) )
        , ( "questionsArea", QuestionsArea.encodeQuestionsArea questionsArea )
        ]

slideTextDecoder : Decoder Text
slideTextDecoder =
    map Text (field "slide" string)

textToString : Text -> String
textToString (Text val) =
    val

init : Model
init =
    { slideText = Text "This is a test"
    , questionsArea = QuestionsArea.init
    }

-- UPDATE

type Msg =
    Update String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Update s ->
            ( { model | slideText = Text s }, Cmd.none )

-- VIEW

view : Model -> Html Msg
view { slideText } =
    textarea
        [ class "edit-page-slide"
        , onInput Update
        ]
        [ text (textToString slideText) ]
