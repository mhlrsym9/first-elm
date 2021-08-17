module Data.Slide exposing (encodeSlide, establishIndexes, init, Model, Msg, slideDecoder, textToString, update, view)

import Data.QuestionsArea as QuestionsArea
import Html exposing (Html, div, text, textarea)
import Html.Attributes exposing (class)
import Html.Events exposing (onInput)
import Json.Decode exposing (Decoder, field, map, string, succeed)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import Json.Encode as Encode

-- MODEL

type Text =
    Text String

type alias Model =
    { slideText : Text
    , slideIndex : Int
    , questionsArea : QuestionsArea.Model
    }

slideDecoder : Decoder Model
slideDecoder =
    succeed Model
        |> custom slideTextDecoder
        |> hardcoded 0
        |> required "questionsarea" QuestionsArea.questionsAreaDecoder

encodeSlide : Model -> Encode.Value
encodeSlide { slideText, questionsArea } =
    Encode.object
        [ ( "slide", Encode.string (textToString slideText) )
        , ( "display", Encode.string "initial")
        , ( "questionsarea", QuestionsArea.encodeQuestionsArea questionsArea )
        ]

slideTextDecoder : Decoder Text
slideTextDecoder =
    map Text (field "slide" string)

textToString : Text -> String
textToString (Text val) =
    val

init : (Model, Cmd Msg)
init =
    let
        (questionsArea, commands) =
            QuestionsArea.init
    in
    (
        { slideText = Text "This is a test"
        , slideIndex = 0
        , questionsArea = questionsArea
        }
        , Cmd.map QuestionsAreaMsg commands
    )

establishIndexes : Int -> Model -> Model
establishIndexes slideIndex ( { questionsArea } as model ) =
    {
        model
            | questionsArea = QuestionsArea.establishIndexes slideIndex questionsArea
            , slideIndex = slideIndex
    }

-- UPDATE

type Msg =
    QuestionsAreaMsg QuestionsArea.Msg
    | Update String

update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { questionsArea } as model ) =
    case msg of
        QuestionsAreaMsg questionsAreaMsg ->
            let
                (updatedQuestionsAreaModel, questionsAreaCmds) =
                    QuestionsArea.update questionsAreaMsg questionsArea
            in
            ( { model | questionsArea = updatedQuestionsAreaModel }
            , Cmd.map QuestionsAreaMsg questionsAreaCmds )

        Update s ->
            ( { model | slideText = Text s }, Cmd.none )

-- VIEW

view : Model -> Html Msg
view { slideText, questionsArea } =
    div
        [ ]
        [ textarea
            [ class "edit-page-slide"
            , onInput Update
            ]
            [ text (textToString slideText) ]
        , QuestionsArea.view questionsArea
            |> Html.map QuestionsAreaMsg
        ]
