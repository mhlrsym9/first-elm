module Data.Slide exposing (encodeSlide, establishIndexes, init, Model, Msg, slideDecoder, textToString, update, updateSlideIndex, view)

import Data.QuestionsArea as QuestionsArea
import Html exposing (Html, text)
import Html.Attributes exposing (attribute)
import Html.Keyed as Keyed
import Json.Decode exposing (Decoder, field, string, succeed)
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
    Json.Decode.map Text (field "slide" string)

textToString : Text -> String
textToString (Text val) =
    val

init : { slideIndex : Int } -> (Model, Cmd Msg)
init ( { slideIndex } as flags ) =
    let
        (questionsArea, commands) =
            QuestionsArea.init flags
    in
    (
        { slideText = Text "This is a test"
        , slideIndex = slideIndex
        , questionsArea = questionsArea
        }
        , Cmd.map QuestionsAreaMsg commands
    )

updateSlideIndex : Int -> Model -> Model
updateSlideIndex slideIndex ( { questionsArea } as model ) =
    {
        model
            | questionsArea = QuestionsArea.updateSlideIndexes slideIndex questionsArea
            , slideIndex = slideIndex
    }

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

update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { questionsArea } as model ) =
    case Debug.log "updateSlide" msg of
        QuestionsAreaMsg questionsAreaMsg ->
            let
                (updatedQuestionsAreaModel, questionsAreaCmds) =
                    QuestionsArea.update questionsAreaMsg questionsArea
            in
            ( { model | questionsArea = updatedQuestionsAreaModel }
            , Cmd.map QuestionsAreaMsg questionsAreaCmds )

-- VIEW

viewTinyMCEEditor : Model -> (String, Html Msg)
viewTinyMCEEditor { slideText, slideIndex } =
    ( "tinymce-editor-" ++ (String.fromInt slideIndex)
    , Html.node "tinymce-editor"
        [ attribute "api-key" "no-api-key"
        , attribute "height" "500"
        , attribute "plugins" "link image code"
        , attribute "toolbar" "undo redo | bold italic | alignleft aligncenter alignright | code | help"
        , attribute "setup" "setupEditor"
        ]
        [ text (textToString slideText) ]
    )

viewQuestionsArea : Model -> (String, Html Msg)
viewQuestionsArea { slideIndex, questionsArea } =
    ( "candor-question-area-" ++ (String.fromInt slideIndex)
    , QuestionsArea.view questionsArea
        |> Html.map QuestionsAreaMsg
    )

view : Model -> Html Msg
view model =
    Keyed.node "div"
        [ ]
        [ viewTinyMCEEditor model
        , viewQuestionsArea model
        ]
