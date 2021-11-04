module Data.Slide exposing (encodeSlide, establishIndexes, init, Model, Msg, slideDecoder, storeSlideContents, textToString, update, updateSlideIndex, view)

import Data.QuestionsArea as QuestionsArea
import Html exposing (Html, text)
import Html.Attributes exposing (attribute)
import Html.Keyed as Keyed
import Json.Decode exposing (Decoder, field, string, succeed)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import Json.Encode as Encode
import UUID

-- MODEL

type Text =
    Text String

type alias Model =
    { questionsArea : QuestionsArea.Model
    , setupEditorName : String
    , slideId : String
    , slideIndex : Int
    , slideText : Text
    }

slideDecoder : String -> Decoder Model
slideDecoder sen =
    succeed Model
        |> required "questionsarea" QuestionsArea.questionsAreaDecoder
        |> hardcoded sen
        |> hardcoded UUID.nilString
        |> hardcoded 0
        |> custom slideTextDecoder

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

init : { slideIndex : Int, sen : String, slideId : String } -> Model
init { slideIndex, sen, slideId } =
    let
        questionsArea = QuestionsArea.init { slideIndex = slideIndex }
    in
    { slideText = Text "This is a test"
    , slideIndex = slideIndex
    , setupEditorName = sen
    , questionsArea = questionsArea
    , slideId = slideId
    }

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

storeSlideContents : String -> Model -> Model
storeSlideContents slideContents model =
    { model | slideText = Text slideContents }

update : Msg -> Model -> Model
update msg ( { questionsArea } as model ) =
    case msg of
        QuestionsAreaMsg questionsAreaMsg ->
            let
                updatedQuestionsAreaModel =
                    QuestionsArea.update questionsAreaMsg questionsArea
            in
            { model | questionsArea = updatedQuestionsAreaModel }

-- VIEW

viewTinyMCEEditor : Model -> (String, Html Msg)
viewTinyMCEEditor { slideText, slideIndex, setupEditorName, slideId } =
    ( "tinymce-editor-" ++ slideId
    , Html.node "tinymce-editor"
        [ attribute "api-key" "no-api-key"
        , attribute "height" "500"
        , attribute "plugins" "link image code"
        , attribute "toolbar" "undo redo | bold italic | alignleft aligncenter alignright | code | help"
        , attribute "setup" setupEditorName
        ]
        [ text (textToString slideText) ]
    )

viewQuestionsArea : Model -> (String, Html Msg)
viewQuestionsArea { slideIndex, questionsArea, slideId } =
    ( "candor-question-area-" ++ slideId
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
