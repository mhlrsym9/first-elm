module Data.Slide exposing (encodeSlide, establishIndexes, init, InitParams, Model, Msg, slideDecoder, storeSlideContents, textToString, update, updateSlideIndex, view)

import Api
import Bytes exposing (Bytes)
import Data.QuestionsArea as QuestionsArea
import File exposing (File)
import File.Select as Select
import Flags exposing (Flags)
import Http exposing (bytesBody, stringResolver)
import Html exposing (button, div, Html, table, text, tr)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Json.Decode exposing (Decoder, field, list, map, string, succeed)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import Json.Encode as Encode
import LanguageHelpers
import Procedure exposing (Procedure)
import Procedure.Program
import Task exposing (Task)
import UUID
import Url.Builder as Builder

-- MODEL

type Text =
    Text String

type alias InitParams =
    { flags : Flags.Model
    , knownLanguage : LanguageHelpers.Language
    , learningLanguage : LanguageHelpers.Language
    , projectName : String
    }

type alias SlideComponent =
    { name : String
    , id : String
    }

type Images
    = HiddenImages (List SlideComponent)
    | VisibleImages (List SlideComponent)

type Sounds
    = HiddenSounds (List SlideComponent)
    | VisibleSounds (List SlideComponent)

type Videos
    = HiddenVideos (List SlideComponent)
    | VisibleVideos (List SlideComponent)

type alias Model =
    { images : Images
    , initParams : InitParams
    , procModel : Procedure.Program.Model Msg
    , questionsArea : QuestionsArea.Model
    , slideId : String
    , slideIndex : Int
    , slideText : Text
    , sounds : Sounds
    , videos : Videos
    }

slideComponentDecoder : Decoder SlideComponent
slideComponentDecoder =
    succeed SlideComponent
        |> required "name" string
        |> optional "id" string ""

slideComponentsDecoder : Decoder (List SlideComponent)
slideComponentsDecoder =
    list slideComponentDecoder

imagesAreaDecoder : Decoder Images
imagesAreaDecoder =
    map HiddenImages slideComponentsDecoder

soundsAreaDecoder : Decoder Sounds
soundsAreaDecoder =
    map HiddenSounds slideComponentsDecoder

videosAreaDecoder : Decoder Videos
videosAreaDecoder =
    map HiddenVideos slideComponentsDecoder

slideDecoder : InitParams -> Decoder Model
slideDecoder initParams =
    succeed Model
        |> optional "images" imagesAreaDecoder (HiddenImages [])
        |> hardcoded initParams
        |> hardcoded Procedure.Program.init
        |> required "questionsarea" QuestionsArea.questionsAreaDecoder
        |> hardcoded UUID.nilString
        |> hardcoded 0
        |> custom slideTextDecoder
        |> optional "sounds" soundsAreaDecoder (HiddenSounds [])
        |> optional "videos" videosAreaDecoder (HiddenVideos [])

encodeSlideComponent : SlideComponent -> Encode.Value
encodeSlideComponent { name, id } =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "id", Encode.string id )
        ]

encodeSlideComponents : List SlideComponent -> Encode.Value
encodeSlideComponents l =
    Encode.list encodeSlideComponent l

extractImageComponents : Images -> List SlideComponent
extractImageComponents images =
    case images of
        HiddenImages l ->
            l

        VisibleImages l ->
            l

encodeImages : Images -> Encode.Value
encodeImages images =
    encodeSlideComponents (extractImageComponents images)

extractSoundComponents : Sounds -> List SlideComponent
extractSoundComponents sounds =
    case sounds of
        HiddenSounds l ->
            l

        VisibleSounds l ->
            l

encodeSounds : Sounds -> Encode.Value
encodeSounds sounds =
    encodeSlideComponents (extractSoundComponents sounds)

extractVideoComponents : Videos -> List SlideComponent
extractVideoComponents videos =
    case videos of
        HiddenVideos l ->
            l

        VisibleVideos l ->
            l

encodeVideos : Videos -> Encode.Value
encodeVideos videos =
    encodeSlideComponents (extractVideoComponents videos)

encodeSlide : Model -> Encode.Value
encodeSlide { slideText, questionsArea, images, sounds, videos } =
    Encode.object
        [ ( "slide", Encode.string (textToString slideText) )
        , ( "display", Encode.string "initial")
        , ( "questionsarea", QuestionsArea.encodeQuestionsArea questionsArea )
        , ( "images", encodeImages images )
        , ( "sounds", encodeSounds sounds )
        , ( "videos", encodeVideos videos )
        ]

slideTextDecoder : Decoder Text
slideTextDecoder =
    Json.Decode.map Text (field "slide" string)

textToString : Text -> String
textToString (Text val) =
    val

init : InitParams -> String -> Int -> Model
init initParams slideId slideIndex =
    let
        questionsArea = QuestionsArea.init { slideIndex = slideIndex }
    in
    { images = HiddenImages [ ]
    , initParams = initParams
    , procModel = Procedure.Program.init
    , questionsArea = questionsArea
    , slideId = slideId
    , slideIndex = slideIndex
    , slideText = Text "This is a test"
    , sounds = HiddenSounds [ ]
    , videos = HiddenVideos [ ]
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

type Msg
    = CopyUrl String
    | ImageRequested
    | ImageTransferred (Result Http.Error SlideComponent)
    | ProcedureMsg (Procedure.Program.Msg Msg)
    | QuestionsAreaMsg QuestionsArea.Msg
    | UpdateImagesVisibility Images Sounds Videos
    | UpdateSoundsVisibility Images Sounds Videos
    | UpdateVideosVisibility Images Sounds Videos

storeSlideContents : String -> Model -> Model
storeSlideContents slideContents model =
    { model | slideText = Text slideContents }

transferToServer : Model -> File -> Bytes -> Task Http.Error SlideComponent
transferToServer { initParams } f componentBytes =
    let
        url = Builder.relative
            [ initParams.flags.candorUrl
            , "image"
            , LanguageHelpers.contentCodeStringFromLanguage initParams.knownLanguage
            , LanguageHelpers.contentCodeStringFromLanguage initParams.learningLanguage
            , initParams.projectName
            , File.name f
            ] []
    in
    Http.task
        { method = "POST"
        , headers = []
        , url = url
        , body = bytesBody (File.mime f) componentBytes
        , resolver = stringResolver (Api.handleJsonResponse slideComponentDecoder)
        , timeout = Nothing
        }

addImageToProject : Model -> Cmd Msg
addImageToProject model =
    Procedure.fetch (Select.file ["image/png", "image/jpg"])
        |> Procedure.andThen
            (\f ->
                File.toBytes f
                    |> Procedure.fromTask
                    |> Procedure.map
                        (\bytes -> (f, bytes) )
            )
        |> Procedure.andThen
            (\(f, bytes) ->
                (transferToServer model f bytes)
                    |> Procedure.fromTask
            )
        |> Procedure.try ProcedureMsg ImageTransferred

update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { images, procModel, questionsArea } as model ) =
    case msg of
        CopyUrl _ ->
            ( model, Cmd.none )

        ImageRequested ->
            ( model, addImageToProject model)

        ImageTransferred result ->
            case result of
                Ok image ->
                    case images of
                        VisibleImages l ->
                            ( { model | images = VisibleImages ( image :: l ) }, Cmd.none )
                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ProcedureMsg procMsg ->
            Procedure.Program.update procMsg procModel
                |> Tuple.mapFirst (\updated -> { model | procModel = updated })

        QuestionsAreaMsg questionsAreaMsg ->
            let
                updatedQuestionsAreaModel =
                    QuestionsArea.update questionsAreaMsg questionsArea
            in
            ( { model | questionsArea = updatedQuestionsAreaModel }, Cmd.none )

        UpdateImagesVisibility updatedImages updatedSounds updatedVideos ->
            (
                { model
                | images = updatedImages
                , sounds = updatedSounds
                , videos = updatedVideos
                }
                , Cmd.none
            )

        UpdateSoundsVisibility updatedImages updatedSounds updatedVideos ->
            (
                { model
                | images = updatedImages
                , sounds = updatedSounds
                , videos = updatedVideos
                }
                , Cmd.none
            )

        UpdateVideosVisibility updatedImages updatedSounds updatedVideos ->
            (
                { model
                | images = updatedImages
                , sounds = updatedSounds
                , videos = updatedVideos
                }
                , Cmd.none
            )

-- VIEW

viewTinyMCEEditor : Model -> (String, Html Msg)
viewTinyMCEEditor { initParams, slideId, slideText } =
    ( "tinymce-editor-" ++ slideId
    , Html.node "tinymce-editor"
        [ attribute "api-key" "no-api-key"
        , attribute "height" "500"
        , attribute "plugins" "link image code"
        , attribute "toolbar" "undo redo | bold italic | alignleft aligncenter alignright | code | help"
        , attribute "setup" initParams.flags.setupEditorName
        ]
        [ text (textToString slideText) ]
    )

viewSlideImages : Model -> Html Msg
viewSlideImages { images, sounds, videos } =
    let
        updatedImages =
            case images of
                VisibleImages is ->
                    HiddenImages is

                HiddenImages is ->
                    VisibleImages is

        updatedSounds =
            case sounds of
                VisibleSounds ss ->
                    HiddenSounds ss

                HiddenSounds _ ->
                    sounds

        updatedVideos =
            case videos of
                VisibleVideos ss ->
                    HiddenVideos ss

                HiddenVideos _ ->
                    videos

    in
    button
        [ onClick ( UpdateImagesVisibility updatedImages updatedSounds updatedVideos ) ]
        [ text "Images" ]

viewSlideSounds : Model -> Html Msg
viewSlideSounds { images, sounds, videos } =
    let
        updatedImages =
            case images of
                VisibleImages is ->
                    HiddenImages is

                HiddenImages _ ->
                    images

        updatedSounds =
            case sounds of
                VisibleSounds is ->
                    HiddenSounds is

                HiddenSounds is ->
                    VisibleSounds is

        updatedVideos =
            case videos of
                VisibleVideos ss ->
                    HiddenVideos ss

                HiddenVideos _ ->
                    videos

    in
    button
        [ onClick ( UpdateSoundsVisibility updatedImages updatedSounds updatedVideos ) ]
        [ text "Sounds" ]

viewSlideVideos : Model -> Html Msg
viewSlideVideos { images, sounds, videos } =
    let
        updatedImages =
            case images of
                VisibleImages is ->
                    HiddenImages is

                HiddenImages _ ->
                    images

        updatedSounds =
            case sounds of
                VisibleSounds ss ->
                    HiddenSounds ss

                HiddenSounds _ ->
                    sounds

        updatedVideos =
            case videos of
                VisibleVideos is ->
                    HiddenVideos is

                HiddenVideos is ->
                    VisibleVideos is

    in
    button
        [ onClick ( UpdateVideosVisibility updatedImages updatedSounds updatedVideos ) ]
        [ text "Videos" ]

viewLoadImageFromFile : Html Msg
viewLoadImageFromFile =
    button
        [ onClick ImageRequested ]
        [ text "Select file" ]

viewComponent : InitParams -> SlideComponent -> List (Html Msg) -> List (Html Msg)
viewComponent initParams sc l =
    let
        url = Builder.relative
            [ initParams.flags.candorUrl
            , "image"
            , LanguageHelpers.contentCodeStringFromLanguage initParams.knownLanguage
            , LanguageHelpers.contentCodeStringFromLanguage initParams.learningLanguage
            , initParams.projectName
            , sc.name
            ] []
        entry =
            tr
                [ ]
                [ text sc.name
                , Html.node "clipboard-copy"
                    [ attribute "value" url
                    , class "w3-button w3-black w3-round"
                    ]
                    [ text "Copy URL to clipboard" ]
                ]
    in
    entry :: l

viewComponents : InitParams -> List SlideComponent -> Html Msg
viewComponents initParams components =
    div
        [ ]
        [ viewLoadImageFromFile
        , List.foldl (viewComponent initParams) [ ] components
          |> List.reverse
          |> table [ class "edit-page-slide-components-table" ]
        ]

viewSlideComponents : Model -> Html Msg
viewSlideComponents { images, initParams, sounds, videos } =
    case ( images, sounds, videos ) of
        ( VisibleImages vis, HiddenSounds _, HiddenVideos _ ) ->
            viewComponents initParams vis

        ( HiddenImages _, VisibleSounds vss, HiddenVideos _ ) ->
            viewComponents initParams vss

        ( HiddenImages _, HiddenSounds _, VisibleVideos vvs ) ->
            viewComponents initParams vvs

        _ ->
            div [ ] [ ]

viewSlideComponentsArea : Model -> (String, Html Msg)
viewSlideComponentsArea ( { initParams, slideId } as model ) =
    ( "candor-slide-components" ++ slideId
    , div
        [ ]
        [ div
            [ ]
            [ viewSlideImages model
            , viewSlideSounds model
            , viewSlideVideos model
            ]
        , viewSlideComponents model
        ]
    )

viewQuestionsArea : Model -> (String, Html Msg)
viewQuestionsArea { initParams, questionsArea, slideId } =
    ( "candor-question-area-" ++ slideId
    , QuestionsArea.view questionsArea
        |> Html.map QuestionsAreaMsg
    )

view : Model -> Html Msg
view model =
    Keyed.node "div"
        [ ]
        [ viewTinyMCEEditor model
        , viewSlideComponentsArea model
        , viewQuestionsArea model
        ]
