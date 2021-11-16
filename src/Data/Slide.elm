module Data.Slide exposing (encodeSlide, establishIndexes, init, Model, Msg, slideDecoder, storeSlideContents, textToString, update, updateSlideIndex, view)

import Data.QuestionsArea as QuestionsArea
import Html exposing (button, div, Html, table, text, tr)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Json.Decode exposing (Decoder, field, list, map, string, succeed)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import Json.Encode as Encode
import UUID

-- MODEL

type Text =
    Text String

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
    , questionsArea : QuestionsArea.Model
    , setupEditorName : String
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
        |> required "id" string

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

slideDecoder : String -> Decoder Model
slideDecoder sen =
    succeed Model
        |> optional "images" imagesAreaDecoder (HiddenImages [])
        |> required "questionsarea" QuestionsArea.questionsAreaDecoder
        |> hardcoded sen
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

init : { slideIndex : Int, sen : String, slideId : String } -> Model
init { slideIndex, sen, slideId } =
    let
        questionsArea = QuestionsArea.init { slideIndex = slideIndex }
    in
    { images = HiddenImages [ ]
    , slideText = Text "This is a test"
    , slideIndex = slideIndex
    , setupEditorName = sen
    , questionsArea = questionsArea
    , slideId = slideId
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
    | QuestionsAreaMsg QuestionsArea.Msg
    | UpdateImagesVisibility Images Sounds Videos
    | UpdateSoundsVisibility Images Sounds Videos
    | UpdateVideosVisibility Images Sounds Videos

storeSlideContents : String -> Model -> Model
storeSlideContents slideContents model =
    { model | slideText = Text slideContents }

update : Msg -> Model -> Model
update msg ( { questionsArea } as model ) =
    case msg of
        CopyUrl url ->
            model

        QuestionsAreaMsg questionsAreaMsg ->
            let
                updatedQuestionsAreaModel =
                    QuestionsArea.update questionsAreaMsg questionsArea
            in
            { model | questionsArea = updatedQuestionsAreaModel }

        UpdateImagesVisibility updatedImages updatedSounds updatedVideos ->
            { model
            | images = updatedImages
            , sounds = updatedSounds
            , videos = updatedVideos
            }

        UpdateSoundsVisibility updatedImages updatedSounds updatedVideos ->
            { model
            | images = updatedImages
            , sounds = updatedSounds
            , videos = updatedVideos
            }

        UpdateVideosVisibility updatedImages updatedSounds updatedVideos ->
            { model
            | images = updatedImages
            , sounds = updatedSounds
            , videos = updatedVideos
            }

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

viewComponent : SlideComponent -> List (Html Msg) -> List (Html Msg)
viewComponent sc l =
    let
        entry =
            tr
                [ ]
                [ text sc.name
                , Html.node "clipboard-copy"
                    [ attribute "value" sc.name
                    , class "button"
                    ]
                    [ text "Copy URL to clipboard" ]
                ]
    in
    entry :: l

viewComponents : List SlideComponent -> Html Msg
viewComponents components =
    List.foldl viewComponent [ ] components
        |> List.reverse
        |> table [ class "edit-page-slide-components-table" ]

viewSlideComponents : Model -> Html Msg
viewSlideComponents { images, sounds, videos } =
    case ( images, sounds, videos ) of
        ( VisibleImages vis, HiddenSounds _, HiddenVideos _ ) ->
            viewComponents vis

        ( HiddenImages _, VisibleSounds vss, HiddenVideos _ ) ->
            viewComponents vss

        ( HiddenImages _, HiddenSounds _, VisibleVideos vvs ) ->
            viewComponents vvs

        _ ->
            div [ ] [ ]

viewSlideComponentsArea : Model -> (String, Html Msg)
viewSlideComponentsArea ( { slideId } as model ) =
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
        , viewSlideComponentsArea model
        , viewQuestionsArea model
        ]
