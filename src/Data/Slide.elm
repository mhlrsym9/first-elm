module Data.Slide exposing (encodeSlide, establishIndexes, init, InitParams, Model, Msg(..), slideDecoder, storeSlideContents, textToString, update, updateSlideIndex, view)

import Api
import Bytes exposing (Bytes)
import Data.QuestionsArea as QuestionsArea
import Dict exposing (Dict)
import Element exposing (centerX, centerY, Column, column, el, Element, fill, html, padding, row, spacing, table, width)
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import File exposing (File)
import File.Select as Select
import Flags exposing (Flags)
import Http exposing (bytesBody, bytesResolver, stringResolver)
import Html exposing (Html, text)
import Html.Attributes exposing (attribute, class)
import Json.Decode exposing (Decoder, field, list, map, string, succeed)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import Json.Encode as Encode
import LanguageHelpers
import List.Extra
import Procedure exposing (Procedure)
import Procedure.Program
import Random
import Task exposing (Task)
import UIHelpers exposing (buttonAttributes)
import Url.Builder as Builder
import UUID exposing (Seeds)

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
    { description : String
    , id : String
    }

type alias TransferResponse =
    { id : String }

type Images
    = HiddenImages (List SlideComponent)
    | VisibleImages (List SlideComponent)

type Sounds
    = HiddenSounds (List SlideComponent)
    | VisibleSounds (List SlideComponent)

type Videos
    = HiddenVideos (List SlideComponent)
    | VisibleVideos (List SlideComponent)

type ComponentType
    = Image
    | Sound
    | Video

type ProcedureError
    = HttpError Http.Error
    | NoMimeType

type alias Model =
    { componentDescription : String
    , componentUrl : String
    , images : Images
    , initParams : InitParams
    , procModel : Procedure.Program.Model Msg
    , questionsArea : QuestionsArea.Model
    , seeds : Seeds
    , slideId : String
    , slideIndex : Int
    , slideText : Text
    , sounds : Sounds
    , videos : Videos
    }

slideComponentDecoder : Decoder SlideComponent
slideComponentDecoder =
    succeed SlideComponent
        |> required "description" string
        |> required "id" string

slideComponentsDecoder : Decoder (List SlideComponent)
slideComponentsDecoder =
    list slideComponentDecoder

imagesAreaDecoder : Decoder Images
imagesAreaDecoder =
    map HiddenImages slideComponentsDecoder

initialSeeds : Flags.Model -> Seeds
initialSeeds { seeds } =
    case seeds of
        ( a :: b :: c :: d :: _ ) ->
            Seeds a b c d
        _ ->
            (Seeds
                (Random.initialSeed 12345)
                (Random.initialSeed 23456)
                (Random.initialSeed 34567)
                (Random.initialSeed 45678)
            )

soundsAreaDecoder : Decoder Sounds
soundsAreaDecoder =
    map HiddenSounds slideComponentsDecoder

videosAreaDecoder : Decoder Videos
videosAreaDecoder =
    map HiddenVideos slideComponentsDecoder

slideDecoder : InitParams -> Decoder Model
slideDecoder ( { flags } as initParams ) =
    succeed Model
        |> hardcoded ""
        |> hardcoded ""
        |> optional "images" imagesAreaDecoder (HiddenImages [])
        |> hardcoded initParams
        |> hardcoded Procedure.Program.init
        |> required "questionsarea" QuestionsArea.questionsAreaDecoder
        |> hardcoded ( initialSeeds flags )
        |> hardcoded UUID.nilString
        |> hardcoded 0
        |> custom slideTextDecoder
        |> optional "sounds" soundsAreaDecoder (HiddenSounds [])
        |> optional "videos" videosAreaDecoder (HiddenVideos [])

encodeSlideComponent : SlideComponent -> Encode.Value
encodeSlideComponent { description, id } =
    Encode.object
        [ ( "description", Encode.string description )
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
init ( { flags } as initParams ) slideId slideIndex =
    let
        questionsArea = QuestionsArea.init { slideIndex = slideIndex }
    in
    { componentDescription = ""
    , componentUrl = ""
    , images = HiddenImages [ ]
    , initParams = initParams
    , procModel = Procedure.Program.init
    , questionsArea = questionsArea
    , seeds = initialSeeds flags
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
    = ComponentDescriptionInput String
    | ComponentUrlInput String
    | CopyUrl String
    | ImageRequested
    | ImageTransferred (Result ProcedureError SlideComponent)
    | ImageUrlRequested
    | MakeDirty
    | ProcedureMsg (Procedure.Program.Msg Msg)
    | QuestionsAreaMsg QuestionsArea.Msg
    | SoundRequested
    | SoundTransferred (Result ProcedureError SlideComponent)
    | SoundUrlRequested
    | UpdateImagesVisibility Images Sounds Videos
    | UpdateSoundsVisibility Images Sounds Videos
    | UpdateVideosVisibility Images Sounds Videos
    | VideoRequested
    | VideoTransferred (Result ProcedureError SlideComponent)
    | VideoUrlRequested

storeSlideContents : String -> Model -> Model
storeSlideContents slideContents model =
    { model | slideText = Text slideContents }

transferResponseDecoder : Decoder TransferResponse
transferResponseDecoder =
    succeed TransferResponse
        |> required "id" string

mimeToRestCall : String -> String
mimeToRestCall mimeType =
    case mimeType of
        "image/jpeg" ->
            "image"
        "image/png" ->
            "image"
        "audio/mpeg" ->
            "audio"
        "video/mp4" ->
            "video"
        _ ->
            ".xxx"

mimeToExt : String -> String
mimeToExt mimeType =
    case mimeType of
        "image/jpeg" ->
            ".jpg"
        "image/png" ->
            ".png"
        "audio/mpeg" ->
            ".mp3"
        "video/mp4" ->
            ".mp4"
        _ ->
            ".xxx"

transferToServer : Model -> String -> String -> Bytes -> Task Http.Error TransferResponse
transferToServer { initParams } fileName fileMime componentBytes =
    let
        url = Builder.relative
            [ initParams.flags.candorUrl
            , (mimeToRestCall fileMime)
            , LanguageHelpers.contentCodeStringFromLanguage initParams.knownLanguage
            , LanguageHelpers.contentCodeStringFromLanguage initParams.learningLanguage
            , initParams.projectName
            , fileName ++ (mimeToExt fileMime)
            ] []
    in
    Http.task
        { method = "POST"
        , headers = []
        , url = url
        , body = bytesBody fileMime componentBytes
        , resolver = stringResolver (Api.handleJsonResponse transferResponseDecoder)
        , timeout = Nothing
        }

addComponentToProject : Model -> (List String) -> ( (Result ProcedureError SlideComponent) -> Msg ) -> (Model, Cmd Msg)
addComponentToProject ( { componentDescription, seeds } as model ) mimeTypes transferred =
    let
        (uuid, updatedSeeds) = UUID.step seeds
    in
    ( { model | componentDescription = "", componentUrl = "", seeds = updatedSeeds }
    , Procedure.fetch (Select.file mimeTypes)
        |> Procedure.andThen
            (\f ->
                File.toBytes f
                    |> Procedure.fromTask
                    |> Procedure.map
                        (\bytes -> (f, bytes))
            )
        |> Procedure.andThen
            (\(f, bytes) ->
                let
                    fileMime = (File.mime f)
                    fname = UUID.toString uuid
                in
                (transferToServer model fname fileMime bytes)
                    |> Procedure.fromTask
            )
        |> Procedure.map ( \{ id } -> { description = componentDescription, id = id  } )
        |> Procedure.mapError (\err -> HttpError err)
        |> Procedure.try ProcedureMsg transferred
    )

fetchComponent : String -> Task Http.Error Api.BytesWithHeaders
fetchComponent componentUrl =
    Http.task
        { method = "GET"
        , headers = []
        , url = componentUrl
        , body = Http.emptyBody
        , resolver = bytesResolver (Api.handleBytesResponse Ok)
        , timeout = Nothing
        }

findValidMimeType : List String -> Maybe String
findValidMimeType ls =
    case ls of
        s :: _ ->
            List.Extra.find (\el -> el == s) ["image/jpeg" , "image/png", "audio/mpg", "video/mp4"]

        _ ->
            Nothing

findMimeType : Dict String String -> Maybe String
findMimeType d =
    let
        c = Dict.get "Content-Type" d
    in
    case c of
        Just ct ->
            let
                p = String.split ";" ct
            in
            findValidMimeType p

        Nothing ->
            Nothing

addUrlComponentToProject : Model -> ( (Result ProcedureError SlideComponent) -> Msg ) -> (Model, Cmd Msg)
addUrlComponentToProject ( { componentDescription, componentUrl, seeds } as model ) transferred =
    let
        (uuid, updatedSeeds) = UUID.step seeds
        fname = UUID.toString uuid
    in
    ( { model | componentDescription = "", componentUrl = "", seeds = updatedSeeds }
    , fetchComponent componentUrl
        |> Procedure.fromTask
        |> Procedure.mapError (\err -> HttpError err)
        |> Procedure.andThen
            (\{ bytes, headers } ->
                let
                    mimeType = findMimeType headers
                in
                case mimeType of
                    Just mt ->
                        Procedure.provide (bytes, mt)
                    _ ->
                        Procedure.break NoMimeType
            )
        |> Procedure.andThen
            (\(bytes, mt) ->
                (transferToServer model fname mt bytes)
                    |> Procedure.fromTask
                    |> Procedure.mapError (\err -> HttpError err)
            )
        |> Procedure.map ( \{ id } -> { description = componentDescription, id = id } )
        |> Procedure.try ProcedureMsg transferred
    )

makeProjectDirty : Cmd Msg
makeProjectDirty =
    Task.perform ( always MakeDirty ) ( Task.succeed () )

update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { images, procModel, questionsArea, sounds, videos } as model ) =
    case msg of
        ComponentDescriptionInput s ->
            ( { model | componentDescription = s }, Cmd.none )

        ComponentUrlInput s ->
            ( { model | componentUrl = s }, Cmd.none )

        CopyUrl _ ->
            ( model, Cmd.none )

        ImageRequested ->
            addComponentToProject model ["image/jpeg", "image/png"] ImageTransferred

        ImageTransferred result ->
            case result of
                Ok image ->
                    case images of
                        VisibleImages l ->
                            ( { model | images = VisibleImages ( image :: l ) }
                            , makeProjectDirty
                            )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ImageUrlRequested ->
            addUrlComponentToProject model ImageTransferred

-- Handled in Project module
        MakeDirty ->
            ( model, Cmd.none )

        ProcedureMsg procMsg ->
            Procedure.Program.update procMsg procModel
                |> Tuple.mapFirst (\updated -> { model | procModel = updated })

        QuestionsAreaMsg questionsAreaMsg ->
            case questionsAreaMsg of
                QuestionsArea.MakeDirty ->
                    ( model, makeProjectDirty )

                _ ->
                    let
                        ( updatedQuestionsAreaModel, commands ) =
                            QuestionsArea.update questionsAreaMsg questionsArea
                    in
                    ( { model | questionsArea = updatedQuestionsAreaModel }
                    , Cmd.map QuestionsAreaMsg commands
                    )

        SoundRequested ->
            addComponentToProject model ["audio/mpg"] SoundTransferred

        SoundTransferred result ->
            case result of
                Ok sound ->
                    case sounds of
                        VisibleSounds l ->
                            ( { model | sounds = VisibleSounds ( sound :: l ) }, makeProjectDirty )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        SoundUrlRequested ->
            addUrlComponentToProject model SoundTransferred

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

        VideoRequested ->
            addComponentToProject model ["video/mp4"] SoundTransferred

        VideoTransferred result ->
            case result of
                Ok video ->
                    case videos of
                        VisibleVideos l ->
                            ( { model | videos = VisibleVideos ( video :: l ) }, makeProjectDirty )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        VideoUrlRequested ->
            addUrlComponentToProject model VideoTransferred

-- VIEW

viewTinyMCEEditor : Model -> (String, Element Msg)
viewTinyMCEEditor { initParams, slideId, slideText } =
    let
        node = Html.node "tinymce-editor"
           [ attribute "api-key" "no-api-key"
           , attribute "config" initParams.flags.editorConfigName
           , attribute "height" "500"
           , attribute "plugins" "link image anchor media table paste code help"
           , attribute "toolbar" "undo redo | bold italic underline | forecolor backcolor | alignleft aligncenter alignright | fontselect fontsizeselect | code | help"
           , attribute "setup" initParams.flags.setupEditorName
           ]
           [ text (textToString slideText) ]
    in
    ( "tinymce-editor-" ++ slideId
    , Element.el [ centerX, width fill ] ( html node ) )

viewSlideComponentsHeader : Element Msg
viewSlideComponentsHeader =
    el
        [ Font.size 24
        , centerX
        ]
        ( Element.text "Slide Components" )

viewSlideImages : Model -> Element Msg
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
    Input.button
        buttonAttributes
        { onPress = Just ( UpdateImagesVisibility updatedImages updatedSounds updatedVideos )
        , label = Element.text "Manage Slide Images"
        }

viewSlideSounds : Model -> Element Msg
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
    Input.button
        buttonAttributes
        { onPress = Just ( UpdateSoundsVisibility updatedImages updatedSounds updatedVideos )
        , label = Element.text "Manage Slide Sounds"
        }

viewSlideVideos : Model -> Element Msg
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
    Input.button
        buttonAttributes
        { onPress = Just ( UpdateVideosVisibility updatedImages updatedSounds updatedVideos )
        , label = Element.text "Manage Slide Videos"
        }

toBaseUrl : ComponentType -> String
toBaseUrl t =
    case t of
        Image ->
            "image"
        Sound ->
            "audio"
        Video ->
            "video"

viewSlideComponentButtons : Model -> Element Msg
viewSlideComponentButtons model =
    row
        [ centerX
        , spacing 10
        ]
        [ viewSlideImages model
        , viewSlideSounds model
        , viewSlideVideos model
        ]

toComponentDescription : ComponentType -> String
toComponentDescription t =
    let
        header =
            case t of
                Image ->
                    "image"
                Sound ->
                    "sound"
                Video ->
                    "video"
    in
    "Description of " ++ header ++ " to stage:"

viewComponentDescription : Model -> ComponentType -> Element Msg
viewComponentDescription { componentDescription } t =
    Input.text
        [ ]
        { onChange = ComponentDescriptionInput
        , text = componentDescription
        , placeholder = Just (Input.placeholder [ ] (Element.text "Supply a desciption here."))
        , label = Input.labelLeft [ ] (Element.text (toComponentDescription t))
        }

toLoadComponentButtonText : ComponentType -> Element Msg
toLoadComponentButtonText t =
    let
        s =
            case t of
                Image ->
                    "image"
                Sound ->
                    "sound"
                Video ->
                    "video"
    in
    Element.text ("Select " ++ s ++ " file to stage")

toLoadComponentButtonMsg : ComponentType -> Maybe Msg
toLoadComponentButtonMsg t =
    let
        m =
            case t of
                Image ->
                    ImageRequested
                Sound ->
                    SoundRequested
                Video ->
                    VideoRequested
    in
    Just m

viewLoadComponentFromFile : Model -> ComponentType -> Element Msg
viewLoadComponentFromFile { componentDescription } componentType =
    if (String.isEmpty componentDescription) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = toLoadComponentButtonMsg componentType
            , label = toLoadComponentButtonText componentType
            }

toLoadUrlComponentLabelText : ComponentType -> String
toLoadUrlComponentLabelText t =
    let
        s =
            case t of
                Image ->
                    "image"
                Sound ->
                    "sound"
                Video ->
                    "video"
    in
    "URL of " ++ s ++ " to stage:"

toLoadUrlComponentButtonText : ComponentType -> Element Msg
toLoadUrlComponentButtonText t =
    let
        s =
            case t of
                Image ->
                    "image"
                Sound ->
                    "sound"
                Video ->
                    "video"
    in
    Element.text ("Stage " ++ s ++ " on server")

toLoadUrlComponentButtonMsg : ComponentType -> Maybe Msg
toLoadUrlComponentButtonMsg t =
    let
        m =
            case t of
                Image ->
                    ImageUrlRequested
                Sound ->
                    SoundUrlRequested
                Video ->
                    VideoUrlRequested
    in
    Just m

viewLoadComponentFromUrl : Model -> ComponentType -> Element Msg
viewLoadComponentFromUrl { componentDescription, componentUrl } componentType =
    row
        [ spacing 10 ]
        [ Input.text
            [ ]
            { onChange = ComponentUrlInput
            , text = componentUrl
            , placeholder = Just (Input.placeholder [ ] (Element.text "Supply a valid URL here."))
            , label = Input.labelLeft [ ] (Element.text ( toLoadUrlComponentLabelText componentType ))
            }
        , Input.button
            buttonAttributes
            { onPress = toLoadUrlComponentButtonMsg componentType
            , label = toLoadUrlComponentButtonText componentType
            }
        ]

viewStagedComponentsHeader : ComponentType -> Element Msg
viewStagedComponentsHeader ct =
    let
        s =
            case ct of
                Image ->
                    "Image"
                Sound ->
                    "Sound"
                Video ->
                    "Video"
    in
    el
        [ Font.size 24
        , centerX
        ]
        ( Element.text ("Staged " ++ s ++ " Components") )

prepareDescription : Column SlideComponent Msg
prepareDescription =
    { header = Element.text "Description"
    , width = fill
    , view =
        \{ description } ->
            el [ centerY ] (Element.text description)
    }

prepareCopyUrlButton : InitParams -> ComponentType -> Column SlideComponent Msg
prepareCopyUrlButton { flags, knownLanguage, learningLanguage, projectName } componentType =
    { header = Element.none
    , width = fill
    , view =
        \{ description, id } ->
            let
                url = Builder.relative
                    [ flags.candorUrl
                    , ( toBaseUrl componentType )
                    , LanguageHelpers.contentCodeStringFromLanguage knownLanguage
                    , LanguageHelpers.contentCodeStringFromLanguage learningLanguage
                    , projectName
                    , id
                    ] []
                node = Html.node "clipboard-copy"
                    [ attribute "value" url
                    , class "w3-button w3-black w3-round"
                    ]
                    [ text "Copy URL to clipboard" ]
            in
            html node
    }

viewComponents : Model -> InitParams -> ComponentType -> List SlideComponent -> Element Msg
viewComponents model initParams componentType components =
    let
        ct =
            case componentType of
                Image ->
                    "image"
                Sound ->
                    "sound"
                Video ->
                    "video"
        t =
            if (List.isEmpty components) then
                Element.text ("No staged " ++ ct ++ " components")
            else
                table
                    [ spacing 10 ]
                    { data = components
                    , columns =
                        [ prepareDescription
                        , prepareCopyUrlButton initParams componentType
                        ]
                    }
    in
    column
        [ centerX
        , spacing 10
        ]
        [ viewComponentDescription model componentType
        , viewLoadComponentFromFile model componentType
        , viewLoadComponentFromUrl model componentType
        , viewStagedComponentsHeader componentType
        , t
        ]

viewSlideComponents : Model -> Element Msg
viewSlideComponents ( { images, initParams, sounds, videos } as model ) =
    case ( images, sounds, videos ) of
        ( VisibleImages vis, HiddenSounds _, HiddenVideos _ ) ->
            viewComponents model initParams Image vis

        ( HiddenImages _, VisibleSounds vss, HiddenVideos _ ) ->
            viewComponents model initParams Sound vss

        ( HiddenImages _, HiddenSounds _, VisibleVideos vvs ) ->
            viewComponents model initParams Video vvs

        _ ->
            Element.none

viewSlideComponentsArea : Model -> (String, Element Msg)
viewSlideComponentsArea ( { initParams, slideId } as model ) =
    ( "candor-slide-components" ++ slideId
    , column
        [ centerX
        , spacing 10
        ]
        [ viewSlideComponentsHeader
        , viewSlideComponentButtons model
        , viewSlideComponents model
        ]
    )

viewQuestionsArea : Model -> (String, Element Msg)
viewQuestionsArea { initParams, questionsArea, slideId } =
    ( "candor-question-area-" ++ slideId
    , QuestionsArea.view questionsArea
        |> Element.map QuestionsAreaMsg
    )

view : Model -> Element Msg
view model =
    Keyed.column
        [ Font.size 14
        , centerX
        , spacing 10
        , padding 10
        ]
        [ viewTinyMCEEditor model
        , viewSlideComponentsArea model
        , viewQuestionsArea model
        ]
