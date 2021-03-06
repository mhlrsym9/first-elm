module Data.Slide exposing (encodeSlide, establishIndexes, init, InitParams, Model, Msg(..), processDirtySlideTextMessage, slideDecoder, storeSlideContents, textToString, update, updateSeeds, updateSlide, updateSlideIndex, view)

import Api
import Bytes exposing (Bytes)
import Data.QuestionsArea as QuestionsArea
import Dialog exposing (Config)
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (alignRight, centerX, centerY, Column, column, el, Element, fill, html, padding, paragraph, px, row, spacing, table, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import File exposing (File)
import File.Select as Select
import Flags exposing (Flags)
import Http exposing (bytesBody, bytesResolver, stringResolver)
import Html exposing (Html, text)
import Html.Attributes exposing (attribute, class, title)
import Json.Decode exposing (bool, Decoder, field, list, map, string, succeed)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import Json.Encode as Encode
import LanguageHelpers
import List.Extra
import Loading
import MessageHelpers exposing (sendCommandMessage)
import Procedure exposing (Procedure)
import Procedure.Program
import Task exposing (Task)
import UIHelpers exposing (buttonAttributes, lightGrey, red, white)
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
    , markedForDeletion : Bool
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

toBaseUrl : ComponentType -> String
toBaseUrl t =
    case t of
        Image ->
            "image"
        Sound ->
            "audio"
        Video ->
            "video"

toText : ComponentType -> String
toText t =
    case t of
        Image ->
            "image"
        Sound ->
            "sound"
        Video ->
            "video"

toCapitalizedText : ComponentType -> String
toCapitalizedText t =
    case t of
        Image ->
            "Image"
        Sound ->
            "Sound"
        Video ->
            "Video"

toCapitalizedPluralText : ComponentType -> String
toCapitalizedPluralText t =
    case t of
        Image ->
            "Images"
        Sound ->
            "Sounds"
        Video ->
            "Videos"

toMimeTypes : ComponentType -> List String
toMimeTypes ct =
    case ct of
        Image ->
            [ "image/jpeg", "image/png" ]

        Sound ->
            [ "audio/mpg" ]

        Video ->
            [ "video/mp4" ]

type ProcedureError
    = HttpError Http.Error
    | NoMimeType
    | NoError

type alias TransferResult = Result ProcedureError SlideComponent
type alias TransferResultToMessage = TransferResult -> Msg

type MediaRequest
    = File
    | Url

type MediaStatus
    = Updated
    | Updating
    | UpdatingSlowly
    | Failed

type alias Model =
    { componentDescription : String
    , componentUrl : String
    , images : Images
    , initParams : InitParams
    , isSlideTextDirty : Bool
    , procModel : Procedure.Program.Model Msg
    , questionsArea : QuestionsArea.Model
    , slideId : String
    , slideIndex : Int
    , slideText : Text
    , sounds : Sounds
    , status : MediaStatus
    , videos : Videos
    }

slideComponentDecoder : Decoder SlideComponent
slideComponentDecoder =
    succeed SlideComponent
        |> required "description" string
        |> required "id" string
        |> optional "markedForDeletion" bool False

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
slideDecoder ( { flags } as initParams ) =
    succeed Model
        |> hardcoded ""
        |> hardcoded ""
        |> optional "images" imagesAreaDecoder (HiddenImages [])
        |> hardcoded initParams
        |> hardcoded False
        |> hardcoded Procedure.Program.init
        |> required "questionsarea" QuestionsArea.questionsAreaDecoder
        |> hardcoded UUID.nilString
        |> hardcoded 0
        |> custom slideTextDecoder
        |> optional "sounds" soundsAreaDecoder (HiddenSounds [])
        |> hardcoded Updated
        |> optional "videos" videosAreaDecoder (HiddenVideos [])

encodeSlideComponent : SlideComponent -> Encode.Value
encodeSlideComponent { description, id, markedForDeletion } =
    Encode.object
        [ ( "description", Encode.string description )
        , ( "id", Encode.string id )
        , ( "markedForDeletion", Encode.bool markedForDeletion)
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
    { componentDescription = ""
    , componentUrl = ""
    , images = HiddenImages [ ]
    , initParams = initParams
    , isSlideTextDirty = False
    , procModel = Procedure.Program.init
    , questionsArea = questionsArea
    , slideId = slideId
    , slideIndex = slideIndex
    , slideText = Text "This is a test"
    , sounds = HiddenSounds [ ]
    , status = Updated
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

updateSlide : Model -> (Model, Bool)
updateSlide ( model ) =
    ( model, False )

updateSeeds : Model -> Seeds -> Model
updateSeeds ( { initParams } as model ) updatedSeeds =
    let
        updatedInitParams = { initParams | flags = Flags.updateSeeds initParams.flags updatedSeeds }
    in
    { model | initParams = updatedInitParams }

processDirtySlideTextMessage : Model -> Bool -> Model
processDirtySlideTextMessage model isDirty =
    { model | isSlideTextDirty = isDirty }

{-
gatherMediaToDelete : Model -> List String
gatherMediaToDelete { images, sounds, videos } =
    let
        imageList = extractImageComponents images
        soundList = extractSoundComponents sounds
        videoList = extractVideoComponents videos
    in
    List.concatMap
        (\l ->
            List.map
                (\{id} -> id)
                (
                    List.filter
                        (\{markedForDeletion} -> markedForDeletion)
                        l
                )
        )
        [ imageList, soundList, videoList ]
-}

-- UPDATE

type Msg
    = Cancelled
    | ComponentDescriptionInput String
    | ComponentUrlInput ComponentType String
    | CopyUrl String
    | DeleteMedia ComponentType String
    | MakeProjectDirty
    | MediaRequested ComponentType MediaRequest
    | MediaTransferred ComponentType Seeds TransferResult
    | PassedSlowThreshold
    | PrepareMediaRequestDialog ComponentType MediaRequest
    | ProcedureMsg (Procedure.Program.Msg Msg)
    | QuestionsAreaMsg QuestionsArea.Msg
    | ShowDialog (Maybe (Config Msg))
    | UpdateSeeds Seeds
    | UpdateVisibility Images Sounds Videos

storeSlideContents : String -> Model -> Model
storeSlideContents slideContents model =
    { model | isSlideTextDirty = False, slideText = Text slideContents }

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

generateTransferProcedure : Procedure ProcedureError (String, Bytes) Msg -> Model -> ComponentType -> (Model, Cmd Msg)
generateTransferProcedure p ( { componentDescription, initParams } as model ) ct =
    let
        (uuid, updatedSeeds) = UUID.step initParams.flags.seeds
        fname = UUID.toString uuid
    in
-- seeds will be updated when project calls updateSeeds for each slide in the project!
    ( { model | componentDescription = "", componentUrl = "" }
    , p
        |> Procedure.andThen
            (\(mimeType, bytes) ->
                (transferToServer model fname mimeType bytes)
                    |> Procedure.fromTask
                    |> Procedure.mapError (\err -> HttpError err)
            )
        |> Procedure.map ( \{ id } -> { description = componentDescription, id = id, markedForDeletion = False } )
        |> Procedure.try ProcedureMsg (MediaTransferred ct updatedSeeds)
    )

addComponentToProject : Model -> ComponentType-> (Model, Cmd Msg)
addComponentToProject model ct =
    let
        mimeTypes = toMimeTypes ct
        preliminaryProcedure =
            Procedure.fetch ( Select.file mimeTypes )
                |> Procedure.mapError (\_ -> NoError)
                |> Procedure.andThen
                    (\f ->
                       File.toBytes f
                           |> Procedure.fromTask
                           |> Procedure.map
                               (\bytes -> (File.mime f, bytes))
                    )
    in
    generateTransferProcedure preliminaryProcedure model ct

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
            List.Extra.find ( \el -> el == ( String.toLower s ) ) ["image/jpeg" , "image/png", "audio/mpg", "video/mp4"]

        _ ->
            Nothing

findMimeType : Dict String String -> Maybe String
findMimeType d =
    let
        c = Dict.get "content-type" (Dict.Extra.mapKeys String.toLower d)
    in
    case c of
        Just ct ->
            let
                p = String.split ";" ct
            in
            findValidMimeType p

        Nothing ->
            Nothing

addUrlComponentToProject : Model -> ComponentType -> (Model, Cmd Msg)
addUrlComponentToProject ( { componentUrl } as model ) ct =
    let
        preliminaryProcedure =
            Procedure.do ( showDialog Nothing )
                |> Procedure.mapError (\_ -> NoError)
                |> Procedure.andThen
                    (\_ ->
                        fetchComponent componentUrl
                            |> Procedure.fromTask
                            |> Procedure.mapError (\err -> HttpError err)
                    )
                |> Procedure.andThen
                    (\{ bytes, headers } ->
                        let
                            theMimeType = findMimeType headers
                        in
                        case theMimeType of
                            Just mt ->
                                Procedure.provide (mt, bytes)
                            _ ->
                                Procedure.break NoMimeType
                    )
    in
    generateTransferProcedure preliminaryProcedure model ct

makeProjectDirty : Cmd Msg
makeProjectDirty =
    sendCommandMessage MakeProjectDirty

showDialog : Maybe (Config Msg) -> Cmd Msg
showDialog config =
    sendCommandMessage (ShowDialog config)

{-
type alias DeleteResult =
    { id : String }

deleteMediaDecoder : Decoder DeleteResult
deleteMediaDecoder =
    succeed DeleteResult
        |> required "id" string

deleteMediaTask : InitParams -> ComponentType -> String -> Task Http.Error DeleteResult
deleteMediaTask { flags, knownLanguage, learningLanguage, projectName } componentType id =
    let
        kcc = LanguageHelpers.contentCodeStringFromLanguage knownLanguage
        lcc = LanguageHelpers.contentCodeStringFromLanguage learningLanguage
        url = Builder.relative [flags.candorUrl, (toBaseUrl componentType), kcc, lcc, projectName, id] []
    in
    Http.task
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = stringResolver ( Api.handleJsonResponse deleteMediaDecoder )
        , timeout = Nothing
        }
-}

notMatchSlideComponentId : String -> SlideComponent -> SlideComponent
notMatchSlideComponentId idToMarkForDeletion sc =
    if (idToMarkForDeletion == sc.id) then
        { sc | markedForDeletion = True }
    else
        sc

markMediaForDeletion : Model -> ComponentType -> String -> Model
markMediaForDeletion ( { images, sounds, videos } as model ) ct idToMarkForDeletion =
    let
        mapFnc = List.map (notMatchSlideComponentId idToMarkForDeletion)
        updatedModel =
            case ct of
                Image ->
                    let
                        updatedImages =
                            case images of
                                VisibleImages l ->
                                    VisibleImages (mapFnc l)

                                HiddenImages l ->
                                    HiddenImages (mapFnc l)
                    in
                    { model | images = updatedImages }

                Sound ->
                    let
                        updatedSounds =
                            case sounds of
                                VisibleSounds l ->
                                    VisibleSounds (mapFnc l)

                                HiddenSounds l ->
                                    HiddenSounds (mapFnc l)
                    in
                    { model | sounds = updatedSounds }

                Video ->
                    let
                        updatedVideos =
                            case videos of
                                VisibleVideos l ->
                                    VisibleVideos (mapFnc l)

                                HiddenVideos l ->
                                    HiddenVideos (mapFnc l)
                    in
                    { model | videos = updatedVideos }
    in
    { updatedModel | status = Updated }

update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { images, procModel, questionsArea, sounds, videos } as model ) =
    case msg of
        Cancelled ->
            ( { model | componentDescription = "", componentUrl = "" }, showDialog Nothing)

        ComponentDescriptionInput s ->
            ( { model | componentDescription = s }, Cmd.none )

        ComponentUrlInput ct s ->
            let
                updatedModel = { model | componentUrl = s }
                config = prepareUrlConfig updatedModel ct Url
            in
            ( updatedModel, showDialog ( Just config ) )

        CopyUrl _ ->
            ( model, Cmd.none )

        DeleteMedia ct id ->
            ( markMediaForDeletion model ct id
            , makeProjectDirty
            )

-- Handled in Project module
        MakeProjectDirty ->
            ( model, Cmd.none )

        MediaRequested ct mr ->
            case mr of
                File ->
                    let
                        (updatedModel, commands) = addComponentToProject model ct
                    in
                    ( { updatedModel | status = Updating }
                    , Cmd.batch
                        [ commands
                        , Task.perform (\_ -> PassedSlowThreshold) Loading.slowThreshold
                        ]
                    )

                Url ->
                    addUrlComponentToProject model ct

        MediaTransferred ct updatedSeeds result ->
            let
                updateSeedCommand = sendCommandMessage (UpdateSeeds updatedSeeds)
                commands = Cmd.batch [ updateSeedCommand, makeProjectDirty ]
                updatedModel = { model | status = Updated }
            in
            case result of
                Ok media ->
                    case ct of
                        Image ->
                           case images of
                                VisibleImages l ->
                                    ( { updatedModel | images = VisibleImages ( media :: l ) }
                                    , commands
                                    )

                                _ ->
                                    ( updatedModel, Cmd.none )

                        Sound ->
                            case sounds of
                                VisibleSounds l ->
                                    (
                                        { updatedModel
                                        | sounds = VisibleSounds ( media :: l )
                                        }
                                        , commands
                                    )

                                _ ->
                                    ( updatedModel, Cmd.none )

                        Video ->
                            case videos of
                                VisibleVideos l ->
                                    (
                                        { updatedModel
                                        | videos = VisibleVideos ( media :: l )
                                        }
                                        , commands
                                    )

                                _ ->
                                    ( updatedModel, Cmd.none )

                Err _ ->
                    ( { model | status = Failed }
                    , updateSeedCommand
                    )

        PassedSlowThreshold ->
            let
                updatedStatus =
                    case model.status of
                        Updating ->
                            UpdatingSlowly

                        _ ->
                            model.status
            in
            ( { model | status = updatedStatus }, Cmd.none )


        PrepareMediaRequestDialog ct mt ->
            ( model, showDialog ( Just ( prepareUrlConfig model ct mt ) ) )

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

-- Handled in Project model
        ShowDialog _ ->
            (model, Cmd.none)

-- Handled in Project model
        UpdateSeeds _ ->
            (model, Cmd.none)

        UpdateVisibility updatedImages updatedSounds updatedVideos ->
            (
                { model
                | images = updatedImages
                , sounds = updatedSounds
                , videos = updatedVideos
                }
                , Cmd.none
            )

-- VIEW

viewTinyMCEEditor : Model -> (String, Element Msg)
viewTinyMCEEditor { initParams, slideId, slideText } =
    let
        node = Html.node "tinymce-editor"
           [ attribute "api-key" "0cqgum03zgzxoroz2zyrwedektxs7dmscrvnfolhyl1f6d57"
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

generateUpdateVisibilityMessage : ComponentType -> Model -> Maybe Msg
generateUpdateVisibilityMessage ct { images, sounds, videos } =
    case ct of
        Image ->
            case (images, sounds, videos) of
                ( HiddenImages is, VisibleSounds ss, _ ) ->
                    Just (UpdateVisibility (VisibleImages is) (HiddenSounds ss) videos)
                ( HiddenImages is, _, VisibleVideos vs ) ->
                    Just (UpdateVisibility (VisibleImages is) sounds (HiddenVideos vs))
                ( HiddenImages is, HiddenSounds _, HiddenVideos _ ) ->
                    Just (UpdateVisibility (VisibleImages is) sounds videos)
                ( VisibleImages is, _, _ ) ->
                    Just (UpdateVisibility (HiddenImages is) sounds videos)
        Sound ->
            case (images, sounds, videos) of
                ( VisibleImages is, HiddenSounds ss, _ ) ->
                    Just (UpdateVisibility (HiddenImages is) (VisibleSounds ss) videos)
                ( _, HiddenSounds ss, VisibleVideos vs ) ->
                    Just (UpdateVisibility images (VisibleSounds ss) (HiddenVideos vs))
                ( HiddenImages _, HiddenSounds ss, HiddenVideos _ ) ->
                    Just (UpdateVisibility images (VisibleSounds ss) videos)
                ( _, VisibleSounds ss, _ ) ->
                    Just (UpdateVisibility images (HiddenSounds ss) videos)
        Video ->
            case (images, sounds, videos) of
                ( VisibleImages is, _, HiddenVideos vs ) ->
                    Just (UpdateVisibility (HiddenImages is) sounds (VisibleVideos vs))
                ( _, VisibleSounds ss, HiddenVideos vs ) ->
                    Just (UpdateVisibility images (HiddenSounds ss) (VisibleVideos vs))
                ( HiddenImages _, HiddenSounds _, HiddenVideos vs ) ->
                    Just (UpdateVisibility images sounds (VisibleVideos vs))
                ( _, _, VisibleVideos vs ) ->
                    Just (UpdateVisibility images sounds (HiddenVideos vs))

viewSlideMediaElements : ComponentType -> Model -> Element Msg
viewSlideMediaElements ct model =
    Input.button
        buttonAttributes
        { onPress = generateUpdateVisibilityMessage ct model
        , label = Element.text ("Manage Slide " ++ (toCapitalizedPluralText ct))
        }

viewSlideComponentButtons : Model -> Element Msg
viewSlideComponentButtons model =
    row
        [ centerX
        , spacing 10
        ]
        [ viewSlideMediaElements Image model
        , viewSlideMediaElements Sound model
        , viewSlideMediaElements Video model
        ]

toComponentDescription : ComponentType -> String
toComponentDescription t =
    "Description of " ++ (toText t) ++ " to stage:"

viewComponentDescription : Model -> ComponentType -> Element Msg
viewComponentDescription { componentDescription } t =
    Input.text
        [ ]
        { onChange = ComponentDescriptionInput
        , text = componentDescription
        , placeholder = Just (Input.placeholder [ ] (Element.text "Supply a description here."))
        , label = Input.labelLeft [ ] (Element.text (toComponentDescription t))
        }

toSelectComponentFileButtonText : ComponentType -> Element Msg
toSelectComponentFileButtonText t =
    Element.text ("Select " ++ (toText t) ++ " file to stage")

toSelectComponentUrlButtonText : ComponentType -> Element Msg
toSelectComponentUrlButtonText t =
    Element.text ("Specify " ++ (toText t) ++ " URL to stage")

viewLoadComponents : Model -> ComponentType -> Element Msg
viewLoadComponents { componentDescription } componentType =
    if (String.isEmpty componentDescription) then
        Element.none
    else
        row
            [ centerX
            , spacing 10
            ]
            [
                Input.button
                    buttonAttributes
                    { onPress = Just (MediaRequested componentType File)
                    , label = toSelectComponentFileButtonText componentType
                    }
                , Input.button
                    buttonAttributes
                    { onPress = Just (PrepareMediaRequestDialog componentType Url)
                    , label = toSelectComponentUrlButtonText componentType
                    }
            ]

viewStagedComponentsHeader : ComponentType -> Element Msg
viewStagedComponentsHeader ct =
    el
        [ Font.size 24
        , centerX
        ]
        ( Element.text ("Staged " ++ (toCapitalizedText ct) ++ " Components") )

viewNoStagedComponents : ComponentType -> Element Msg
viewNoStagedComponents componentType =
    paragraph
        [ centerX ]
        [ Element.text ("No staged " ++ (toText componentType) ++ " components") ]

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
        \{ id } ->
            let
                url = Builder.relative
                    [ flags.candorUrl
                    , ( toBaseUrl componentType )
                    , LanguageHelpers.contentCodeStringFromLanguage knownLanguage
                    , LanguageHelpers.contentCodeStringFromLanguage learningLanguage
                    , projectName
                    , id
                    ]
                    [ ]
                node = Html.node "clipboard-copy"
                    [ attribute "value" url
                    , class "w3-button w3-black w3-round"
                    , title id
                    ]
                    [ text "Copy URL to clipboard" ]
            in
            html node
    }

toDeleteComponentButtonText : ComponentType -> Element Msg
toDeleteComponentButtonText t =
    Element.text ("Delete " ++ (toText t) )

prepareDeleteComponentButton : Text -> ComponentType -> Column SlideComponent Msg
prepareDeleteComponentButton slideText componentType =
    { header = Element.none
    , width = fill
    , view =
        \{ id } ->
            if ( String.contains id ( textToString slideText ) ) then
                Element.none
            else
                Input.button
                    buttonAttributes
                    { onPress = Just (DeleteMedia componentType id)
                    , label = toDeleteComponentButtonText componentType
                    }
    }

addDeleteComponentButtonIfNecessary : Model -> ComponentType -> List (Column SlideComponent Msg)
addDeleteComponentButtonIfNecessary { initParams, isSlideTextDirty, slideText } componentType =
    if ( isSlideTextDirty ) then
        [ ]
    else
        [ prepareDeleteComponentButton slideText componentType ]

viewComponentsTable : Model -> ComponentType -> List SlideComponent -> Element Msg
viewComponentsTable ( { initParams } as model ) componentType components =
    table
        [ spacing 10 ]
        { data = components
        , columns =
            List.append
                [ prepareDescription
                , prepareCopyUrlButton initParams componentType
                ]
                (addDeleteComponentButtonIfNecessary model componentType)
        }

viewComponents : Model -> ComponentType -> List SlideComponent -> Element Msg
viewComponents model componentType components =
    let
        nonDeletedComponents = List.filter (\{markedForDeletion} -> not markedForDeletion) components
        t =
            if (List.isEmpty components) then
                viewNoStagedComponents componentType
            else
                viewComponentsTable model componentType nonDeletedComponents
    in
    column
        [ centerX
        , spacing 10
        ]
        [ viewComponentDescription model componentType
        , viewLoadComponents model componentType
        , viewStagedComponentsHeader componentType
        , t
        ]

viewSlideComponents : Model -> Element Msg
viewSlideComponents ( { images, sounds, videos } as model ) =
    case ( images, sounds, videos ) of
        ( VisibleImages vis, HiddenSounds _, HiddenVideos _ ) ->
            viewComponents model Image vis

        ( HiddenImages _, VisibleSounds vss, HiddenVideos _ ) ->
            viewComponents model Sound vss

        ( HiddenImages _, HiddenSounds _, VisibleVideos vvs ) ->
            viewComponents model Video vvs

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
view ( { initParams, slideId, status } as model ) =
    let
        spinner =
            case status of
                UpdatingSlowly ->
                    Loading.iconElement initParams.flags.loadingPath

                _ ->
                    Element.none
    in
    Keyed.column
        [ Font.size 14
        , centerX
        , spacing 10
        , padding 10
        ]
        [ ("candor-spinner-" ++ slideId, spinner)
        , viewTinyMCEEditor model
        , viewSlideComponentsArea model
        , viewQuestionsArea model
        ]

-- Retreive media URL dialog

headerUrlText : ComponentType -> Element Msg
headerUrlText ct =
    Element.text ( "Supply " ++ ( toText ct ) ++ " URL to stage" )

toLoadUrlComponentLabelText : ComponentType -> String
toLoadUrlComponentLabelText t =
    "URL of " ++ (toText t) ++ " to stage:"

viewLoadComponentFromUrl : Model -> ComponentType -> Element Msg
viewLoadComponentFromUrl { componentUrl } componentType =
    Input.text
        [ ]
        { onChange = ComponentUrlInput componentType
        , text = componentUrl
        , placeholder = Just (Input.placeholder [ ] (Element.text "Supply a valid URL here."))
        , label = Input.labelHidden ( toLoadUrlComponentLabelText componentType )
        }

toLoadComponentButtonText : ComponentType -> Element Msg
toLoadComponentButtonText t =
    Element.text ("Stage " ++ (toText t) ++ " on server")

viewLoadComponentFooter : Msg -> Element Msg -> Msg -> Element Msg
viewLoadComponentFooter requestedMsg requestedText cancelledMsg =
    row
        [ spacing 10
        , alignRight
        ]
        [ Input.button
            buttonAttributes
            { onPress = Just requestedMsg
            , label = requestedText
            }
        , Input.button
            buttonAttributes
            { onPress = Just cancelledMsg
            , label = Element.text "Cancel"
            }
        ]

viewLoadComponentFromUrlFooter : ComponentType -> Element Msg
viewLoadComponentFromUrlFooter ct =
    viewLoadComponentFooter (MediaRequested ct Url) (toLoadComponentButtonText ct) Cancelled

prepareUrlConfig : Model -> ComponentType -> MediaRequest -> Config Msg
prepareUrlConfig model ct _ =
    { closeMessage = Nothing
    , maskAttributes = [ ]
    , containerAttributes =
        [ Background.color white
        , Border.rounded 5
        , centerX
        , centerY
        , padding 10
        , spacing 20
        , width (px 400)
        ]
    , headerAttributes =
        [ Font.size 24
        , Font.color red
        , padding 5
        ]
    , bodyAttributes =
        [ Background.color lightGrey
        , padding 20
        ]
    , footerAttributes =
        [ padding 5 ]
    , header = Just ( headerUrlText ct )
    , body = Just ( viewLoadComponentFromUrl model ct )
    , footer = Just ( viewLoadComponentFromUrlFooter ct )
    }
