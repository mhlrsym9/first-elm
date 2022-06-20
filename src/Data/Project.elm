module Data.Project exposing (encodeProject, establishIndexes, establishSlideUUIDs, init, initEmptyProject, initNewProject, InitParams, Model, Msg(..), processDirtySlideTextMessage, projectDecoder, storeSlideContents, update, updateProject, updateSeeds, view)

import Data.ProjectHelpers as ProjectHelpers
import Data.Slide as Slide
import Dialog exposing (Config)
import Dict exposing (Dict)
import Dict.Extra
import Element exposing (centerX, column, Element, padding, row, spacing)
import Element.Font as Font
import Element.Input as Input
import Flags exposing (Flags)
import Json.Decode exposing (Decoder, succeed)
import Json.Decode.Extra exposing (indexedList)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode
import LanguageHelpers
import MessageHelpers exposing (sendCommandMessage)
import UIHelpers exposing (buttonAttributes)
import UUID exposing (Seeds)

type alias SlideDict =
    Dict Int Slide.Model

type alias InitParams =
    { flags : Flags.Model
    , knownLanguage : LanguageHelpers.Language
    , learningLanguage : LanguageHelpers.Language
    , projectName : String
    }

type alias Model =
    { initParams : InitParams
    , slideIndex : Int
    , slides : SlideDict
    }

slideDecoder : InitParams -> Int -> Decoder (Int, Slide.Model)
slideDecoder { flags, knownLanguage, learningLanguage, projectName } index =
    let
        slideInitParams =
            { flags = flags
            , knownLanguage = knownLanguage
            , learningLanguage = learningLanguage
            , projectName = projectName
            }
    in
    Slide.slideDecoder slideInitParams
        |> Json.Decode.map (\s -> (index, s))

slidesDecoder : InitParams -> Decoder (Dict Int Slide.Model)
slidesDecoder initParams =
    (indexedList (slideDecoder initParams))
        |> Json.Decode.map (\l -> Dict.fromList l)

projectDecoder : InitParams -> Decoder Model
projectDecoder ( { flags } as initParams ) =
    succeed Model
        |> hardcoded initParams
        |> hardcoded 0
        |> required "slides" (slidesDecoder initParams)

encodeProject : Model -> Encode.Value
encodeProject { slides } =
    slides |> Dict.values |> Encode.list Slide.encodeSlide

initEmptyProject : InitParams -> Model
initEmptyProject initParams =
    { initParams = initParams
    , slideIndex = 0
    , slides = Dict.empty
    }

initNewProject : InitParams -> ( Model, Cmd Msg )
initNewProject ( { flags } as initParams ) =
    let
        (slides, updatedSeeds) = createNewSlide (initEmptyProject initParams) 0
        model =
            { initParams = initParams
            , slideIndex = 0
            , slides = slides
            }
    in
    ( model, sendCommandMessage (UpdateSeeds updatedSeeds) )

init : InitParams -> Model
init initParams =
    initEmptyProject initParams

updateSlideIndexes : Dict Int Slide.Model -> Dict Int Slide.Model
updateSlideIndexes slides =
    Dict.map Slide.updateSlideIndex slides

establishIndexes : Model -> Model
establishIndexes ( { slides } as model ) =
    { model | slides = Dict.map (\i s -> Slide.establishIndexes i s) slides }

establishSlideUUID : Int -> Slide.Model -> (Dict Int Slide.Model, Seeds) -> (Dict Int Slide.Model, Seeds)
establishSlideUUID index ( { initParams } as slideModel ) ( dict, seeds ) =
    let
        (uuid, updatedSeeds) = UUID.step seeds
        updatedSlide = { slideModel | slideId = UUID.toString uuid }
    in
    ( Dict.insert index updatedSlide dict, updatedSeeds )

establishSlideUUIDs : Model -> ( Model, Cmd Msg )
establishSlideUUIDs ( { initParams, slides } as model ) =
    let
        (updatedSlides, updatedSeeds) =
            Dict.foldl establishSlideUUID (Dict.empty, initParams.flags.seeds) slides
    in
    ( { model | slides = updatedSlides }
    , sendCommandMessage (UpdateSeeds updatedSeeds) )

updateSlide : Int -> Slide.Model -> (Dict Int Slide.Model, Bool) -> (Dict Int Slide.Model, Bool)
updateSlide index slideModel (dict, dirty) =
    let
        (updatedSlide, updatedDirty) = Slide.updateSlide slideModel
        finalDirty =
            case dirty of
                True ->
                    True
                False ->
                    updatedDirty
    in
    ( Dict.insert index updatedSlide dict, finalDirty )

updateProject : Model -> (Maybe Model, Bool)
updateProject ( { slides } as model ) =
    let
        (updatedSlides, dirty) = Dict.foldl updateSlide (Dict.empty, False) slides
    in
    ( Just { model | slides = updatedSlides }, dirty )

updateSlideSeeds : SlideDict -> Seeds -> SlideDict
updateSlideSeeds slides updatedSeeds =
    Dict.map (\_ slide -> Slide.updateSeeds slide updatedSeeds) slides

updateSeeds : Model -> Seeds -> Model
updateSeeds ( { initParams, slides } as model ) updatedSeeds =
    let
        updatedFlags = Flags.updateSeeds initParams.flags updatedSeeds
        updatedInitParams = { initParams | flags = updatedFlags }
        updatedSlides = updateSlideSeeds slides updatedSeeds
    in
    { model | initParams = updatedInitParams, slides = updatedSlides }

processDirtySlideTextMessage : Model -> Bool -> Model
processDirtySlideTextMessage ( { slideIndex, slides } as model ) isDirty =
    let
        currentSlide = Dict.get slideIndex slides
    in
    case currentSlide of
        Nothing ->
            model

        Just slide ->
            { model | slides = Dict.insert slideIndex ( Slide.processDirtySlideTextMessage slide isDirty ) slides }

-- UPDATE

type Msg =
    DeleteSlide
    | DisplaySlide Int
    | InsertSlide ProjectHelpers.Direction
    | MakeDirty
    | Move ProjectHelpers.Direction
    | ShowDialog ( Maybe (Config Msg) )
    | SlideMsg Slide.Msg
    | UpdateCurrentSlideContents Msg
    | UpdateSeeds Seeds

createNewSlide : Model -> Int -> (Dict Int Slide.Model, Seeds)
createNewSlide { initParams } slideIndex =
    let
        (uuid, updatedSeeds) = UUID.step initParams.flags.seeds
        slideInitParams =
            { flags = initParams.flags
            , knownLanguage = initParams.knownLanguage
            , learningLanguage = initParams.learningLanguage
            , projectName = initParams.projectName
            }
        newSlide = Slide.init slideInitParams (UUID.toString uuid) slideIndex
    in
    ( Dict.singleton slideIndex newSlide, updatedSeeds )

insertSlideAtSlicePoint : Model -> Int -> ( Model, Cmd Msg )
insertSlideAtSlicePoint ( { initParams, slides } as model ) slicePoint =
    let
        (beforeSlides, afterSlides) = Dict.partition (\i _ -> (i < slicePoint)) slides
        (newSlides, updatedSeeds) = createNewSlide model slicePoint
        updatedSlides =
            afterSlides
                |> Dict.Extra.mapKeys (\k -> k + 1)
                |> updateSlideIndexes
                |> Dict.union newSlides
                |> Dict.union beforeSlides
    in
    ( { model | slides = updatedSlides, slideIndex = slicePoint }
    , sendCommandMessage (UpdateSeeds updatedSeeds)
    )

storeSlideContents : String -> Model -> Model
storeSlideContents slideContents ( { slideIndex, slides } as projectModel ) =
    let
        maybeSlideModel = Dict.get slideIndex slides
    in
    case maybeSlideModel of
        Just slideModel ->
            let
                updatedSlideModel =
                    Slide.storeSlideContents slideContents slideModel
            in
            { projectModel | slides = Dict.insert slideIndex updatedSlideModel slides }

        Nothing ->
            projectModel

makeProjectDirty : Cmd Msg
makeProjectDirty =
    sendCommandMessage MakeDirty

showDialog : Maybe (Config Msg) -> Cmd Msg
showDialog config =
    sendCommandMessage (ShowDialog config)

update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { slideIndex, slides } as model ) =
    case msg of
        DeleteSlide ->
            let
                updatedSlideIndex =
                    if (0 == slideIndex) then
                        0
                    else
                        slideIndex - 1
            in
            (
                { model | slides = ProjectHelpers.deleteEntry slideIndex updateSlideIndexes slides
                        , slideIndex = updatedSlideIndex
                }
                , makeProjectDirty
            )

        DisplaySlide updatedSlideIndex ->
            ( { model | slideIndex = updatedSlideIndex }, Cmd.none )

        InsertSlide ProjectHelpers.Top ->
            let
                (updatedModel, command) = insertSlideAtSlicePoint model 0
            in
            ( updatedModel
            , Cmd.batch [ makeProjectDirty, command ]
            )

        InsertSlide ProjectHelpers.Up ->
            let
                (updatedModel, command) = insertSlideAtSlicePoint model slideIndex
            in
            ( updatedModel
            , Cmd.batch [ makeProjectDirty, command ]
            )

        InsertSlide ProjectHelpers.Down ->
            let
                (updatedModel, command) = insertSlideAtSlicePoint model (slideIndex + 1)
            in
            ( updatedModel
            , Cmd.batch [ makeProjectDirty, command ]
            )

        InsertSlide ProjectHelpers.Bottom ->
            let
                (updatedModel, command) = insertSlideAtSlicePoint model (Dict.size slides)
            in
            ( updatedModel
            , Cmd.batch [ makeProjectDirty, command ]
            )

-- Handled by Edit module
        MakeDirty ->
            ( model, Cmd.none )

        Move ProjectHelpers.Top ->
            let
                updatedSlides = ProjectHelpers.moveEntry
                    slideIndex ProjectHelpers.Increment 0 updateSlideIndexes slides
            in
            (
                { model
                    | slides = updatedSlides
                    , slideIndex = 0
                }
                , makeProjectDirty
            )

        Move ProjectHelpers.Up ->
            let
                updatedSlides = ProjectHelpers.flipAdjacentEntries
                    slideIndex ProjectHelpers.Decrement updateSlideIndexes slides
            in
            (
                { model
                    | slides = updatedSlides
                    , slideIndex = slideIndex - 1
                }
                , makeProjectDirty
            )

        Move ProjectHelpers.Down ->
            let
                updatedSlides = ProjectHelpers.flipAdjacentEntries
                    slideIndex ProjectHelpers.Increment updateSlideIndexes slides
            in
            (
                { model
                    | slides = updatedSlides
                    , slideIndex = slideIndex + 1
                }
                , makeProjectDirty
            )

        Move ProjectHelpers.Bottom ->
            let
                finalIndex = (Dict.size slides) - 1
                updatedSlides = ProjectHelpers.moveEntry
                    slideIndex ProjectHelpers.Decrement finalIndex updateSlideIndexes slides
            in
            (
                { model
                    | slides = updatedSlides
                    , slideIndex = finalIndex
                }
                , makeProjectDirty
            )

-- Handled by Edit module
        ShowDialog _ ->
            (model, Cmd.none)

        SlideMsg slideMsg ->
            case slideMsg of
                Slide.MakeProjectDirty ->
                    ( model, makeProjectDirty )

                Slide.ShowDialog config ->
                    case config of
                        Just c ->
                            ( model, showDialog (Just (Dialog.map SlideMsg c) ) )

                        Nothing ->
                            ( model, showDialog Nothing )

                Slide.UpdateSeeds updatedSeeds ->
                    ( model, sendCommandMessage (UpdateSeeds updatedSeeds) )

                _ ->
                    let
                        maybeSlide = Dict.get slideIndex slides
                    in
                    case maybeSlide of
                        Just slide ->
                            let
                                ( updatedSlide, commands ) =
                                    Slide.update slideMsg slide
                            in
                            (
                                { model | slides = Dict.insert slideIndex updatedSlide slides }
                                , Cmd.map SlideMsg commands
                            )

                        Nothing ->
                            ( model, Cmd.none )

        UpdateCurrentSlideContents _ ->
            ( model, Cmd.none )

        -- Handled by parent
        UpdateSeeds _ ->
            ( model, Cmd.none )
-- VIEW

viewFirstSlideButton : Model -> Element Msg
viewFirstSlideButton { slideIndex, slides } =
    let
        numberSlides = Dict.size slides
    in
    if ((0 == slideIndex) || (1 == numberSlides)) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = Just ( UpdateCurrentSlideContents ( DisplaySlide 0 ) )
            , label = Element.text "<<- First"
            }

viewPreviousSlideButton : Model -> Element Msg
viewPreviousSlideButton { slideIndex, slides } =
    let
        numberSlides = Dict.size slides
    in
    if ((0 == slideIndex) || (1 == numberSlides)) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = Just ( UpdateCurrentSlideContents ( DisplaySlide (slideIndex - 1) ) )
            , label = Element.text "<- Previous"
            }

viewNextSlideButton : Model -> Element Msg
viewNextSlideButton { slideIndex, slides } =
    let
        numberSlides = Dict.size slides
    in
    if (slideIndex == (numberSlides - 1)) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = Just ( UpdateCurrentSlideContents ( DisplaySlide (slideIndex + 1) ) )
            , label = Element.text "Next ->"
            }

viewLastSlideButton : Model -> Element Msg
viewLastSlideButton { slideIndex, slides } =
    let
        numberSlides = Dict.size slides
    in
    if (slideIndex == (numberSlides - 1)) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = Just (UpdateCurrentSlideContents ( DisplaySlide (numberSlides - 1) ) )
            , label = Element.text "Last ->>"
            }

viewSlideInfoRow : Model -> Element Msg
viewSlideInfoRow ( { slideIndex, slides } as model ) =
    let
        numberSlides = Dict.size slides
    in
    row
        [ centerX
        , spacing 10
        ]
        [ viewFirstSlideButton model
        , viewPreviousSlideButton model
        , Element.text
            ( "Slide "
            ++ ( String.fromInt ( slideIndex + 1 ) )
            ++ " of "
            ++ ( String.fromInt numberSlides )
            )
        , viewNextSlideButton model
        , viewLastSlideButton model
        ]

viewInsertAtTopButton : Element Msg
viewInsertAtTopButton =
    Input.button
        buttonAttributes
        { onPress = Just ( UpdateCurrentSlideContents ( InsertSlide ProjectHelpers.Top ) )
        , label = Element.text "<<- Add new slide before first slide"
        }

viewInsertBeforeSlideButton : Element Msg
viewInsertBeforeSlideButton =
    Input.button
        buttonAttributes
        { onPress = Just ( UpdateCurrentSlideContents ( InsertSlide ProjectHelpers.Up ) )
        , label = Element.text "<- Add new slide before this slide"
        }

viewInsertAfterSlideButton : Element Msg
viewInsertAfterSlideButton =
    Input.button
        buttonAttributes
        { onPress = Just ( UpdateCurrentSlideContents ( InsertSlide ProjectHelpers.Down ) )
        , label = Element.text "Add new slide after this slide ->"
        }

viewInsertAtBottomButton : Element Msg
viewInsertAtBottomButton =
    Input.button
        buttonAttributes
        { onPress = Just ( UpdateCurrentSlideContents ( InsertSlide ProjectHelpers.Bottom ) )
        , label = Element.text "Add new slide after last slide ->>"
        }

viewInsertSlideActionRow : Element Msg
viewInsertSlideActionRow =
    row
         [ spacing 10
         , centerX
         ]
         [ viewInsertAtTopButton
         , viewInsertBeforeSlideButton
         , viewInsertAfterSlideButton
         , viewInsertAtBottomButton
         ]

viewMoveSlideToTopButton : Model -> Element Msg
viewMoveSlideToTopButton { slideIndex } =
    if (0 == slideIndex) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = Just ( UpdateCurrentSlideContents ( Move ProjectHelpers.Top ) )
            , label = Element.text "Move Slide to Top"
            }

viewMoveSlideUpButton : Model -> Element Msg
viewMoveSlideUpButton { slideIndex } =
    if (0 == slideIndex) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = Just ( UpdateCurrentSlideContents ( Move ProjectHelpers.Up ) )
            , label = Element.text "Move Slide Up"
            }

viewMoveSlideDownButton : Model -> Element Msg
viewMoveSlideDownButton { slideIndex, slides }  =
    let
        numberSlides = Dict.size slides
    in
    if ( slideIndex == (numberSlides - 1) ) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = Just ( UpdateCurrentSlideContents ( Move ProjectHelpers.Down ) )
            , label = Element.text "Move Slide Down"
            }

viewMoveSlideToBottomButton : Model -> Element Msg
viewMoveSlideToBottomButton { slideIndex, slides } =
    let
        numberSlides = Dict.size slides
    in
    if ( slideIndex == (numberSlides - 1) ) then
        Element.none
    else
        Input.button
            buttonAttributes
            { onPress = Just ( UpdateCurrentSlideContents ( Move ProjectHelpers.Bottom ) )
            , label = Element.text "Move Slide to Bottom"
            }

viewMoveSlideActionRow : Model -> Element Msg
viewMoveSlideActionRow model =
    row
        [ spacing 10
        , centerX
        ]
        [ viewMoveSlideToTopButton model
        , viewMoveSlideUpButton model
        , viewMoveSlideDownButton model
        , viewMoveSlideToBottomButton model
        ]

viewDeleteSlideActionRow : Model -> Element Msg
viewDeleteSlideActionRow { slides } =
    if ( 1 == Dict.size slides ) then
        Element.none
    else
    Input.button
        (centerX :: buttonAttributes)
        { onPress = Just DeleteSlide
        , label = Element.text "Delete This Slide"
        }

viewSlide : Maybe Slide.Model -> Element Msg
viewSlide maybeSlide =
    case maybeSlide of
        Just slide ->
            Slide.view slide
                |> Element.map SlideMsg

        Nothing ->
            Element.none

viewCurrentSlide : Model -> Element Msg
viewCurrentSlide ( { slideIndex, slides } as model ) =
    column
        [ Font.size 14
        , centerX
        , padding 10
        , spacing 10
        ]
        [ viewSlideInfoRow model
        , viewInsertSlideActionRow
        , viewMoveSlideActionRow model
        , viewDeleteSlideActionRow model
        , viewSlide (Dict.get slideIndex slides)
        ]

view : Model -> Element Msg
view model =
    viewCurrentSlide model
