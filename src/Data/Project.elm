module Data.Project exposing (encodeProject, establishIndexes, establishSlideUUIDs, projectDecoder, init, initEmptyProject, initNewProject, Model, Msg(..), storeSlideContents, update, view)

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
import Random
import Task exposing (Task)
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
    , seeds : Seeds
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

initialSeeds : Seeds
initialSeeds =
    (Seeds
        (Random.initialSeed 12345)
        (Random.initialSeed 23456)
        (Random.initialSeed 34567)
        (Random.initialSeed 45678)
    )

projectDecoder : InitParams -> Decoder Model
projectDecoder ( { flags } as initParams ) =
    succeed Model
        |> hardcoded initParams
        |> hardcoded 0
        |> required "slides" (slidesDecoder initParams)
        |> hardcoded initialSeeds

encodeProject : Model -> Encode.Value
encodeProject { slides } =
    slides |> Dict.values |> Encode.list Slide.encodeSlide

initProject : InitParams -> SlideDict -> Seeds -> Model
initProject initParams slides seeds =
    { initParams = initParams
    , slideIndex = 0
    , slides = slides
    , seeds = seeds
    }

initEmptyProject : InitParams -> Model
initEmptyProject initParams =
    initProject initParams Dict.empty initialSeeds

initNewProject : InitParams -> Model
initNewProject initParams  =
    let
        seeds = initialSeeds
        (slides, updatedSeeds) = createNewSlide initParams 0 seeds
    in
    initProject initParams slides updatedSeeds

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

establishSlideUUIDs : Model -> Model
establishSlideUUIDs ( { seeds, slides } as model ) =
    let
        (updatedSlides, updatedSeeds) = Dict.foldl establishSlideUUID (Dict.empty, seeds) slides
    in
    { model | slides = updatedSlides, seeds = updatedSeeds }

-- UPDATE

type Msg =
    DeleteSlide
    | DisplaySlide Int
    | InsertSlide ProjectHelpers.Direction
    | MakeDirty
    | Move ProjectHelpers.Direction
    | ShowDialog (Config Msg)
    | SlideMsg Slide.Msg
    | UpdateCurrentSlideContents Msg

createNewSlide : InitParams -> Int -> Seeds -> (Dict Int Slide.Model, Seeds)
createNewSlide { flags, knownLanguage, learningLanguage, projectName } slideIndex seeds =
    let
        (uuid, updatedSeeds) = UUID.step seeds
        slideInitParams =
            { flags = flags
            , knownLanguage = knownLanguage
            , learningLanguage = learningLanguage
            , projectName = projectName
            }
        newSlide = Slide.init slideInitParams (UUID.toString uuid) slideIndex
    in
    ( Dict.singleton slideIndex newSlide, updatedSeeds )

insertSlideAtSlicePoint : Int -> Model -> Model
insertSlideAtSlicePoint slicePoint ( { initParams, slides, seeds } as model ) =
    let
        (beforeSlides, afterSlides) = Dict.partition (\i _ -> (i < slicePoint)) slides
        (newSlides, updatedSeeds) = createNewSlide initParams slicePoint seeds
        updatedSlides =
            afterSlides
                |> Dict.Extra.mapKeys (\k -> k + 1)
                |> updateSlideIndexes
                |> Dict.union newSlides
                |> Dict.union beforeSlides
    in
    { model | slides = updatedSlides, slideIndex = slicePoint, seeds = updatedSeeds }

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

showDialog : Config Msg -> Cmd Msg
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
            ( insertSlideAtSlicePoint 0 model, makeProjectDirty )

        InsertSlide ProjectHelpers.Up ->
            ( insertSlideAtSlicePoint slideIndex model, makeProjectDirty )

        InsertSlide ProjectHelpers.Down ->
            ( insertSlideAtSlicePoint (slideIndex + 1) model, makeProjectDirty )

        InsertSlide ProjectHelpers.Bottom ->
            ( insertSlideAtSlicePoint (Dict.size slides) model, makeProjectDirty )

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
                Slide.MakeDirty ->
                    ( model, makeProjectDirty )

                Slide.ShowDialog config ->
                    ( model, showDialog (Dialog.map SlideMsg config) )

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
