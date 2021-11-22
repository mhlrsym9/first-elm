module Data.Project exposing (encodeProject, establishIndexes, establishSlideUUIDs, projectDecoder, init, initEmptyProject, initNewProject, Model, Msg(..), storeSlideContents, update, view)

import Data.ProjectHelpers as ProjectHelpers
import Data.Slide as Slide
import Dict exposing (Dict)
import Dict.Extra
import Flags exposing (Flags)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, succeed)
import Json.Decode.Extra exposing (indexedList)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode
import LanguageHelpers
import Random
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
    | Move ProjectHelpers.Direction
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
                , Cmd.none
            )

        DisplaySlide updatedSlideIndex ->
            ( { model | slideIndex = updatedSlideIndex }, Cmd.none )

        InsertSlide ProjectHelpers.Top ->
            ( insertSlideAtSlicePoint 0 model, Cmd.none )

        InsertSlide ProjectHelpers.Up ->
            ( insertSlideAtSlicePoint slideIndex model, Cmd.none )

        InsertSlide ProjectHelpers.Down ->
            ( insertSlideAtSlicePoint (slideIndex + 1) model, Cmd.none )

        InsertSlide ProjectHelpers.Bottom ->
            ( insertSlideAtSlicePoint (Dict.size slides) model, Cmd.none )

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
                , Cmd.none
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
                , Cmd.none
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
                , Cmd.none
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
                , Cmd.none
            )

        SlideMsg slideMsg ->
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

viewFirstSlideButton : Model -> Html Msg
viewFirstSlideButton { slideIndex, slides } =
    let
        numberSlides = Dict.size slides
    in
    button
        [ class "slide-button"
        , disabled ( (slideIndex == 0) || (numberSlides == 1) )
        , onClick ( UpdateCurrentSlideContents ( DisplaySlide 0 ) )
        ]
        [ text "<<- First" ]

viewPreviousSlideButton : Model -> Html Msg
viewPreviousSlideButton { slideIndex, slides } =
    let
        numberSlides = Dict.size slides
    in
    button
        [ class "slide-button"
        , disabled ( (slideIndex == 0) || (numberSlides == 1) )
        , onClick ( UpdateCurrentSlideContents ( DisplaySlide (slideIndex - 1) ) )
        ]
        [ text "<- Previous"]

viewNextSlideButton : Model -> Html Msg
viewNextSlideButton { slideIndex, slides } =
    let
        numberSlides = Dict.size slides
    in
    button
        [ class "slide-button"
        , disabled ( slideIndex == (numberSlides - 1) )
        , onClick ( UpdateCurrentSlideContents ( DisplaySlide ( slideIndex + 1 ) ) )
        ]
        [ text "Next ->" ]

viewLastSlideButton : Model -> Html Msg
viewLastSlideButton { slideIndex, slides } =
    let
        numberSlides = Dict.size slides
    in
    button
        [ class "slide-button"
        , disabled ( slideIndex == (numberSlides - 1) )
        , onClick (UpdateCurrentSlideContents ( DisplaySlide (numberSlides - 1) ) )
        ]
        [ text "Last ->>" ]

viewSlideInfoRow : Model -> Html Msg
viewSlideInfoRow ( { slideIndex, slides } as model ) =
    let
        numberSlides = Dict.size slides
    in
    div
        [ class "edit-page-slide-info-row" ]
        [ viewFirstSlideButton model
        , viewPreviousSlideButton model
        , text
            ( "Slide "
            ++ ( String.fromInt ( slideIndex + 1 ) )
            ++ " of "
            ++ ( String.fromInt numberSlides )
            )
        , viewNextSlideButton model
        , viewLastSlideButton model
        ]

viewInsertAtTopButton : Html Msg
viewInsertAtTopButton =
    button
        [ class "slide-button"
        , onClick ( UpdateCurrentSlideContents ( InsertSlide ProjectHelpers.Top ) )
        ]
        [ text "<<- Add new slide before first slide"]

viewInsertBeforeSlideButton : Html Msg
viewInsertBeforeSlideButton =
    button
        [ class "slide-button"
        , onClick ( UpdateCurrentSlideContents ( InsertSlide ProjectHelpers.Up ) )
        ]
        [ text "<- Add new slide before this slide"]

viewInsertAfterSlideButton : Html Msg
viewInsertAfterSlideButton =
    button
        [ class "slide-button"
        , onClick (UpdateCurrentSlideContents ( InsertSlide ProjectHelpers.Down ) )
        ]
        [ text "Add new slide after this slide ->"]

viewInsertAtBottomButton : Html Msg
viewInsertAtBottomButton =
    button
        [ class "slide-button"
        , onClick ( UpdateCurrentSlideContents ( InsertSlide ProjectHelpers.Bottom ) )
        ]
        [ text "Add new slide after last slide ->>"]

viewInsertSlideActionRow : Html Msg
viewInsertSlideActionRow =
    div
         [ class "edit-page-insert-slide-action-row" ]
         [ viewInsertAtTopButton
         , viewInsertBeforeSlideButton
         , viewInsertAfterSlideButton
         , viewInsertAtBottomButton
         ]

viewMoveSlideToTopButton : Model -> Html Msg
viewMoveSlideToTopButton { slideIndex } =
    button
        [ onClick ( UpdateCurrentSlideContents ( Move ProjectHelpers.Top ) )
        , disabled (0 == slideIndex)
        ]
        [ text "Move Slide to Top" ]

viewMoveSlideUpButton : Model -> Html Msg
viewMoveSlideUpButton { slideIndex } =
    button
        [ onClick ( UpdateCurrentSlideContents ( Move ProjectHelpers.Up ) )
        , disabled (0 == slideIndex)
        ]
        [ text "Move Slide Up" ]

viewMoveSlideDownButton : Model -> Html Msg
viewMoveSlideDownButton { slideIndex, slides }  =
    let
        numberSlides = Dict.size slides
    in
    button
        [ onClick ( UpdateCurrentSlideContents ( Move ProjectHelpers.Down ) )
        , disabled ( slideIndex == (numberSlides - 1) )
        ]
        [ text "Move Slide Down" ]

viewMoveSlideToBottomButton : Model -> Html Msg
viewMoveSlideToBottomButton { slideIndex, slides } =
    let
        numberSlides = Dict.size slides
    in
    button
        [ onClick ( UpdateCurrentSlideContents ( Move ProjectHelpers.Bottom ) )
        , disabled ( slideIndex == (numberSlides - 1) )
        ]
        [ text "Move Slide to Bottom" ]

viewMoveSlideActionRow : Model -> Html Msg
viewMoveSlideActionRow model =
    div
        [ class "edit-page-move-slide-action-row" ]
        [ viewMoveSlideToTopButton model
        , viewMoveSlideUpButton model
        , viewMoveSlideDownButton model
        , viewMoveSlideToBottomButton model
        ]

viewDeleteSlideActionRow : Model -> Html Msg
viewDeleteSlideActionRow { slides } =
    div
        [ class "edit-page-delete-slide-action-row" ]
        [
            button
                [ class "slide-button"
                , disabled ( 1 == Dict.size slides )
                , onClick DeleteSlide
                ]
                [ text "Delete This Slide" ]
        ]

viewSlide : Maybe Slide.Model -> Html Msg
viewSlide maybeSlide =
    case maybeSlide of
        Just slide ->
            Slide.view slide
                |> Html.map SlideMsg

        Nothing ->
            div [ ] [ ]

viewCurrentSlide : Model -> Html Msg
viewCurrentSlide ( { slideIndex, slides } as model ) =
    div
        [ class "edit-page-current-slide" ]
        [ viewSlideInfoRow model
        , viewInsertSlideActionRow
        , viewMoveSlideActionRow model
        , viewDeleteSlideActionRow model
        , viewSlide (Dict.get slideIndex slides)
        ]

view : Model -> Html Msg
view model =
    viewCurrentSlide model
