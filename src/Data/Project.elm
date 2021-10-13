module Data.Project exposing (encodeProject, establishIndexes, projectDecoder, init, Model, Msg, update, view)

import Data.ProjectHelpers as ProjectHelpers
import Data.Slide as Slide
import Dict exposing (Dict)
import Dict.Extra
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, succeed)
import Json.Decode.Extra exposing (indexedList)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode

type alias Model =
    { slideIndex : Int
    , setupEditorName : String
    , slides : Dict Int Slide.Model
    }

slideDecoder : String -> Int -> Decoder (Int, Slide.Model)
slideDecoder sen index =
    Slide.slideDecoder sen
        |> Json.Decode.map (\s -> (index, s))

slidesDecoder : String -> Decoder (Dict Int Slide.Model)
slidesDecoder sen =
    (indexedList (slideDecoder sen))
        |> Json.Decode.map (\l -> Dict.fromList l)

projectDecoder : String -> Decoder Model
projectDecoder sen =
    succeed Model
        |> hardcoded 0
        |> hardcoded sen
        |> required "slides" (slidesDecoder sen)

encodeProject : Model -> Encode.Value
encodeProject { slides } =
    slides |> Dict.values |> Encode.list Slide.encodeSlide

init : String -> (Model, Cmd Msg)
init sen =
    ( { slideIndex = 0, slides = Dict.empty, setupEditorName = sen }, Cmd.none )

updateSlideIndexes : Dict Int Slide.Model -> Dict Int Slide.Model
updateSlideIndexes slides =
    Dict.map Slide.updateSlideIndex slides

establishIndexes : Model -> Model
establishIndexes ( { slides } as model ) =
    { model | slides = Dict.map (\i s -> Slide.establishIndexes i s) slides }

-- UPDATE

type Msg =
    DeleteSlide
    | DisplaySlide Int
    | InsertSlide ProjectHelpers.Direction
    | Move ProjectHelpers.Direction
    | SlideMsg Slide.Msg

createNewSlide : Int -> String -> (Dict Int Slide.Model, Cmd Msg)
createNewSlide slideIndex sen =
    let
        (newSlide, slideCommands) =
            Slide.init { slideIndex = slideIndex, sen = sen }
    in
    (Dict.singleton slideIndex newSlide, Cmd.map SlideMsg slideCommands)

insertSlideAtSlicePoint : Int -> Model -> (Model, Cmd Msg)
insertSlideAtSlicePoint slicePoint ( { slides, setupEditorName } as model ) =
    let
        (beforeSlides, afterSlides) = Dict.partition (\i _ -> (i < slicePoint)) slides
        (newSlides, commands) = createNewSlide slicePoint setupEditorName
        updatedSlides =
            afterSlides
                |> Dict.Extra.mapKeys (\k -> k + 1)
                |> updateSlideIndexes
                |> Dict.union newSlides
                |> Dict.union beforeSlides
    in
    ( { model | slides = updatedSlides, slideIndex = slicePoint }, commands )

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
            insertSlideAtSlicePoint 0 model

        InsertSlide ProjectHelpers.Up ->
            insertSlideAtSlicePoint slideIndex model

        InsertSlide ProjectHelpers.Down ->
            insertSlideAtSlicePoint (slideIndex + 1) model

        InsertSlide ProjectHelpers.Bottom ->
            insertSlideAtSlicePoint ((Dict.size slides) - 1) model

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
                        (updatedSlide, updatedSlideMsg) =
                            Slide.update slideMsg slide
                    in
                    ( { model | slides = Dict.insert slideIndex updatedSlide slides }
                    , Cmd.map SlideMsg updatedSlideMsg
                    )

                Nothing ->
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
        , onClick (DisplaySlide 0)
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
        , onClick ( DisplaySlide (slideIndex - 1) )
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
        , onClick ( DisplaySlide ( slideIndex + 1 ) )
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
        , onClick ( DisplaySlide (numberSlides - 1) )
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
        , onClick (InsertSlide ProjectHelpers.Top)
        ]
        [ text "<<- Insert new slide before first slide"]

viewInsertBeforeSlideButton : Html Msg
viewInsertBeforeSlideButton =
    button
        [ class "slide-button"
        , onClick (InsertSlide ProjectHelpers.Up)
        ]
        [ text "<- Insert new slide before this slide"]

viewInsertAfterSlideButton : Html Msg
viewInsertAfterSlideButton =
    button
        [ class "slide-button"
        , onClick (InsertSlide ProjectHelpers.Down)
        ]
        [ text "Add new slide after this slide ->"]

viewInsertAtBottomButton : Html Msg
viewInsertAtBottomButton =
    button
        [ class "slide-button"
        , onClick (InsertSlide ProjectHelpers.Bottom)
        ]
        [ text "Insert new slide after last slide ->>"]

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
        [ onClick (Move ProjectHelpers.Top)
        , disabled (0 == slideIndex)
        ]
        [ text "Move Slide to Top" ]

viewMoveSlideUpButton : Model -> Html Msg
viewMoveSlideUpButton { slideIndex } =
    button
        [ onClick (Move ProjectHelpers.Up)
        , disabled (0 == slideIndex)
        ]
        [ text "Move Slide Up" ]

viewMoveSlideDownButton : Model -> Html Msg
viewMoveSlideDownButton { slideIndex, slides }  =
    let
        numberSlides = Dict.size slides
    in
    button
        [ onClick (Move ProjectHelpers.Down)
        , disabled ( slideIndex == (numberSlides - 1) )
        ]
        [ text "Move Slide Down" ]

viewMoveSlideToBottomButton : Model -> Html Msg
viewMoveSlideToBottomButton { slideIndex, slides } =
    let
        numberSlides = Dict.size slides
    in
    button
        [ onClick (Move ProjectHelpers.Bottom)
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
                [ text "Delete This Slide"]
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
