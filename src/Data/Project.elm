module Data.Project exposing (encodeProject, establishIndexes, insertSlideBefore, projectDecoder, init, Model, Msg, update, view)

import Array exposing (Array)
import Data.ProjectHelpers as ProjectHelpers
import Data.Slide as Slide
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Json.Decode exposing (array, Decoder, succeed)
import Json.Decode.Extra exposing (indexedList)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode

type alias Model =
    { slideIndex : Int
    , slides : Dict Int Slide.Model }

slideDecoder : Int -> Decoder (Int, Slide.Model)
slideDecoder index =
    Slide.slideDecoder
        |> Json.Decode.map (\s -> (index, s))

slidesDecoder : Decoder (Dict Int Slide.Model)
slidesDecoder =
    (indexedList slideDecoder)
        |> Json.Decode.map (\l -> Dict.fromList l)

projectDecoder : Decoder Model
projectDecoder =
    succeed Model
        |> hardcoded 0
        |> required "slides" slidesDecoder

encodeProject : Model -> Encode.Value
encodeProject { slides } =
    slides |> Dict.values |> Encode.list Slide.encodeSlide

init : (Model, Cmd Msg)
init =
    ( { slideIndex = 0, slides = Dict.empty }, Cmd.none )

establishIndexes : Model -> Model
establishIndexes ( { slides } as model ) =
    { model | slides = Dict.map (\i s -> Slide.establishIndexes i s) slides }

-- UPDATE

type Msg =
    DeleteSlide
    | DisplaySlide Int
    | InsertSlideAfter
    | InsertSlideBefore
    | SlideMsg Slide.Msg

createNewSlide : Int -> (Dict Int Slide.Model, Cmd Msg)
createNewSlide index =
    let
        (newSlide, slideCommands) =
            Slide.init
        updatedSlide = Slide.establishIndexes index newSlide
    in
    (Dict.singleton index updatedSlide, Cmd.map SlideMsg slideCommands)

insertSlideAtSlicePoint : Int -> Model -> (Model, Cmd Msg)
insertSlideAtSlicePoint slicePoint ( { slides } as model ) =
    let
        (beforeSlides, afterSlides) = Dict.partition (\i _ -> (i < slicePoint)) slides
        (newSlides, commands) = createNewSlide slicePoint
    in
    (
        { model | slides = Dict.union beforeSlides ( Dict.union newSlides afterSlides )
                , slideIndex = slicePoint
        }
        , commands
    )

insertSlideAfter : Model -> (Model, Cmd Msg)
insertSlideAfter ( { slideIndex } as model ) =
    insertSlideAtSlicePoint (slideIndex + 1) model

insertSlideBefore : Model -> (Model, Cmd Msg)
insertSlideBefore ( { slideIndex } as model ) =
    insertSlideAtSlicePoint slideIndex model

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
                { model | slides = ProjectHelpers.deleteEntry slideIndex slides
                        , slideIndex = updatedSlideIndex
                }
                , Cmd.none
            )

        DisplaySlide updatedSlideIndex ->
            ( { model | slideIndex = updatedSlideIndex }, Cmd.none )

        InsertSlideAfter ->
            insertSlideAfter model

        InsertSlideBefore ->
            insertSlideBefore model

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

viewInsertBeforeSlideButton : Html Msg
viewInsertBeforeSlideButton =
    button
        [ class "slide-button"
        , onClick InsertSlideBefore
        ]
        [ text "<- Insert new slide before this slide"]

viewInsertAfterSlideButton : Html Msg
viewInsertAfterSlideButton =
    button
        [ class "slide-button"
        , onClick InsertSlideAfter
        ]
        [ text "Add new slide after this slide ->"]

viewInsertSlideActionRow : Html Msg
viewInsertSlideActionRow =
    div
         [ class "edit-page-insert-slide-action-row" ]
         [ viewInsertBeforeSlideButton
         , viewInsertAfterSlideButton
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
        , viewDeleteSlideActionRow model
        , viewSlide (Dict.get slideIndex slides)
        ]

view : Model -> Html Msg
view model =
    viewCurrentSlide model
