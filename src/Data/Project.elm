module Data.Project exposing (encodeProject, establishIndexes, insertSlideBefore, projectDecoder, init, Model, Msg, update, view)

import Array exposing (Array)
import Data.Slide as Slide
import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (array, Decoder, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode as Encode

type alias Model =
    { slideIndex : Int
    , slides : Array Slide.Model }

projectDecoder : Decoder Model
projectDecoder =
    succeed Model
        |> hardcoded 0
        |> required "slides" (array Slide.slideDecoder)

encodeProject : Model -> Encode.Value
encodeProject model =
    Encode.array Slide.encodeSlide model.slides

init : (Model, Cmd Msg)
init =
    ( { slideIndex = 0, slides = Array.empty }, Cmd.none )

establishIndexes : Model -> Model
establishIndexes ( { slides } as model ) =
    { model | slides = Array.indexedMap Slide.establishIndexes slides }

-- UPDATE

type Msg =
    DeleteSlide
    | DisplaySlide Int
    | InsertSlideAfter
    | InsertSlideBefore
    | SlideMsg Slide.Msg

createNewSlide : (Array Slide.Model, Cmd Msg)
createNewSlide =
    let
        newSlideTuples = Array.repeat 1 Slide.init
        newSlides = Array.map (\(slide, _) -> slide) newSlideTuples
        commands = Array.toList (Array.map (\(_, slideCommands) -> Cmd.map SlideMsg slideCommands) newSlideTuples)
    in
    (newSlides, Cmd.batch commands)

insertSlideAtSlicePoint : Int -> Model -> (Model, Cmd Msg)
insertSlideAtSlicePoint slicePoint ( { slides } as model ) =
    let
        beforeSlides = Array.slice 0 slicePoint slides
        (newSlides, commands) = createNewSlide
        afterSlides = Array.slice slicePoint ( Array.length slides ) slides
    in
    (
        { model | slides = Array.append beforeSlides ( Array.append newSlides afterSlides )
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
                beforeSlides = Array.slice 0 slideIndex slides
                afterSlides = Array.slice (slideIndex + 1) ( Array.length slides ) slides
            in
            (
                { model | slides = Array.append beforeSlides afterSlides
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
                maybeSlide = Array.get slideIndex slides
            in
            case maybeSlide of
                Just slide ->
                    let
                        (updatedSlide, updatedSlideMsg) =
                            Slide.update slideMsg slide
                    in
                    ( { model | slides = Array.set slideIndex updatedSlide slides }
                    , Cmd.map SlideMsg updatedSlideMsg
                    )

                Nothing ->
                    ( model, Cmd.none )

-- VIEW

viewFirstSlideButton : Model -> Html Msg
viewFirstSlideButton { slideIndex, slides } =
    let
        numberSlides = Array.length slides
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
        numberSlides = Array.length slides
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
        numberSlides = Array.length slides
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
        numberSlides = Array.length slides
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
        numberSlides = Array.length slides
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
                , disabled ( 1 == Array.length slides )
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
        , viewSlide (Array.get slideIndex slides)
        ]

view : Model -> Html Msg
view model =
    viewCurrentSlide model
