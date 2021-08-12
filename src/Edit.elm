module Edit exposing (Model, Msg, init, update, view)

import Api
import Array exposing (Array)
import Browser.Navigation as Navigation
import Data.Project as Project
import Data.Slide as Slide
import Html exposing (Html, button, div, h1, text, textarea)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Routes

-- MODEL

type alias Model =
    { slideIndex : Int
    , project : Api.Status Project.Model
    , knownContentCode : String
    , learningContentCode : String
    , projectName : String
    , navigationKey : Navigation.Key
    }

init : Navigation.Key -> String -> String -> String -> Api.Status Project.Model -> (Model, Cmd Msg)
init navigationKey knownContentCode learningContentCode projectName project =
    (
        { slideIndex = 0
        , project = project
        , knownContentCode = knownContentCode
        , learningContentCode = learningContentCode
        , projectName = projectName
        , navigationKey = navigationKey
        }
        , Cmd.none
    )

-- UPDATE

type Insert =
    Before Int
    | After Int

type Msg =
    DeleteSlide Int
    | InsertSlideBefore Int
    | UpdateSlide Int
    | Cancel

update : Msg -> Model -> (Model, Cmd Msg)
update msg ( { project } as model ) =
    case msg of
        DeleteSlide slideIndex ->
            case project of
                Api.Loaded slideModel ->
                    let
                        updatedSlideIndex =
                            if (0 == slideIndex) then
                                0
                            else
                                slideIndex - 1
                    in
                    (
                        {
                            model |
                                project = Api.Loaded ( Project.deleteSlide slideIndex slideModel )
                                , slideIndex = updatedSlideIndex
                        }
                        , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        InsertSlideBefore slideIndex ->
            case project of
                Api.Loaded slideModel ->
                    (
                        {
                            model |
                                project = Api.Loaded ( Project.insertSlideBefore slideIndex slideModel )
                                , slideIndex = slideIndex
                        }
                        , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateSlide slideIndex ->
            ( { model | slideIndex = slideIndex }, Cmd.none )

        Cancel ->
            ( model, Navigation.pushUrl model.navigationKey (Routes.routeToUrl Routes.Home) )

viewEditPageInfo : Model -> Html Msg
viewEditPageInfo { knownContentCode, learningContentCode, projectName } =
    div
        [ class "edit-page-info"]
        [
            div
                [ class "edit-page-language-info" ]
                [
                    div
                        [ ]
                        [ text ( "Known Language Code: " ++ knownContentCode ) ]
                    ,
                    div
                        [ ]
                        [ text ( "Learning Language Code: " ++ learningContentCode ) ]
                ]
            ,
            h1
                [ class "edit-page-project-name" ]
                [ text ( "Project Name: " ++ projectName ) ]
        ]

viewFirstSlideButton : Int -> Int -> Html Msg
viewFirstSlideButton slideIndex numberSlides =
    button
        [ class "slide-button"
        , disabled ( (slideIndex == 0) || (numberSlides == 1) )
        , onClick (UpdateSlide 0)
        ]
        [ text "<<- First" ]

viewPreviousSlideButton : Int -> Int -> Html Msg
viewPreviousSlideButton slideIndex numberSlides =
    button
        [ class "slide-button"
        , disabled ( (slideIndex == 0) || (numberSlides == 1) )
        , onClick ( UpdateSlide (slideIndex - 1) )
        ]
        [ text "<- Previous"]

viewNextSlideButton : Int -> Int -> Html Msg
viewNextSlideButton slideIndex numberSlides =
    button
        [ class "slide-button"
        , disabled ( slideIndex == (numberSlides - 1) )
        , onClick ( UpdateSlide ( slideIndex + 1 ) )
        ]
        [ text "Next ->" ]

viewLastSlideButton : Int -> Int -> Html Msg
viewLastSlideButton slideIndex numberSlides =
    button
        [ class "slide-button"
        , disabled ( slideIndex == (numberSlides - 1) )
        , onClick ( UpdateSlide (numberSlides - 1) )
        ]
        [ text "Last ->>" ]

viewSlideInfoRow : Int -> Project.Model -> Html Msg
viewSlideInfoRow slideIndex { slides } =
    let
        numberSlides = Array.length slides
    in
    div
        [ class "edit-page-slide-info-row" ]
        [ viewFirstSlideButton slideIndex numberSlides
        , viewPreviousSlideButton slideIndex numberSlides
        , text
            ( "Slide "
            ++ ( String.fromInt ( slideIndex + 1 ) )
            ++ " of "
            ++ ( String.fromInt numberSlides )
            )
        , viewNextSlideButton slideIndex numberSlides
        , viewLastSlideButton slideIndex numberSlides
        ]

viewInsertBeforeSlideButton : Int -> Html Msg
viewInsertBeforeSlideButton slideIndex =
    button
        [ class "slide-button"
        , onClick (InsertSlideBefore slideIndex )
        ]
        [ text "<- Insert new slide before this slide"]

viewInsertAfterSlideButton : Int -> Html Msg
viewInsertAfterSlideButton slideIndex =
    button
        [ class "slide-button"
        , onClick (InsertSlideBefore ( slideIndex + 1 ) )
        ]
        [ text "Add new slide after this slide ->"]

viewInsertSlideActionRow : Int -> Html Msg
viewInsertSlideActionRow slideIndex =
    div
         [ class "edit-page-insert-slide-action-row" ]
         [ viewInsertBeforeSlideButton slideIndex
         , viewInsertAfterSlideButton slideIndex
         ]

viewDeleteSlideActionRow : Int -> Array Slide.Model -> Html Msg
viewDeleteSlideActionRow slideIndex slides =
    div
        [ class "edit-page-delete-slide-action-row" ]
        [
            button
                [ class "slide-button"
                , disabled ( 1 == Array.length slides )
                , onClick ( DeleteSlide slideIndex )
                ]
                [ text "Delete This Slide"]
        ]

viewSlide : Maybe Slide.Model -> Html Msg
viewSlide maybeSlide =
    case maybeSlide of
        Just slide ->
            textarea
                [ class "edit-page-slide" ]
                [ text (Slide.textToString slide.text) ]
        Nothing ->
            div [ ] [ ]

viewCurrentSlide : Int -> Project.Model -> Html Msg
viewCurrentSlide slideIndex ( { slides } as projectModel ) =
    div
        [ class "edit-page-current-slide" ]
        [ viewSlideInfoRow slideIndex projectModel
        , viewInsertSlideActionRow slideIndex
        , viewDeleteSlideActionRow slideIndex slides
        , viewSlide (Array.get slideIndex slides)
        ]

viewActionButtons : Model -> Html Msg
viewActionButtons _ =
    div
        [ class "edit-page-action-buttons" ]
        [ button
            [ onClick Cancel ]
            [ text "Cancel" ]
        ]

view : Model -> Html Msg
view ( { slideIndex, project } as model ) =
    case project of
        Api.Loaded projectModel ->
            div
                [ class "edit-page" ]
                [ viewEditPageInfo model
                , viewCurrentSlide slideIndex projectModel
                , viewActionButtons model
                ]

        _ ->
            div
                [ ]
                [ ]


