module Main exposing (..)

import Api
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Create
import Edit
import EditExisting
import EditNew
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import Http exposing (Response)
import LanguageSelect
import Loading
import Open
import Routes
import Start
import Task exposing (Task)
import Url exposing (Url)


---- MODEL ----

type Page
    = Start Start.Model
    | Create Create.Model
    | Open Open.Model
    | Edit Edit.Model
    | NotFound

type alias Model =
    { page : Page
    , languages : Api.Status LanguageSelect.Languages
    , navigationKey : Navigation.Key
    }

initialModel : Navigation.Key -> Model
initialModel navigationKey =
    { page = NotFound
    , languages = Api.Loading
    , navigationKey = navigationKey
    }

init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init () url navigationKey =
    let
        (model, cmd) =
            setNewPage (Routes.match url) (initialModel navigationKey)
    in
    ( model
    , Cmd.batch
        [ cmd
        , LanguageSelect.fetchLanguages
            |> Task.attempt CompletedLanguageLoad
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )

---- UPDATE ----


type Msg
    = NewRoute (Maybe Routes.Route)
    | Visit UrlRequest
    | StartMsg Start.Msg
    | CreateMsg Create.Msg
    | OpenMsg Open.Msg
    | EditMsg Edit.Msg
    | EditExistingMsg EditExisting.Msg
    | CompletedLanguageLoad (Result Http.Error LanguageSelect.Languages)
    | PassedSlowLoadThreshold

setNewPage : Maybe Routes.Route -> Model -> ( Model, Cmd Msg )
setNewPage maybeRoute model =
    case maybeRoute of
        Just Routes.Home ->
            let
                ( startModel, startCmd ) =
                    Start.init model.navigationKey
            in
            ( { model | page = Start startModel }
            , Cmd.map StartMsg startCmd )

        Just Routes.Create ->
            let
                ( createModel, createCmd ) =
                    Create.init
                        model.navigationKey <|
                        case model.languages of
                            Api.Loaded languages ->
                                languages
                            _ ->
                                []
            in
            ( { model | page = Create createModel }
            , Cmd.map CreateMsg createCmd )

        Just Routes.Open ->
            let
                ( openModel, openCmd ) =
                    Open.init
                        model.navigationKey <|
                        case model.languages of
                            Api.Loaded languages ->
                                languages
                            _ ->
                                []
            in
            ( { model | page = Open openModel }
            , Cmd.map OpenMsg openCmd )

        Just (Routes.EditNew k l p) ->
            case p of
                Just projectName ->
                    let
                        ( editModel, editCmd ) =
                            EditNew.init { key = model.navigationKey, kcc = k, lcc = l, pn =  projectName }
                    in
                    ( { model | page = Edit editModel }
                    , Cmd.map EditMsg editCmd )
                Nothing ->
                    ( model, Cmd.none )

        Just (Routes.EditExisting k l p) ->
            case p of
                Just projectName ->
                    let
                        ( editModel, editCmd ) =
                            EditExisting.init { key = model.navigationKey, kcc = k, lcc = l, pn = projectName }
                    in
                    ( { model | page = Edit editModel }
                    , Cmd.map EditExistingMsg editCmd )
                Nothing ->
                    ( model, Cmd.none )

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( NewRoute maybeRoute, _ ) ->
            setNewPage maybeRoute model

        ( StartMsg startMsg, Start startModel ) ->
            let
                ( updatedStartModel, startCmd ) =
                    Start.update startMsg startModel
            in
            ( { model | page = Start updatedStartModel }
            , Cmd.map StartMsg startCmd
            )

        ( StartMsg _, _ ) ->
            Debug.todo "Handle StartMsg error case"

        ( CreateMsg createMsg, Create createModel ) ->
            let
                ( updatedCreateModel, createCmd ) =
                    Create.update createMsg createModel
            in
            ( { model | page = Create updatedCreateModel }
            , Cmd.map CreateMsg createCmd
            )

        ( CreateMsg _, _ ) ->
            Debug.todo "Handle CreateMsg error case"

        ( OpenMsg openMsg, Open openModel ) ->
            let
                ( updatedOpenModel, openCmd ) =
                    Open.update openMsg openModel
            in
            ( { model | page = Open updatedOpenModel }
            , Cmd.map OpenMsg openCmd
            )

        ( OpenMsg _, _ ) ->
            Debug.todo "Handle OpenMsg error case"

        ( EditMsg editMsg, Edit editModel ) ->
            let
                ( updatedEditModel, editCmd ) =
                    Edit.update editMsg editModel
            in
            ( { model | page = Edit updatedEditModel }
            , Cmd.map EditMsg editCmd
            )

        ( EditMsg _, _ ) ->
            Debug.todo "Handle EditMsg error case"

        ( EditExistingMsg editExistingMsg, Edit editModel ) ->
            let
                ( updatedEditModel, editCmd ) =
                    EditExisting.update editExistingMsg editModel
            in
            ( { model | page = Edit updatedEditModel }
            , Cmd.map EditExistingMsg editCmd
            )

        ( EditExistingMsg _, _ ) ->
            Debug.todo "Handle EditExistingMsg error case"

        ( Visit _, _ ) ->
            Debug.todo "Handle Visit"

        ( CompletedLanguageLoad result, _ ) ->
            case result of
                Ok languages ->
                    ( { model | languages = Api.Loaded languages }, Cmd.none )

                Err _ ->
                    ( { model | languages = Api.Failed }, Cmd.none )

        ( PassedSlowLoadThreshold, _ ) ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                languages =
                    case model.languages of
                        Api.Loading ->
                            Api.LoadingSlowly

                        other ->
                            other
            in
            ( { model | languages = languages }, Cmd.none )


---- VIEW ---

viewStandardHeader : String -> Html Msg
viewStandardHeader header =
    h1 [] [ text header ]

viewHeader : Page -> Html Msg
viewHeader page =
    case page of
        Start startModel ->
            Start.view startModel
                |> Html.map StartMsg

        Create createModel ->
            Create.view createModel
                |> Html.map CreateMsg

        Open openModel ->
            Open.view openModel
                |> Html.map OpenMsg

        Edit editModel ->
            Edit.view editModel
                |> Html.map EditMsg

        NotFound ->
            div
                [ class "not-found" ]
                [ h1
                    []
                    [ text "Page Not Found" ]
                ]

viewContent : Model -> ( String, Html Msg )
viewContent { page, languages } =
    let
        contents =
            case languages of
                Api.Loading ->
                    div [] []

                Api.LoadingSlowly ->
                    div [] [ Loading.icon ]

                Api.Failed ->
                    div [] [ Loading.error "languages" ]

                Api.Loaded _ ->
                    viewHeader page

    in
    ( "Candor HTML", contents )

view : Model -> Document Msg
view model =
    let
        ( title, content ) =
            viewContent model
    in
    { title = title
    , body = [ content ]
    }

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = Visit
        , onUrlChange = Routes.match >> NewRoute
        }
