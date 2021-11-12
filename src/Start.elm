module Start exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Html exposing (button, div, Html, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Routes

-- MODEL

type alias Model =
    { navigationKey : Navigation.Key }

initialModel : Navigation.Key -> Model
initialModel navigationKey =
    { navigationKey = navigationKey }

init : Navigation.Key -> Model
init navigationKey =
    initialModel navigationKey

-- UPDATE

type Msg = Create | Open

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Create ->
            ( model, Navigation.pushUrl model.navigationKey (Routes.routeToUrl Routes.Create) )

        Open ->
            ( model, Navigation.pushUrl model.navigationKey (Routes.routeToUrl Routes.Open) )

-- VIEW

view : Model -> Html Msg
view _ =
    div
        [ class "start-page" ]
        [ button
            [ onClick Create ]
            [ text "Create new Powerslides Module" ]
        , button
            [ onClick Open ]
            [ text "Open existing Powerslides Module" ]
        ]
