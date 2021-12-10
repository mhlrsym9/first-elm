module Start exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Element exposing (centerX, Element, row, spacing)
import Element.Input as Input
import Routes
import UIHelpers exposing (buttonAttributes)

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

view : Model -> Element Msg
view _ =
    row
        [ centerX
        , spacing 10
        ]
        [ Input.button
            buttonAttributes
            { onPress = Just Create
            , label = Element.text "Create new Powerslides Module"
            }
        , Input.button
            buttonAttributes
            { onPress = Just Open
            , label = Element.text "Open existing Powerslides Module"
            }
        ]
