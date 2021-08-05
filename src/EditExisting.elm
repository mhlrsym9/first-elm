module EditExisting exposing (..)

import Array
import Browser.Navigation as Navigation
import Edit
import Html exposing (Html)

-- MODEL

type alias Model =
    Edit.Model

init : Navigation.Key -> String -> String -> String -> (Model, Cmd Msg)
init key k l p =
    Edit.init key k l p Array.empty

-- UPDATE

type alias Msg =
    Edit.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    Edit.update msg model

view : Model -> Html Msg
view model =
    Edit.view model
