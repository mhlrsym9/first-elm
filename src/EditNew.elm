module EditNew exposing (Model, Msg, init, update, view)

import Api
import Array
import Browser.Navigation as Navigation
import Data.Project as Project
import Edit
import Html exposing (Html)

-- MODEL

type alias Model =
    Edit.Model

init : Navigation.Key -> String -> String -> String -> (Model, Cmd Msg)
init key k l p =
    Edit.init key k l p ( Api.Loaded Project.init )

-- UPDATE

type alias Msg =
    Edit.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    Edit.update msg model

view : Model -> Html Msg
view model =
    Edit.view model
