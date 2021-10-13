module EditNew exposing (Model, Msg, init, update, view)

import Api
import Browser.Navigation as Navigation
import Data.Project as Project
import Edit
import Html exposing (Html)

-- MODEL

type alias Model =
    Edit.Model

init : { key : Navigation.Key, kcc : String, lcc : String, pn : String, sen : String } -> (Model, Cmd Msg)
init { key, kcc, lcc, pn, sen } =
    let
        (projectModel, projectCommands) = Project.init sen
        (editModel, editCommands) = Edit.init { key = key, kcc = kcc, lcc = lcc, pn = pn, model = Api.Loaded projectModel }
    in
    ( editModel
    , Cmd.batch
        [ Cmd.map Edit.ProjectMsg projectCommands
        , editCommands
        ]
    )

-- UPDATE

type alias Msg =
    Edit.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    Edit.update msg model

view : Model -> Html Msg
view model =
    Edit.view model
