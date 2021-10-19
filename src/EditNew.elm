module EditNew exposing (Model, Msg, init, update, view)

import Api
import Browser.Navigation as Navigation
import Edit
import Html exposing (Html)

-- MODEL

type alias Model =
    Edit.Model

init : { key : Navigation.Key, kcc : String, lcc : String, pn : String, sen : String } -> (Model, Cmd Msg)
init { key, kcc, lcc, pn, sen } =
    let
        (projectModel, projectCommands) = Edit.initNewProject sen
        (editModel, editCommands) = Edit.init { key = key, kcc = kcc, lcc = lcc, pn = pn, model = Api.Loaded projectModel }
    in
    ( editModel
    , Cmd.batch
        [ Cmd.map EditMsg projectCommands
        , Cmd.map EditMsg editCommands
        ]
    )

-- UPDATE

type Msg =
    EditMsg Edit.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        EditMsg editMsg ->
            let
                (updatedEditModel, editCommands) =
                    Edit.update editMsg model
            in
            ( updatedEditModel
            , Cmd.map EditMsg editCommands
            )

view : Model -> Html Msg
view model =
    Html.map EditMsg (Edit.view model)
