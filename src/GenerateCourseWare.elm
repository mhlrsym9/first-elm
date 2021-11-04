module GenerateCourseWare exposing (..)

import Browser.Navigation as Navigation
import Flags exposing (Flags)
import Generate

-- MODEL

type alias Model =
    Generate.Model

type alias Init =
    { flags : Flags
    , kcc : String
    , key : Navigation.Key
    , lcc : String
    , pn : String
    }

init : Init -> (Model, Cmd Msg)
init { flags, key, kcc, lcc, pn } =
    Generate.init
        { flags = flags
        , imageRepository = "cw"
        , key = key
        , kcc = kcc
        , lcc = lcc
        , pn = pn
        }

-- UPDATE

type alias Msg =
    Generate.Msg
