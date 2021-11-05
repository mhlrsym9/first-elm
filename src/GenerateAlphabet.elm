module GenerateAlphabet exposing (..)

import Generate
import ProjectAccess exposing (ProjectAccess)

-- MODEL

type alias Model =
    Generate.Model

init : ProjectAccess -> (Model, Cmd Msg)
init { flags, key, kcc, lcc, pn } =
    Generate.init
        { flags = flags
        , imageRepository = "alphabet"
        , key = key
        , kcc = kcc
        , lcc = lcc
        , pn = pn
        }

-- UPDATE

type alias Msg =
    Generate.Msg

