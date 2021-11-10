module GenerateAlphabet exposing (..)

import Generate
import ProjectAccess exposing (ProjectAccess)

-- MODEL

type alias Model =
    Generate.Model

init : ProjectAccess -> (Model, Cmd Msg)
init { flags, key, kl, ll, pn } =
    Generate.init
        { flags = flags
        , imageRepository = "alphabet"
        , key = key
        , kl = kl
        , ll = ll
        , pn = pn
        }

-- UPDATE

type alias Msg =
    Generate.Msg

