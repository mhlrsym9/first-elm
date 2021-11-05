module ProjectAccess exposing (ProjectAccess)

import Flags
import Browser.Navigation as Navigation

type alias ProjectAccess =
    { flags : Flags.Model
    , kcc : String
    , key : Navigation.Key
    , lcc : String
    , pn : String
    }

