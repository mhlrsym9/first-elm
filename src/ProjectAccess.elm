module ProjectAccess exposing (ProjectAccess)

import Flags
import Browser.Navigation as Navigation
import LanguageHelpers

type alias ProjectAccess =
    { flags : Flags.Model
    , kl : LanguageHelpers.Language
    , key : Navigation.Key
    , ll : LanguageHelpers.Language
    , pn : String
    }

