module ProjectAccess exposing (ProjectAccess)

import Flags
import Browser.Navigation as Navigation
import LanguageSelect

type alias ProjectAccess =
    { flags : Flags.Model
    , kl : LanguageSelect.Language
    , key : Navigation.Key
    , ll : LanguageSelect.Language
    , pn : String
    }

