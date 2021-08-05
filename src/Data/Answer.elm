module Data.Answer exposing (Model)

import Array exposing (Array)

-- MODEL

type Text =
    Text String

type alias Model =
    { text : Text
    , isCorrect : Bool
    }