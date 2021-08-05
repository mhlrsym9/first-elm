module Data.Question exposing (Model)

import Array exposing (Array)
import Data.Answer as Answer exposing (Model)

-- MODEL

type Text =
    Text String

type alias Model =
    { text : Text
    , index : Int
    , answers : Array Answer.Model
    }