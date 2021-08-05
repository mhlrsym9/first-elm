module Data.Slide exposing (Model)

import Array exposing (Array)
import Data.Question as Question exposing (Model)

-- MODEL

type Text =
    Text String

type alias Model =
    { text : Text
    , questionIndex : Int
    , questions : Array Question.Model
    }

text : Text -> String
text (Text val) =
    val