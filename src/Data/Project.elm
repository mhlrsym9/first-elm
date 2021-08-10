module Data.Project exposing (..)

import Array exposing (Array)
import Data.Slide as Slide
import Json.Decode exposing (array, Decoder)
import Json.Encode as Encode

type alias Model =
    Array Slide.Model

projectDecoder : Decoder Model
projectDecoder =
    (array Slide.slideDecoder)

encodeProject : Model -> Encode.Value
encodeProject model =
    Encode.array Slide.encodeSlide model