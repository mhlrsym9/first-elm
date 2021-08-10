module Data.Project exposing (..)

import Array exposing (Array)
import Data.Slide as Slide
import Json.Decode exposing (array, Decoder, succeed)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode

type alias Model =
    { slides : Array Slide.Model }

projectDecoder : Decoder Model
projectDecoder =
    succeed Model
        |> required "slides" (array Slide.slideDecoder)

encodeProject : Model -> Encode.Value
encodeProject model =
    Encode.array Slide.encodeSlide model.slides