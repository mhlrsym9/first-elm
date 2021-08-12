module Data.Project exposing (deleteSlide, insertSlideBefore, projectDecoder, Model)

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

numberSlides : Model -> Int
numberSlides model =
    Array.length model.slides

deleteSlide : Int -> Model -> Model
deleteSlide index ( { slides } as model ) =
    let
        beforeSlides = Array.slice 0 index slides
        afterSlides = Array.slice (index + 1) ( Array.length slides ) slides
    in
    { model | slides = Array.append beforeSlides afterSlides }

insertSlideBefore : Int -> Model -> Model
insertSlideBefore index ( { slides } as model ) =
    let
        beforeSlides = Array.slice 0 index slides
        newSlides = Array.repeat 1 Slide.init
        afterSlides = Array.slice index ( Array.length slides ) slides
    in
    { model | slides = Array.append beforeSlides ( Array.append newSlides afterSlides ) }