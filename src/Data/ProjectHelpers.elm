module Data.ProjectHelpers exposing (..)

import Dict exposing (Dict)

shiftIndexes : Int -> Int -> Dict Int b -> Dict Int b
shiftIndexes atIndex otherIndex answers =
    let
        maybeOther = Dict.get otherIndex answers
    in
    case maybeOther of
        Just other ->
            let
                maybeAt = Dict.get atIndex answers
            in
            case maybeAt of
                Just at ->
                    answers
                        |> Dict.insert otherIndex at
                        |> Dict.insert atIndex other

                Nothing ->
                    answers

        Nothing ->
            answers

updateIndexes : Int -> Int -> Int -> Dict Int b -> Dict Int b
updateIndexes index shiftBy finalIndex answers =
    let
        maybeAt = Dict.get index answers
    in
    case maybeAt of
        Just at ->
            answers
                |> Dict.remove index
                |> Dict.toList
                |> List.map (\(i, q) -> ( (i + shiftBy), q ))
                |> Dict.fromList
                |> Dict.insert finalIndex at

        Nothing ->
            answers

