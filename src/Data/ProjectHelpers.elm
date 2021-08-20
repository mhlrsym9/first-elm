module Data.ProjectHelpers exposing (..)

import Dict exposing (Dict)

type IndexAdjustment =
    Increment
    | Decrement

adjustIndex : IndexAdjustment -> Int -> Int
adjustIndex shiftBy atIndex =
    case shiftBy of
        Increment ->
            atIndex + 1

        Decrement ->
            atIndex - 1

flipAdjacentEntries : Int -> IndexAdjustment -> Dict Int b -> Dict Int b
flipAdjacentEntries atIndex shiftBy answers =
    let
        otherIndex = adjustIndex shiftBy atIndex
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

updateIndexes : IndexAdjustment -> Dict Int b -> Dict Int b
updateIndexes shiftBy answers =
    answers
        |> Dict.toList
        |> List.map (\(i, q) -> ( (adjustIndex shiftBy i), q ) )
        |> Dict.fromList

moveEntry : Int -> IndexAdjustment -> Int -> Dict Int b -> Dict Int b
moveEntry index shiftBy finalIndex answers =
    let
        maybeAt = Dict.get index answers
    in
    case maybeAt of
        Just at ->
            let
                (beforeIndex, afterIndex) =
                    answers
                        |> Dict.remove index
                        |> Dict.partition (\i a -> (i < index))

                updatedBeforeIndex =
                    case shiftBy of
                        Increment ->
                            updateIndexes shiftBy beforeIndex

                        Decrement ->
                            beforeIndex

                updatedAfterIndex =
                    case shiftBy of
                        Increment ->
                            afterIndex

                        Decrement ->
                            updateIndexes shiftBy afterIndex

            in
            Dict.union updatedBeforeIndex updatedAfterIndex
                |> Dict.insert finalIndex at

        Nothing ->
            answers

