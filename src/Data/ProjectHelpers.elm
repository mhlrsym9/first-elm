module Data.ProjectHelpers exposing (..)

import Dict exposing (Dict)

type Direction =
    Up
    | Down
    | Top
    | Bottom

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
updateIndexes shiftBy theDict =
    theDict
        |> Dict.toList
        |> List.map (\(i, q) -> ( (adjustIndex shiftBy i), q ) )
        |> Dict.fromList

moveEntry : Int -> IndexAdjustment -> Int -> Dict Int b -> Dict Int b
moveEntry index shiftBy finalIndex theDict =
    let
        maybeAt = Dict.get index theDict
    in
    case maybeAt of
        Just at ->
            let
                (beforeIndex, afterIndex) =
                    theDict
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
            theDict

deleteEntry : Int -> Dict Int b -> Dict Int b
deleteEntry index theDict =
    let
        (keepSameIndex, decrementIndex) =
            theDict
                |> Dict.remove index
                |> Dict.partition (\i _ -> (i < index))
    in
    Dict.union keepSameIndex (updateIndexes Decrement decrementIndex)
