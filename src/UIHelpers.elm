module UIHelpers exposing (..)

import Element exposing (Attribute, centerY, Column, Element, IndexedColumn, padding, rgb255)
import Element.Background as Background
import Element.Border as Border

lightGrey : Element.Color
lightGrey =
    rgb255 211 211 211

black : Element.Color
black =
    rgb255 0 0 0

green : Element.Color
green =
    rgb255 0 128 0

buttonAttributes : List (Attribute msg)
buttonAttributes =
    [ centerY
    , padding 5
    , Background.color lightGrey
    , Border.rounded 3
    , Border.color black
    , Border.width 1
    ]
