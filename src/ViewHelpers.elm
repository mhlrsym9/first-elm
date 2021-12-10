module ViewHelpers exposing (viewLanguageSelect)

import Element exposing (Element)
import LanguageSelect

viewLanguageSelect : String -> (LanguageSelect.Msg -> a) -> LanguageSelect.Model -> Element a
viewLanguageSelect selectLabel selectMsg languageModel =
    Element.row
        [ Element.centerX
        , Element.centerY
        , Element.spacing 10
        ]
        [ Element.text (selectLabel ++ ":")
        , LanguageSelect.view languageModel
            |> Element.map selectMsg
        ]
