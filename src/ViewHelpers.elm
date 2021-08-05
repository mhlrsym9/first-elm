module ViewHelpers exposing (viewLanguageSelect)

import Html exposing (Html, div, label, text)
import Html.Attributes exposing (class)
import LanguageSelect

viewLanguageSelect : String -> (LanguageSelect.Msg -> a) -> LanguageSelect.Model -> Html a
viewLanguageSelect selectLabel selectMsg languageModel =
    div
        [ class "language-select" ]
        [
            label
                [ ]
                [ text (selectLabel ++ ":")
                , LanguageSelect.view languageModel
                    |> Html.map selectMsg
                ]
        ]

