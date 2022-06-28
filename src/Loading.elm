module Loading exposing (error, iconElement, slowThreshold)

{-| A loading spinner icon.
-}

import Element exposing (Element)
import Process
import Task exposing (Task)

iconElement : String -> Element msg
iconElement loadingPath =
    Element.image
        [ Element.width (Element.px 200)
        , Element.height (Element.px 200)
        , Element.centerX
        ]
        { src = loadingPath
        , description = "Loading..."
        }

error : String -> Element msg
error str =
    Element.text ("Error loading " ++ str ++ ".")

slowThreshold : Task x ()
slowThreshold =
    Process.sleep 500
