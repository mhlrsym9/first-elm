module Routes exposing (Route(..), href, match, routeToUrl)

import Html
import Html.Attributes
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query

type Route
    = Home
    | Create
    | Open
    | EditNew String String (Maybe String)
    | EditExisting String String (Maybe String)

routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Create (Parser.s "create")
        , Parser.map Open (Parser.s "open")
        , Parser.map EditNew (Parser.s "edit" </> Parser.s "new" </> Parser.string </> Parser.string <?> Query.string "projectName")
        , Parser.map EditExisting (Parser.s "edit" </> Parser.s "existing" </> Parser.string </> Parser.string <?> Query.string "projectName")
        ]

match : Url -> Maybe Route
match url =
    Parser.parse routes url

routeToUrl : Route -> String
routeToUrl route =
    case route of
        Home ->
            "/"

        Create ->
            "/create"

        Open ->
            "/open"

        EditNew k l p ->
            case p of
                Just projectName ->
                    "/edit/new/" ++ k ++ "/" ++ l ++ "?" ++ "projectName=" ++ projectName

                Nothing ->
                    "/edit/new/" ++ k ++ "/" ++ l

        EditExisting k l p ->
            case p of
                Just projectName ->
                    "/edit/existing/" ++ k ++ "/" ++ l ++ "?" ++ "projectName=" ++ projectName

                Nothing ->
                    "/edit/existing/" ++ k ++ "/" ++ l

href : Route -> Html.Attribute msg
href route =
    Html.Attributes.href (routeToUrl route)