module Routes exposing (Route(..), href, match, routeToUrl)

import Html
import Html.Attributes
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query

type Route
    = Create
    | Delete String String (Maybe String)
    | EditNew String String (Maybe String)
    | EditExisting String String (Maybe String)
    | GenerateAlphabet String String (Maybe String)
    | GenerateCourseWare String String (Maybe String)
    | Home
    | Open

routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map Create (Parser.s "create")
        , Parser.map Delete (Parser.s "delete" </> Parser.string </> Parser.string <?> Query.string "projectName")
        , Parser.map EditNew (Parser.s "edit" </> Parser.s "new" </> Parser.string </> Parser.string <?> Query.string "projectName")
        , Parser.map EditExisting (Parser.s "edit" </> Parser.s "existing" </> Parser.string </> Parser.string <?> Query.string "projectName")
        , Parser.map GenerateAlphabet (Parser.s "generate" </> Parser.s "alphabet" </> Parser.string </> Parser.string <?> Query.string "projectName")
        , Parser.map GenerateCourseWare (Parser.s "generate" </> Parser.s "cw" </> Parser.string </> Parser.string <?> Query.string "projectName")
        , Parser.map Home Parser.top
        , Parser.map Open (Parser.s "open")
        ]

match : Url -> Maybe Route
match url =
    Parser.parse routes url

routeToUrl : Route -> String
routeToUrl route =
    case route of
        Create ->
            "/create"

        Delete k l p ->
            case p of
                Just projectName ->
                    "/delete/" ++ k ++ "/" ++ l ++ "?" ++ "projectName=" ++ projectName

                Nothing ->
                    "/delete/" ++ k ++ "/" ++ l

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

        GenerateAlphabet k l p ->
            case p of
                Just projectName ->
                    "/generate/alphabet/" ++ k ++ "/" ++ l ++ "?" ++ "projectName=" ++ projectName

                Nothing ->
                    "/generate/alphabet/" ++ k ++ "/" ++ l

        GenerateCourseWare k l p ->
            case p of
                Just projectName ->
                    "/generate/cw/" ++ k ++ "/" ++ l ++ "?" ++ "projectName=" ++ projectName

                Nothing ->
                    "/generate/cw/" ++ k ++ "/" ++ l

        Home ->
            "/"

        Open ->
            "/open"

href : Route -> Html.Attribute msg
href route =
    Html.Attributes.href (routeToUrl route)