module Routing exposing (Route(..), urlToRoute)

import Browser
import Url exposing (Url)
import Url.Builder exposing (absolute)
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string, top)


type Route
    = Database
    | Patch String
    | PatchGraphical String
    | About


urlToRoute : Url -> Route
urlToRoute =
    parseRoute >> Maybe.withDefault Database


parseRoute : Url -> Maybe Route
parseRoute =
    parse route


route : Parser (Route -> a) a
route =
    oneOf
        [ map Database (s "database")
        , map PatchGraphical (s "patch" </> string </> s "graphical")
        , map Patch (s "patch" </> string)
        , map About (s "about")
        ]


l : List String
l =
    [ "a", "b", "c" ]


l1 : List String
l1 =
    "a" :: "b" :: [ "c" ]
