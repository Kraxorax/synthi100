module Routing exposing (Route(..), urlToRoute)

import Browser
import Url exposing (Url)
import Url.Builder exposing (absolute)
import Url.Parser exposing (Parser, int, map, oneOf, parse, s, top)


type Route
    = Database
    | Credits
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
        , map Credits (s "credits")
        , map About (s "about")
        ]
