module Routing exposing (..)

import Navigation exposing (Location)
import Models exposing (Route(..))
import UrlParser exposing (..)


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map HomeRoute (s "" <?> stringParam "q" <?> stringParam "license")
        , map AboutRoute (s "about")
        ]


parseLocation : Location -> Route
parseLocation location =
    location
        |> parsePath matchers
        |> Maybe.withDefault NotFoundRoute
