module Helpers exposing (..)

import Navigation exposing (Location)
import UrlParser as Url exposing ((</>))

type Route
    = ChatRoute
    | NotFoundRoute

reverseRoute : Route -> String
reverseRoute route =
    case route of
        -- SeatSaverRoute ->
        --     "#/seatsaver"

        _ ->
            "#/"

routeParser : Url.Parser (Route ->a) a
routeParser =
    -- Url.oneOf
        -- [
            Url.map ChatRoute Url.top
        -- , Url.map SeatSaverRoute (Url.s "seatsaver")
        -- ]

parseLocation : Location -> Route
parseLocation location =
    location
        |> Url.parseHash routeParser
        |> Maybe.withDefault NotFoundRoute
