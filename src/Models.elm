module Models exposing (..)

import RemoteData exposing (WebData)
import Http
import Dict
import Set


type Route
    = HomeRoute (Maybe String) (Maybe String)
    | AboutRoute
    | NotFoundRoute


type alias Licenses =
    Set.Set String


type alias Sources =
    Set.Set String


type alias License =
    { name : String
    , url : String
    }


type alias Photo =
    { url : String
    , source : String
    , owner : Maybe String
    , description : Maybe String
    , license : Maybe License
    }


type alias Params =
    Dict.Dict String (Set.Set String)


type alias Model =
    { query : String
    , previousSearch : String
    , photos : WebData (List Photo)
    , route : Route
    , error : Maybe Http.Error
    , viewing : Maybe Photo
    , license : String
    , filtered : Bool
    , sources : Set.Set String
    }


initialModel : Route -> Model
initialModel route =
    { query = ""
    , previousSearch = ""
    , photos = RemoteData.Loading
    , route = route
    , error = Nothing
    , viewing = Nothing
    , license = "public"
    , sources = Set.singleton "flickr"
    , filtered = True
    }


getQuery : Params -> String
getQuery params =
    case (Dict.get "q" params) of
        Just vals ->
            if Set.isEmpty vals then
                ""
            else
                String.join "" (Set.toList vals)

        Nothing ->
            ""
