module Models exposing (..)

import RemoteData exposing (WebData)
import Dict
import Array exposing (Array)


type Route
    = HomeRoute (Maybe String) (Maybe String)
    | AboutRoute
    | NotFoundRoute


type alias Model =
    { query : String
    , previousQuery : String
    , photos : WebData (Array Photo)
    , route : Route
    , viewing : Maybe Int
    , license : String
    , filtered : Bool
    , sources : Sources
    }


initialModel : Route -> Model
initialModel route =
    { query = ""
    , previousSearch = ""
    , photos = RemoteData.Loading
    , route = route
    , viewing = Nothing
    , license = "any"
    , sources = Dict.fromList [ ( "flickr", True ), ( "bing", False ) ]
    , filtered = True
    }


type alias Query =
    String


type alias License =
    String


type alias Sources =
    Dict.Dict String Bool


type alias PhotoLicense =
    { name : String
    , url : String
    }


type alias Photo =
    { url : String
    , thumbnail : String
    , origin : String
    , source : String
    , owner : Maybe String
    , title : Maybe String
    , description : Maybe String
    , license : Maybe PhotoLicense
    }


type alias Param =
    List String


type alias Params =
    Dict.Dict String (List String)
