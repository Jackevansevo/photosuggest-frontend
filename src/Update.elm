module Update exposing (..)

import Msgs exposing (Msg)
import Models exposing (Route(..), Model)
import Routing exposing (parseLocation)
import Navigation exposing (newUrl)
import Commands exposing (searchImage)
import RemoteData exposing (RemoteData(..))
import Dom
import Task
import Dict
import Array


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.ToggleSource source ->
            let
                newSources =
                    Dict.update source (Maybe.map not) model.sources
            in
                update (Msgs.SearchSubmit) { model | sources = newSources }

        Msgs.UpdateLicense license ->
            update (Msgs.SearchSubmit) { model | license = license }

        Msgs.UpdateQuery newQuery ->
            ( { model | query = newQuery }, Cmd.none )

        Msgs.UrlChange location ->
            let
                newRoute =
                    parseLocation location
            in
                case newRoute of
                    HomeRoute query license ->
                        case ( query, license ) of
                            ( Just query, Just license ) ->
                                ( { model
                                    | route = newRoute
                                    , query = query
                                    , license = license
                                  }
                                , searchImage query license model.sources
                                )

                            ( _, _ ) ->
                                ( { model | route = newRoute, query = "" }
                                , Task.attempt (always Msgs.NoOp) <|
                                    Dom.focus "searchInput"
                                )

                    _ ->
                        ( { model | route = newRoute }, Cmd.none )

        Msgs.KeyMsg code ->
            case ( model.route, model.viewing, model.photos ) of
                ( HomeRoute query _, Just index, RemoteData.Success photos ) ->
                    if code == 27 then
                        update (Msgs.StopViewing index) model
                    else
                        case code of
                            37 ->
                                update (Msgs.ViewPhoto (index - 1)) model

                            39 ->
                                update (Msgs.ViewPhoto (index + 1)) model

                            _ ->
                                ( model, Cmd.none )

                ( _, _, _ ) ->
                    ( model, Cmd.none )

        Msgs.FetchPhotos response ->
            ( { model | photos = response }, Cmd.none )

        Msgs.SearchSubmit ->
            -- Dont do anything if the query is empty
            if String.isEmpty model.query then
                ( model, Cmd.none )
            else
                ( model
                , newUrl ("/?q=" ++ model.query ++ "&license=" ++ model.license)
                )

        Msgs.ClearInput ->
            ( { model | query = "" }
            , Task.attempt (always Msgs.NoOp) <| Dom.focus "searchInput"
            )

        Msgs.ViewPhoto index ->
            let
                photos =
                    RemoteData.withDefault Array.empty model.photos
            in
                case Array.get index photos of
                    Just photo ->
                        ( { model | viewing = Just index }, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

        Msgs.StopViewing index ->
            ( { model | viewing = Nothing }, Cmd.none )

        _ ->
            ( model, Cmd.none )
