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
        Msgs.ToggleFilter ->
            ( { model | filtered = (not model.filtered) }, Cmd.none )

        Msgs.ToggleSource source ->
            let
                newSources =
                    Dict.update source (Maybe.map not) model.sources
            in
                ( { model | sources = newSources }, Cmd.none )

        Msgs.UpdateLicense license ->
            ( { model | license = license }, newUrl ("/?q=" ++ model.query ++ "&license=" ++ license) )

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
                                let
                                    newModel =
                                        { model
                                            | route = newRoute
                                            , license = license
                                            , query = query
                                            , photos = Loading
                                            , previousSearch = query
                                            , viewing = Nothing
                                        }
                                in
                                    ( newModel, searchImage query model.license model.sources )

                            ( _, _ ) ->
                                ( { model
                                    | route = newRoute
                                    , query = ""
                                    , photos = Loading
                                    , viewing = Nothing
                                  }
                                , Task.attempt (always Msgs.NoOp) <| Dom.focus "searchInput"
                                )

                    _ ->
                        -- Update the route by default
                        ( { model | route = newRoute }, Cmd.none )

        Msgs.KeyMsg code ->
            case ( model.route, model.viewing, model.photos ) of
                ( HomeRoute query _, Just index, RemoteData.Success photos ) ->
                    if code == 27 then
                        -- Escape Key
                        ( { model | viewing = Nothing }, Cmd.none )
                    else
                        let
                            newIndex =
                                case code of
                                    37 ->
                                        index - 1

                                    39 ->
                                        index + 1

                                    _ ->
                                        index
                        in
                            let
                                nextPhoto =
                                    Array.get newIndex photos
                            in
                                case nextPhoto of
                                    Just photo ->
                                        ( { model | viewing = Just newIndex }, Cmd.none )

                                    Nothing ->
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
                ( model, newUrl ("/?q=" ++ model.query ++ "&license=" ++ model.license) )

        Msgs.ClearInput ->
            ( { model | query = "" }, Task.attempt (always Msgs.NoOp) <| Dom.focus "searchInput" )

        Msgs.ViewPhoto index ->
            ( { model | viewing = Just index }, Cmd.none )

        Msgs.StopViewing ->
            ( { model | viewing = Nothing }, Cmd.none )

        _ ->
            ( model, Cmd.none )
