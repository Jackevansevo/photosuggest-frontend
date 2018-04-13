module Commands exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder, bool, array, string, at, nullable)
import Models exposing (Model)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Models exposing (License, Param, Params, Photo, PhotoLicense, Query, Sources)
import Msgs exposing (Msg)
import RemoteData
import Dict
import Array exposing (Array)


asParam : ( String, Param ) -> String
asParam ( key, values ) =
    if List.isEmpty values then
        ""
    else
        String.join "&" (List.map (\v -> key ++ "=" ++ v) values)


buildParams : Dict.Dict String (List String) -> String
buildParams params =
    if not (Dict.isEmpty params) then
        "?" ++ String.join "&" (List.map asParam (Dict.toList params))
    else
        ""


buildUrl : Query -> License -> Sources -> String
buildUrl query license sources =
    let
        params =
            Dict.singleton "q" (List.singleton query)
                |> Dict.insert "license" (List.singleton license)
                |> Dict.insert "sources"
                    (Dict.keys (Dict.filter (\_ v -> v) sources))
    in
        "http://localhost:8080/" ++ (buildParams params)


searchImage : String -> String -> Sources -> Cmd Msg
searchImage query license sources =
    let
        url =
            buildUrl query license sources

        _ =
            Debug.log "url" url
    in
        Http.get url photosDecoder
            |> RemoteData.sendRequest
            |> Cmd.map Msgs.FetchPhotos


photosDecoder : Decoder (Array Photo)
photosDecoder =
    at [ "results" ] (array photoDecoder)


photoDecoder : Decoder Photo
photoDecoder =
    decode Photo
        |> required "url" string
        |> required "thumbnail" string
        |> required "origin" string
        |> required "source" string
        |> optional "owner" (Decode.map Just string) Nothing
        |> optional "title" (Decode.map Just string) Nothing
        |> optional "description" (Decode.map Just string) Nothing
        |> optional "license" (Decode.map Just licenseDecoder) Nothing


licenseDecoder : Decoder PhotoLicense
licenseDecoder =
    decode PhotoLicense
        |> required "name" string
        |> required "url" string
