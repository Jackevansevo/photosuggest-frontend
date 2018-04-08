module Commands exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder, bool, list, string, at, nullable)
import Models exposing (Model)
import Json.Decode.Pipeline exposing (decode, optional, required)
import Models exposing (Photo, License, Params)
import Msgs exposing (Msg)
import RemoteData
import Set as S
import Dict


asParam : ( String, S.Set String ) -> String
asParam ( key, values ) =
    if S.isEmpty values then
        ""
    else
        String.join "&" (S.toList (S.map (\v -> key ++ "=" ++ v) values))


buildParams : Params -> String
buildParams params =
    if not (Dict.isEmpty params) then
        "?" ++ String.join "&" (List.map asParam (Dict.toList params))
    else
        ""


searchImage : Model -> Cmd Msg
searchImage model =
    let
        params =
            Dict.singleton "q" (S.singleton model.query)
                |> Dict.insert "license" (S.singleton model.license)
                |> Dict.insert "sources" model.sources

        url =
            "http://localhost:8080/" ++ (buildParams params)

        _ =
            Debug.log "url" url
    in
        Http.get url photosDecoder
            |> RemoteData.sendRequest
            |> Cmd.map Msgs.FetchPhotos


photosDecoder : Decoder (List Photo)
photosDecoder =
    at [ "results" ] (list photoDecoder)


photoDecoder : Decoder Photo
photoDecoder =
    decode Photo
        |> required "url" string
        |> required "source" string
        |> optional "owner" (Decode.map Just string) Nothing
        |> optional "description" (Decode.map Just string) Nothing
        |> optional "license" (Decode.map Just licenseDecoder) Nothing


licenseDecoder : Decoder License
licenseDecoder =
    decode License
        |> required "name" string
        |> required "url" string
