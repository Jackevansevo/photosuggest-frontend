module Main exposing (..)

import View exposing (view)
import Msgs exposing (..)
import Models exposing (Model, initialModel, Route(..))
import Update exposing (update)
import Commands exposing (searchImage)
import Navigation exposing (Location)
import Keyboard
import Dom
import Routing
import Task


init : Location -> ( Model, Cmd Msg )
init location =
    let
        initalRoute =
            Routing.parseLocation location
    in
        case initalRoute of
            HomeRoute query license ->
                case query of
                    Just query ->
                        let
                            startModel =
                                initialModel initalRoute

                            newModel =
                                { startModel | query = query, previousSearch = query }
                        in
                            ( newModel, searchImage newModel )

                    Nothing ->
                        ( initialModel initalRoute
                        , Task.attempt (always Msgs.NoOp) <| Dom.focus "searchInput"
                        )

            _ ->
                ( initialModel initalRoute, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg ]


main : Program Never Model Msg
main =
    Navigation.program Msgs.UrlChange
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
