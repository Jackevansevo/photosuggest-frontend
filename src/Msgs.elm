module Msgs exposing (..)

import Models exposing (Photo)
import Navigation exposing (Location)
import RemoteData exposing (WebData)
import Keyboard
import Array exposing (Array)


type Msg
    = UpdateQuery String
    | UrlChange Location
    | SearchSubmit
    | FetchPhotos (WebData (Array Photo))
    | ViewPhoto Int
    | ClearInput
    | KeyMsg Keyboard.KeyCode
    | NoOp
    | StopViewing Int
    | UpdateLicense String
    | ToggleSource String
