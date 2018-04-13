module Msgs exposing (..)

import Models exposing (Photo)
import Navigation exposing (Location)
import RemoteData exposing (WebData)
import Keyboard
import Dom exposing (..)
import Array exposing (Array)


type Msg
    = UpdateQuery String
    | UrlChange Location
    | SearchSubmit
    | FetchPhotos (WebData (Array Photo))
    | ImageLoaded String
    | FocusOn String
    | FocusResult (Result Dom.Error ())
    | ViewPhoto Int
    | ClearInput
    | KeyMsg Keyboard.KeyCode
    | NoOp
    | StopViewing
    | UpdateLicense String
    | ToggleSource String
    | ToggleFilter
