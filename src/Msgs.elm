module Msgs exposing (..)

import Models exposing (Photo)
import Navigation exposing (Location)
import RemoteData exposing (WebData)
import Keyboard
import Dom exposing (..)


type Msg
    = UpdateQuery String
    | UrlChange Location
    | SearchSubmit
    | FetchPhotos (WebData (List Photo))
    | ImageLoaded String
    | FocusOn String
    | FocusResult (Result Dom.Error ())
    | ViewPhoto Photo
    | ClearInput
    | KeyMsg Keyboard.KeyCode
    | NoOp
    | StopViewing
    | UpdateLicense String
    | ToggleSource String
    | ToggleFilter
