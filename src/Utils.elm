module Utils exposing (..)

import Array exposing (Array)
import Char


capitalize : Bool -> String -> String
capitalize shouldCapitalize str =
    case String.uncons str of
        Nothing ->
            str

        Just ( firstLetter, rest ) ->
            let
                newFirstLetter =
                    if shouldCapitalize then
                        Char.toUpper firstLetter
                    else
                        Char.toLower firstLetter
            in
                String.cons newFirstLetter rest


lastElem : Array a -> Maybe a
lastElem =
    Array.foldl (Just >> always) Nothing
