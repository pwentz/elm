module Prelude exposing (..)


type alias Map a b =
    List ( a, b )


lookup : a -> Map a b -> Maybe b
lookup key list =
    case list of
        ( a, b ) :: rest ->
            if key == a then
                Just b
            else
                lookup key rest

        [] ->
            Nothing


maybe : b -> (a -> b) -> Maybe a -> b
maybe default f maybe =
    case maybe of
        Just a ->
            f a

        Nothing ->
            default
