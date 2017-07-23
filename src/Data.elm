module Data exposing (..)


maybe : b -> (a -> b) -> Maybe a -> b
maybe default f maybe =
    case maybe of
        Just a ->
            f a

        Nothing ->
            default
