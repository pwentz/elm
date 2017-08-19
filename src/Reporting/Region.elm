module Reporting.Region exposing (..)

import Json.Encode as Json


-- REGION


type Region
    = Region Position Position


type Position
    = Position Int Int


merge : Region -> Region -> Region
merge (Region start _) (Region _ end) =
    Region start end



-- TO STRING


toString : Region -> String
toString (Region (Position startLine startColumn) (Position endLine endColumn)) =
    case startLine == endLine of
        False ->
            "between lines "
                ++ Basics.toString startLine
                ++ " and "
                ++ Basics.toString endLine

        True ->
            "on line "
                ++ Basics.toString endLine
                ++ ", column "
                ++ Basics.toString startColumn
                ++ " to "
                ++ Basics.toString endColumn



-- JSON


encode : Region -> Json.Value
encode (Region start end) =
    Json.object
        [ ( "start", encodePosition start )
        , ( "end", encodePosition end )
        ]


encodePosition : Position -> Json.Value
encodePosition (Position line column) =
    Json.object
        [ ( "line", Json.int line )
        , ( "column", Json.int column )
        ]
