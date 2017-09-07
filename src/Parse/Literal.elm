module Parse.Literal exposing (literal)

import AST.Literal as L
import Parse.Primitives as P


literal : P.Parser L.Literal
literal =
    P.oneOf
        [ P.map L.Str P.string
        , P.map L.Chr P.character
        , P.number
        ]
