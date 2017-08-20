module Parse.Helpers
    exposing
        ( Parser
        , SParser
        , SPos(..)
        , capVar
        , comma
        , dot
        , getPosition
        , hasType
          --, hint
        , leftCurly
        , leftParen
        , lowVar
          -- , oneOf
        , pipe
        , qualifiedCapVar
        , qualifiedVar
        , rightArrow
        , rightCurly
        , rightParen
        )

import AST.Helpers as Help exposing (isSymbol)
import AST.Literal as L
import Char
import Parser as P
import Parser.LowLevel as P
import Reporting.Annotation as A
import Reporting.Error.Syntax as E
    exposing
        ( BadOp(..)
        , ParseError(..)
        , Problem(..)
        , Theory(..)
        )
import Reporting.Region as R
import Set


-- PARSER


type alias Parser a =
    P.Parser ( E.Context, R.Position ) Problem a


deadend : List Theory -> Parser a
deadend theory =
    -- TODO
    P.fail (Theories [] [])



-- SYMBOLS


dot : Parser ()
dot =
    symbol "."


comma : Parser ()
comma =
    symbol ","


pipe : Parser ()
pipe =
    symbol "|"


hasType : Parser ()
hasType =
    symbol ":"


rightArrow : Parser ()
rightArrow =
    symbol "->"


leftParen : Parser ()
leftParen =
    symbol "("


rightParen : Parser ()
rightParen =
    symbol ")"


leftCurly : Parser ()
leftCurly =
    symbol "{"


rightCurly : Parser ()
rightCurly =
    symbol "}"


symbol : String -> Parser ()
symbol str =
    P.mapError
        (\e -> Theories e.context [ Symbol e.problem ])
        (P.token str)



-- KEYWORDS
-- VARIABLES


lowVar : Parser String
lowVar =
    variable Char.isLower LowVar


capVar : Parser String
capVar =
    variable Char.isUpper CapVar


qualifiedVar : Parser String
qualifiedVar =
    P.oneOf
        [ lowVar
        , P.andThen (\var -> qualifiedVarHelp lowVar [ var ]) capVar
        ]


qualifiedCapVar : Parser String
qualifiedCapVar =
    P.andThen (\var -> qualifiedVarHelp (deadend [ E.CapVar ]) [ var ]) capVar


qualifiedVarHelp : Parser String -> List String -> Parser String
qualifiedVarHelp altEnding vars =
    P.oneOf
        [ let
            step () =
                P.oneOf
                    [ P.andThen (\var -> qualifiedVarHelp altEnding (var :: vars)) capVar
                    , P.map (\var -> String.join "." (List.reverse (var :: vars))) altEnding
                    ]
          in
          P.andThen step dot
        , P.succeed (String.join "." (List.reverse vars))
        ]


variable : (Char -> Bool) -> Theory -> Parser String
variable isGoodStart theory =
    P.mapError
        (\{ context } -> Theories context [ theory ])
        (P.variable isGoodStart isAlphaNum keywords)



-- WHITESPACE


type alias SParser a =
    Parser ( a, R.Position, SPos )


type SPos
    = SPos R.Position



-- STATE


getPosition : Parser R.Position
getPosition =
    P.map (\( x, y ) -> R.Position x y) P.getPosition
