module Parse.Pattern exposing (expression, term)

import AST.Literal as L
import AST.Pattern as P
import AST.Variable as Var
import Parse.Helpers exposing (..)
import Parse.Literal as Literal
import Parse.Primitives exposing (..)
import Reporting.Annotation as A
import Reporting.Error.Syntax as E
import Reporting.Region as R


-- PATTERN TERMS


term : Parser P.Raw
term =
    hint E.Pattern <|
        andThen termHelp getPosition


termHelp : R.Position -> Parser P.Raw
termHelp start =
    oneOf
        [ record start
        , tuple start
        , list start
        , succeed (,,)
            |= oneOf
                [ map (\_ -> P.Anything) underscore
                , map P.Var lowVar
                , map mkCtor qualifiedCapVar
                , map P.Literal Literal.literal
                ]
            |= getPosition
            |> map (\( pattern, end ) -> A.At start end pattern)
        ]


mkCtor : String -> P.Raw_
mkCtor ctor =
    case ctor of
        "True" ->
            P.Literal (L.Boolean True)

        "False" ->
            P.Literal (L.Boolean False)

        _ ->
            P.Ctor (Var.Raw ctor) []
