module Parse.Type exposing (expression, unionConstructor)

import AST.Type as Type
import AST.Variable as Var
import Parse.Helpers exposing (..)
import Parse.Primitives exposing (..)
import Reporting.Annotation as A
import Reporting.Error.Syntax as E
import Reporting.Region as R


-- TYPE TERMS


term : Parser Type.Raw
term =
    let
        termHelp start =
            oneOf
                [ app0
                , variable start
                , tuple start
                , record start
                ]
    in
    hint E.Type <|
        andThen termHelp getPosition



-- TYPE VARIABLES


variable : R.Position -> Parser Type.Raw
variable start =
    map2 (\var end -> A.at start end (Type.RVar var)) lowVar getPosition



-- TYPE EXPRESSIONS


expression : SParser Type.Raw
expression =
    let
        expressionBegin start =
            oneOf
                [ app start
                , succeed (,,) |= term |= getPosition |= whitespace
                ]
                |> andThen (expressionEnd start)

        expressionEnd start ( tipe1, end1, pos1 ) =
            oneOf
                [ succeed identity
                    |. checkSpace pos1
                    |. rightArrow
                    |. spaces
                    |= expression
                    |> map
                        (\( tipe2, end2, pos2 ) ->
                            let
                                tipe =
                                    A.at start end2 (Type.RLambda tipe1 tipe2)
                            in
                            ( tipe, end2, pos2 )
                        )
                , succeed ( tipe1, end1, pos1 )
                ]
    in
    hint E.Type <| andThen expressionBegin getPosition



-- TYPE CONSTRUCTORS


app0 : Parser Type.Raw
app0 =
    let
        app0Help start name end =
            A.at start end (Type.RType (A.at start end (Var.Raw name)) [])
    in
    succeed app0Help
        |= getPosition
        |= qualifiedCapVar
        |= getPosition


app : R.Position -> SParser Type.Raw
app start =
    qualifiedCapVar
        |> andThen
            (\ctor ->
                getPosition
                    |> andThen
                        (\ctorEnd ->
                            let
                                name =
                                    A.at start ctorEnd (Var.Raw ctor)
                            in
                            whitespace
                                |> andThen
                                    (\ctorPos ->
                                        eatArgs [] ctorEnd ctorPos
                                            |> map
                                                (\( args, end, pos ) ->
                                                    let
                                                        tipe =
                                                            A.at start end (Type.RType name args)
                                                    in
                                                    ( tipe, end, pos )
                                                )
                                    )
                        )
            )


unionConstructor : SParser ( String, List Type.Raw )
unionConstructor =
    capVar
        |> andThen
            (\ctor ->
                getPosition
                    |> andThen
                        (\ctorEnd ->
                            whitespace
                                |> andThen
                                    (\ctorSpace ->
                                        eatArgs [] ctorEnd ctorSpace
                                            |> map (\( args, end, space ) -> ( ( ctor, args ), end, space ))
                                    )
                        )
            )


eatArgs : List Type.Raw -> R.Position -> SPos -> SParser (List Type.Raw)
eatArgs args end pos =
    oneOf
        [ succeed (\arg -> eatArgs (arg :: args))
            |. checkSpace pos
            |= term
            |= getPosition
            |= whitespace
        , succeed ( List.reverse args, end, pos )
        ]



-- TUPLES


tuple : R.Position -> Parser Type.Raw
tuple start =
    map2 (\_ x -> x) leftParen <|
        inContext start E.TypeTuple <|
            oneOf
                [ succeed (\end -> Type.tuple (R.Region start end) [])
                    |. rightParen
                    |= getPosition
                , succeed identity
                    |. spaces
                    |= expression
                    |> andThen
                        (\( tipe, _, pos ) ->
                            map2 (\_ x -> x) (checkSpace pos) (tupleEnding start [ tipe ])
                        )
                ]


tupleEnding : R.Position -> List Type.Raw -> Parser Type.Raw
tupleEnding start tipes =
    oneOf
        [ succeed identity
            |. comma
            |. spaces
            |= andThen (\( tipe, _, pos ) -> map2 (\_ x -> x) (checkSpace pos) (succeed tipe)) expression
            |> andThen (\tipe -> tupleEnding start (tipe :: tipes))
        , succeed identity
            |. rightParen
            |= getPosition
            |> map
                (\end ->
                    case List.reverse tipes of
                        [ tipe ] ->
                            tipe

                        tupleTypes ->
                            Type.tuple (R.Region start end) tupleTypes
                )
        ]



-- RECORD


record : R.Position -> Parser Type.Raw
record start =
    leftCurly
        |> andThen
            (\() ->
                spaces
                    |> andThen
                        (\() ->
                            oneOf
                                [ succeed (\end -> A.at start end (Type.RRecord [] Nothing))
                                    |. rightCurly
                                    |= getPosition
                                , addLocation lowVar
                                    |. spaces
                                    |> andThen
                                        (\var ->
                                            oneOf
                                                [ succeed identity
                                                    |. pipe
                                                    |. spaces
                                                    |= field
                                                    |> andThen (\firstField -> chompFields [ firstField ])
                                                    |> map (,)
                                                    |= getPosition
                                                    |> map (\( fields, end ) -> A.at start end (Type.RRecord fields (Just (A.map Type.RVar var))))
                                                , succeed identity
                                                    |. hasType
                                                    |. spaces
                                                    |= expression
                                                    |> andThen
                                                        (\( tipe, _, nextPos ) ->
                                                            succeed (,)
                                                                |. checkSpace nextPos
                                                                |= chompFields [ ( var, tipe ) ]
                                                                |= getPosition
                                                                |> map (\( fields, end ) -> A.at start end (Type.RRecord fields Nothing))
                                                        )
                                                ]
                                        )
                                ]
                        )
                    |> inContext start E.TypeRecord
            )


type alias Field =
    ( A.Located String, Type.Raw )


chompFields : List Field -> Parser (List Field)
chompFields fields =
    oneOf
        [ succeed (\f -> f :: fields)
            |. comma
            |. spaces
            |= field
            |> andThen chompFields
        , succeed (List.reverse fields) |. rightCurly
        ]


field : Parser Field
field =
    addLocation lowVar
        |. spaces
        |. hasType
        |. spaces
        |> andThen
            (\name ->
                expression
                    |> andThen
                        (\( tipe, _, endPos ) ->
                            succeed ( name, tipe ) |. checkSpace endPos
                        )
            )
