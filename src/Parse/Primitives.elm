module Parse.Primitives
    exposing
        ( Error
        , Parser
        , andThen
        , capVar
        , delayedCommit
        , delayedCommitMap
        , end
        , fail
        , inContext
        , keyword
        , lazy
        , lowVar
        , map
        , map2
        , number
        , oneOf
        , run
        , succeed
        , symbol
        )

import AST.Literal as L
import Char
import ParserPrimitives as Prim
import Reporting.Error.Syntax as E
    exposing
        ( Problem(..)
        , Theory(..)
        )
import Reporting.Region as R
import Set exposing (Set)


-- PARSER


type Parser a
    = Parser (State -> Step a)


type Step a
    = Good a State
    | Bad Problem State


type alias State =
    { source : String
    , offset : Int
    , indent : Int
    , context : E.ContextStack
    , row : Int
    , col : Int
    }


run : Parser a -> String -> Result Error a
run (Parser parse) source =
    let
        initialState =
            { source = source
            , offset = 0
            , indent = 1
            , context = []
            , row = 1
            , col = 1
            }
    in
    case parse initialState of
        Good a _ ->
            Ok a

        Bad problem { row, col, context } ->
            Err
                { row = row
                , col = col
                , source = source
                , problem = problem
                , context = context
                }



-- ERRORS


type alias Error =
    { row : Int
    , col : Int
    , source : String
    , problem : Problem
    , context : E.ContextStack
    }



-- PRIMITIVES


succeed : a -> Parser a
succeed a =
    Parser <| \state -> Good a state


fail : Problem -> Parser a
fail problem =
    Parser <| \state -> Bad problem state



-- MAPPING


map : (a -> b) -> Parser a -> Parser b
map func (Parser parse) =
    Parser <|
        \state1 ->
            case parse state1 of
                Good a state2 ->
                    Good (func a) state2

                Bad problem state2 ->
                    Bad problem state2


map2 : (a -> b -> value) -> Parser a -> Parser b -> Parser value
map2 func (Parser parseA) (Parser parseB) =
    Parser <|
        \state1 ->
            case parseA state1 of
                Bad problem state2 ->
                    Bad problem state2

                Good a state2 ->
                    case parseB state2 of
                        Bad problem state3 ->
                            Bad problem state3

                        Good b state3 ->
                            Good (func a b) state3



-- AND THEN


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen callback (Parser parseA) =
    Parser <|
        \state1 ->
            case parseA state1 of
                Bad problem state2 ->
                    Bad problem state2

                Good a state2 ->
                    let
                        (Parser parseB) =
                            callback a
                    in
                    parseB state2



-- LAZY


lazy : (() -> Parser a) -> Parser a
lazy thunk =
    Parser <|
        \state ->
            let
                (Parser parse) =
                    thunk ()
            in
            parse state



-- ONE OF


oneOf : List (Parser a) -> Parser a
oneOf parsers =
    Parser <| \state -> oneOfHelp state [] parsers


oneOfHelp : State -> List Problem -> List (Parser a) -> Step a
oneOfHelp state problems parsers =
    case parsers of
        [] ->
            Bad (List.foldr mergeProblems (Theories [] []) problems) state

        (Parser parse) :: remainingParsers ->
            case parse state of
                Good a state1 ->
                    Good a state1

                Bad problem ({ row, col } as context) ->
                    if state.row == row && state.col == col then
                        oneOfHelp state (problem :: problems) remainingParsers
                    else
                        Bad problem context


mergeProblems : Problem -> Problem -> Problem
mergeProblems p1 p2 =
    case ( p1, p2 ) of
        ( Theories _ [], Theories _ _ ) ->
            p2

        ( Theories _ _, Theories _ [] ) ->
            p1

        ( Theories ctx ts1, Theories _ ts2 ) ->
            Theories ctx (ts1 ++ ts2)

        ( Theories _ _, _ ) ->
            p2

        ( _, _ ) ->
            p1



-- DELAYED COMMIT


delayedCommit : Parser a -> Parser value -> Parser value
delayedCommit filler realStuff =
    delayedCommitMap (\_ v -> v) filler realStuff


delayedCommitMap : (a -> b -> value) -> Parser a -> Parser b -> Parser value
delayedCommitMap func (Parser parseA) (Parser parseB) =
    Parser <|
        \state1 ->
            case parseA state1 of
                Bad problem _ ->
                    Bad problem state1

                Good a state2 ->
                    case parseB state2 of
                        Good b state3 ->
                            Good (func a b) state3

                        Bad problem state3 ->
                            if state2.row == state3.row && state2.col == state3.col then
                                Bad problem state1
                            else
                                Bad problem state3



-- TOKENS


symbol : String -> Parser ()
symbol str =
    token (Symbol str) str


keyword : String -> Parser ()
keyword str =
    token (Keyword str) str


token : Theory -> String -> Parser ()
token problem str =
    Parser <|
        \({ source, offset, indent, context, row, col } as state) ->
            let
                ( newOffset, newRow, newCol ) =
                    Prim.isSubString str offset row col source
            in
            if newOffset == -1 then
                Bad (Theories context [ problem ]) state
            else
                Good ()
                    { source = source
                    , offset = newOffset
                    , indent = indent
                    , context = context
                    , row = newRow
                    , col = newCol
                    }



-- VARIABLES


lowVar : Parser String
lowVar =
    variable LowVar Char.isLower


capVar : Parser String
capVar =
    variable CapVar Char.isUpper


variable : Theory -> (Char -> Bool) -> Parser String
variable theory isFirst =
    Parser <|
        \({ source, offset, indent, context, row, col } as state1) ->
            let
                firstOffset =
                    Prim.isSubChar isFirst offset source
            in
            if firstOffset == -1 then
                Bad (Theories context [ theory ]) state1
            else
                let
                    state2 =
                        if firstOffset == -2 then
                            varHelp isAlphaNum (offset + 1) (row + 1) 1 source indent context
                        else
                            varHelp isAlphaNum firstOffset row (col + 1) source indent context

                    name =
                        String.slice offset state2.offset source
                in
                if Set.member name keywords then
                    Bad (Theories context [ theory ]) state1
                else
                    Good name state2


varHelp : (Char -> Bool) -> Int -> Int -> Int -> String -> Int -> E.ContextStack -> State
varHelp isGood offset row col source indent context =
    let
        newOffset =
            Prim.isSubChar isGood offset source
    in
    if newOffset == -1 then
        { source = source
        , offset = offset
        , indent = indent
        , context = context
        , row = row
        , col = col
        }
    else if newOffset == -2 then
        varHelp isGood (offset + 1) (row + 1) 1 source indent context
    else
        varHelp isGood newOffset row (col + 1) source indent context


isAlphaNum : Char -> Bool
isAlphaNum c =
    Char.isLower c || Char.isUpper c || Char.isDigit c || c == '_'


keywords : Set.Set String
keywords =
    Set.fromList
        [ "if"
        , "then"
        , "else"
        , "case"
        , "of"
        , "let"
        , "in"
        , "type"
        , "module"
        , "where"
        , "import"
        , "exposing"
        , "as"
        , "port"
        ]



-- NUMBER


type alias Offset a =
    Result ( Int, Problem ) ( Int, a )


number : Parser L.Literal
number =
    Parser <|
        \{ source, offset, indent, context, row, col } ->
            case intHelp offset (Prim.isSubChar isZero offset source) source of
                Err ( badOffset, problem ) ->
                    Bad problem
                        { source = source
                        , offset = badOffset
                        , indent = indent
                        , context = context
                        , row = row
                        , col = col + (badOffset - offset)
                        }

                Ok ( goodOffset, lit ) ->
                    Good lit
                        { source = source
                        , offset = goodOffset
                        , indent = indent
                        , context = context
                        , row = row
                        , col = col + (goodOffset - offset)
                        }


intHelp : Int -> Int -> String -> Offset L.Literal
intHelp offset zeroOffset source =
    if zeroOffset == -1 then
        floatHelp offset zeroOffset source
    else if Prim.isSubChar isDot zeroOffset source /= -1 then
        floatHelp offset zeroOffset source
    else if Prim.isSubChar isX zeroOffset source /= -1 then
        chompDigits Char.isHexDigit (String.slice offset) (offset + 2) source
            |> Result.map (\( n, f ) -> ( n, readInt (f n source) ))
    else if Prim.isSubChar isBadIntEnd zeroOffset source == -1 then
        Ok ( zeroOffset, readInt <| String.slice offset zeroOffset source )
    else
        Err ( zeroOffset, BadNumberEnd )


floatHelp : Int -> Int -> String -> Offset L.Literal
floatHelp offset zeroOffset source =
    let
        unwrap ( goodOffset, f ) =
            ( goodOffset, f (String.slice offset goodOffset source) )
    in
    if zeroOffset >= 0 then
        chompDotAndExp zeroOffset source |> Result.map unwrap
    else
        let
            dotOffset =
                chomp Char.isDigit offset source

            result =
                chompDotAndExp dotOffset source |> Result.map unwrap
        in
        case result of
            Err _ ->
                result

            Ok ( n, _ ) ->
                if n == offset then
                    Err ( n, BadNumberEnd )
                else
                    result


readInt : String -> L.Literal
readInt =
    readLiteral L.IntNum String.toInt


readFloat : String -> L.Literal
readFloat =
    readLiteral L.FloatNum String.toFloat


readLiteral : (a -> L.Literal) -> (String -> Result x a) -> String -> L.Literal
readLiteral toLit toResult str =
    case toResult str of
        Ok x ->
            toLit x

        Err _ ->
            Debug.crash <|
                "The number parser seems to have a bug.\n"
                    ++ "Please report an SSCCE to "
                    ++ "<https://github.com/hkgumbs/elm/issues>."



-- END


end : Parser ()
end =
    Parser <|
        \state ->
            if String.length state.source == state.offset then
                Good () state
            else
                Bad (Theories [] []) state



-- CONTEXT


inContext : E.Context -> Parser a -> Parser a
inContext ctx (Parser parse) =
    Parser <|
        \({ context, row, col } as initialState) ->
            let
                state1 =
                    changeContext (( ctx, R.Position row col ) :: context) initialState
            in
            case parse state1 of
                Good a state2 ->
                    Good a (changeContext context state2)

                (Bad _ _) as step ->
                    step


changeContext : E.ContextStack -> State -> State
changeContext newContext { source, offset, indent, row, col } =
    { source = source
    , offset = offset
    , indent = indent
    , context = newContext
    , row = row
    , col = col
    }



-- CHOMPERS


chomp : (Char -> Bool) -> Int -> String -> Int
chomp isGood offset source =
    let
        newOffset =
            Prim.isSubChar isGood offset source
    in
    if newOffset < 0 then
        offset
    else
        chomp isGood newOffset source



-- CHOMP DIGITS


chompDigits : (Char -> Bool) -> a -> Int -> String -> Offset a
chompDigits isValidDigit goodValue offset source =
    let
        newOffset =
            chomp isValidDigit offset source
    in
    if newOffset == offset then
        -- no digits
        Err ( newOffset, BadNumberEnd )
    else if Prim.isSubChar isBadIntEnd newOffset source /= -1 then
        -- ends with non-digit characters
        Err ( newOffset, BadNumberEnd )
    else
        -- all valid digits!
        Ok ( newOffset, goodValue )


isBadIntEnd : Char -> Bool
isBadIntEnd char =
    Char.isDigit char || Char.isUpper char || Char.isLower char || isDot char



-- CHOMP FLOAT STUFF


chompDotAndExp : Int -> String -> Offset (String -> L.Literal)
chompDotAndExp offset source =
    let
        dotOffset =
            Prim.isSubChar isDot offset source
    in
    if dotOffset == -1 then
        chompExp True offset source
    else
        chompExp False (chomp Char.isDigit dotOffset source) source


chompExp : Bool -> Int -> String -> Offset (String -> L.Literal)
chompExp maybeInt offset source =
    let
        eOffset =
            Prim.isSubChar isE offset source
    in
    if eOffset == -1 then
        Ok
            ( offset
            , if maybeInt then
                readInt
              else
                readFloat
            )
    else
        let
            opOffset =
                Prim.isSubChar isPlusOrMinus eOffset source

            expOffset =
                if opOffset == -1 then
                    eOffset
                else
                    opOffset
        in
        if Prim.isSubChar isZero expOffset source /= -1 then
            Err ( expOffset, BadNumberExp )
        else if Prim.isSubChar Char.isDigit expOffset source == -1 then
            Err ( expOffset, BadNumberExp )
        else
            chompDigits Char.isDigit readFloat expOffset source


isDot : Char -> Bool
isDot char =
    char == '.'


isX : Char -> Bool
isX char =
    char == 'x'


isE : Char -> Bool
isE char =
    char == 'e' || char == 'E'


isZero : Char -> Bool
isZero char =
    char == '0'


isPlusOrMinus : Char -> Bool
isPlusOrMinus char =
    char == '+' || char == '-'
