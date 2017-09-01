module Parse.Primitives
    exposing
        ( Error
        , Parser
        , SPos(..)
        , andThen
        , capVar
        , deadend
        , delayedCommit
        , delayedCommitMap
        , end
        , fail
        , getIndent
        , getPosition
        , hint
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
        , whitespace
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


badParse : Int
badParse =
    -1


deadend : List Theory -> Parser a
deadend theories =
    Parser <|
        \({ context } as state) ->
            Bad (Theories context theories) state


hint : E.Next -> Parser a -> Parser a
hint next (Parser parse) =
    Parser <|
        \state ->
            case parse state of
                Bad problem ({ context } as state1) ->
                    Bad (Theories context [ Expecting next ]) state1

                (Good _ _) as step ->
                    step



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
            if newOffset == badParse then
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
            if firstOffset == badParse then
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
    if newOffset == badParse then
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


number : Parser L.Literal
number =
    Parser <|
        \{ source, offset, indent, context, row, col } ->
            let
                zeroOffset =
                    Prim.isSubChar (is '0') offset source

                chompResults =
                    if zeroOffset == badParse then
                        chompInt offset source
                    else
                        chompZero offset zeroOffset source
            in
            case chompResults of
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



-- WHITESPACE


type SPos
    = SPos R.Position


whitespace : Parser SPos
whitespace =
    Parser <|
        \state ->
            let
                ( maybeProblem, newOffset, newRow, newCol ) =
                    whitespaceHelp state.offset state.row state.col state.source

                newState =
                    { state | offset = newOffset, row = newRow, col = newCol }
            in
            case maybeProblem of
                Just problem ->
                    Bad problem newState

                Nothing ->
                    Good (SPos (R.Position newRow newCol)) newState


whitespaceHelp : Int -> Int -> Int -> String -> ( Maybe Problem, Int, Int, Int )
whitespaceHelp offset row col source =
    let
        newOffset =
            chomp isSpace offset source
    in
    if nextChar (is '\n') newOffset source then
        whitespaceHelp (newOffset + 1) (row + 1) 1 source
        {-
           else if nextChar (is '-') newOffset source then
               lineComment (newOffset + 1) row (col + 1) source
           else if nextChar (is '{') newOffset source then
               multiComment (newOffset + 1) row (col + 1) source
        -}
    else if nextChar (is '\t') newOffset source then
        ( Just Tab, newOffset, row, col )
    else
        ( Nothing, newOffset, row, col + newOffset - offset )



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



-- STATE


getPosition : Parser R.Position
getPosition =
    Parser <|
        \({ row, col } as state) ->
            Good (R.Position row col) state


getIndent : Parser Int
getIndent =
    Parser <|
        \({ indent } as state) ->
            Good indent state



-- CHOMP


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


nextChar : (Char -> Bool) -> Int -> String -> Bool
nextChar isGood offset source =
    Prim.isSubChar isGood offset source /= badParse



-- CHOMP NUMBER STUFF


type alias Number =
    Result ( Int, Problem ) ( Int, L.Literal )


chompInt : Int -> String -> Number
chompInt offset source =
    let
        stopOffset =
            chomp Char.isDigit offset source
    in
    if offset == stopOffset then
        Err ( stopOffset, Theories [] [] )
    else if nextChar (is '.') stopOffset source then
        chompFraction offset (stopOffset + 1) source
    else if nextChar isE stopOffset source then
        chompExponent offset (stopOffset + 1) source
    else if nextChar isBadIntEnd stopOffset source then
        Err ( stopOffset, BadNumberEnd )
    else
        Ok ( stopOffset, readInt offset stopOffset source )


chompZero : Int -> Int -> String -> Number
chompZero offset zeroOffset source =
    if nextChar (is '.') zeroOffset source then
        chompFraction offset (zeroOffset + 1) source
    else if nextChar (is 'x') zeroOffset source then
        chompHex offset (zeroOffset + 1) source
    else if nextChar Char.isDigit zeroOffset source then
        Err ( zeroOffset, BadNumberZero )
    else
        Ok ( zeroOffset, L.IntNum 0 )


chompFraction : Int -> Int -> String -> Number
chompFraction offset dotOffset source =
    let
        stopOffset =
            chomp Char.isDigit dotOffset source
    in
    if dotOffset == stopOffset then
        Err ( stopOffset, BadNumberDot )
    else if nextChar isE stopOffset source then
        chompExponent offset (stopOffset + 1) source
    else if nextChar isBadIntEnd stopOffset source then
        Err ( stopOffset, BadNumberEnd )
    else
        Ok ( stopOffset, readFloat offset stopOffset source )


chompExponent : Int -> Int -> String -> Number
chompExponent offset eOffset source =
    let
        expOffset =
            if nextChar isPlusOrMinus eOffset source then
                eOffset + 1
            else
                eOffset

        newOffset =
            chomp Char.isDigit expOffset source
    in
    if newOffset == expOffset then
        Err ( newOffset, BadNumberExp )
    else if nextChar isBadIntEnd newOffset source then
        Err ( newOffset, BadNumberExp )
    else
        Ok ( newOffset, readFloat offset newOffset source )


chompHex : Int -> Int -> String -> Number
chompHex offset xOffset source =
    let
        newOffset =
            chomp Char.isHexDigit xOffset source
    in
    if newOffset == xOffset then
        Err ( newOffset, BadNumberHex )
    else if nextChar isBadIntEnd newOffset source then
        Err ( newOffset, BadNumberHex )
    else
        Ok ( newOffset, readInt offset newOffset source )



-- CHARACTER CHOMP HELPERS


is : a -> a -> Bool
is target =
    \char -> char == target


isE : Char -> Bool
isE char =
    char == 'e' || char == 'E'


isPlusOrMinus : Char -> Bool
isPlusOrMinus char =
    char == '+' || char == '-'


isBadIntEnd : Char -> Bool
isBadIntEnd char =
    char == '.' || isAlphaNum char


isSpace : Char -> Bool
isSpace char =
    char == ' ' || char == '\x0D'


isAlphaNum : Char -> Bool
isAlphaNum char =
    Char.isLower char || Char.isUpper char || Char.isDigit char || char == '_'



-- LITERAL CHOMP HELPERS


readInt : Int -> Int -> String -> L.Literal
readInt start end source =
    readLiteral L.IntNum String.toInt (String.slice start end source)


readFloat : Int -> Int -> String -> L.Literal
readFloat start end source =
    readLiteral L.FloatNum String.toFloat (String.slice start end source)


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
