module Parse.Primitives
    exposing
        ( Parser
        , SPos(..)
        , andThen
        , capVar
        , character
        , deadend
        , delayedCommit
        , delayedCommitMap
        , docComment
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
        , string
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


run : Parser a -> String -> Result E.ParseError a
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
            Err (E.ParseError row col problem)



-- ERRORS


badParse : Int
badParse =
    -1


newLineParse : Int
newLineParse =
    -2


noError : Problem
noError =
    Theories [] []


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
            Bad (List.foldr mergeProblems noError problems) state

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
                        if firstOffset == newLineParse then
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
    else if newOffset == newLineParse then
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
    if atNewLine newOffset source then
        whitespaceHelp (newOffset + 1) (row + 1) 1 source
    else if nextStringIs "--" newOffset source then
        whitespaceHelp (chomp isAny (newOffset + 2) source) row col source
    else if
        nextStringIs "{-" newOffset source
            && not (nextCharIs '|' (newOffset + 2) source)
    then
        let
            (( maybeProblem, endOffset, endRow, endCol ) as result) =
                chompMultiComment (newOffset + 2) row (col + 2) 1 source
        in
        if maybeProblem == Nothing then
            whitespaceHelp endOffset endRow endCol source
        else
            result
    else if nextCharIs '\t' newOffset source then
        ( Just Tab, newOffset, row, col )
    else
        ( Nothing, newOffset, row, col + newOffset - offset )



-- DOCUMENTATION COMMENT


docComment : Parser String
docComment =
    let
        parse ({ offset, row, col, source } as state) =
            let
                ( maybeProblem, newOffset, newRow, newCol ) =
                    chompMultiComment offset row col 1 source

                newState =
                    { state | offset = newOffset, row = newRow, col = newCol }
            in
            case maybeProblem of
                Just problem ->
                    Bad problem newState

                Nothing ->
                    Good (String.slice offset (newOffset - 2) source) newState
    in
    symbol "{-|"
        |> andThen (\_ -> Parser parse)



-- STRINGS


string : Parser String
string =
    Parser <|
        \({ offset, col, row, source } as state) ->
            if not (nextCharIs '"' offset source) then
                Bad noError state
            else
                let
                    ( result, newOffset, newRow, newCol ) =
                        if nextStringIs "\"\"" (offset + 1) source then
                            multiString (offset + 3) row (col + 3) [] source
                        else
                            singleString (offset + 1) row (col + 1) [] source

                    newState =
                        { state | offset = newOffset, row = newRow, col = newCol }
                in
                case result of
                    Err problem ->
                        Bad problem newState

                    Ok str ->
                        Good str newState


singleString :
    Int
    -> Int
    -> Int
    -> List String
    -> String
    -> ( Result Problem String, Int, Int, Int )
singleString offset row col acc source =
    if nextCharIs '"' offset source then
        ( Ok (String.concat (List.reverse acc)), offset + 1, row, col + 1 )
    else if atEnd offset source then
        ( Err EndOfFile_String, offset, row, col )
    else if atNewLine offset source then
        ( Err NewLineInString, offset, row, col )
    else if nextCharIs '\\' offset source then
        case chompEscape (offset + 1) source EndOfFile_String of
            Ok ( diff, escapeCode ) ->
                singleString
                    (offset + diff)
                    row
                    (col + diff)
                    (String.fromChar escapeCode :: acc)
                    source

            Err problem ->
                ( Err problem, offset, row, col )
    else
        singleString
            (offset + 1)
            row
            (col + 1)
            (String.slice offset (offset + 1) source :: acc)
            source


multiString :
    Int
    -> Int
    -> Int
    -> List String
    -> String
    -> ( Result Problem String, Int, Int, Int )
multiString offset row col acc source =
    if nextStringIs "\"\"\"" offset source then
        ( Ok (String.concat (List.reverse acc)), offset + 3, row, col + 3 )
    else if atEnd offset source then
        ( Err EndOfFile_String, offset, row, col )
    else if atNewLine offset source then
        multiString (offset + 1) (row + 1) 1 ("\n" :: acc) source
    else if nextCharIs '\\' offset source then
        case chompEscape (offset + 1) source EndOfFile_String of
            Ok ( diff, escapeCode ) ->
                multiString
                    (offset + diff)
                    row
                    (col + diff)
                    (String.fromChar escapeCode :: acc)
                    source

            Err problem ->
                ( Err problem, offset, row, col )
    else
        multiString
            (offset + 1)
            row
            (col + 1)
            (String.slice offset (offset + 1) source :: acc)
            source



-- CHARACTER


character : Parser Char
character =
    Parser <|
        \({ offset, col, source } as state) ->
            if not (nextCharIs '\'' offset source) then
                Bad noError state
            else if
                nextCharIs '\'' (offset + 1) source
                    || atNewLine (offset + 1) source
            then
                Bad BadChar { state | offset = offset + 1, col = col + 1 }
            else if nextCharIs '\\' (offset + 1) source then
                characterEscape state
            else if nextCharIs '\'' (offset + 2) source then
                characterEnd state
            else
                Bad BadChar { state | offset = offset + 2, col = col + 2 }


characterEscape : State -> Step Char
characterEscape ({ offset, col, source } as state) =
    case chompEscape (offset + 2) source BadChar of
        Ok ( diff, escapeCode ) ->
            let
                newState =
                    { state | offset = offset + diff + 1, col = col + diff + 1 }
            in
            if nextCharIs '\'' (offset + diff + 1) source then
                Good escapeCode newState
            else
                Bad BadChar newState

        Err problem ->
            Bad problem { state | offset = offset + 1, col = col + 1 }


characterEnd : State -> Step Char
characterEnd ({ offset, col, source } as state) =
    case String.uncons (String.slice (offset + 1) (offset + 2) source) of
        Just ( c, _ ) ->
            Good c { state | offset = offset + 3, col = col + 3 }

        Nothing ->
            Debug.crash <|
                "The character parser seems to have a bug.\n"
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
                Bad noError state



-- CONTEXT


inContext : R.Position -> E.Context -> Parser a -> Parser a
inContext pos ctx (Parser parse) =
    Parser <|
        \({ context } as initialState) ->
            let
                state1 =
                    changeContext (( ctx, pos ) :: context) initialState
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


nextCharIs : Char -> Int -> String -> Bool
nextCharIs good =
    nextChar ((==) good)


nextStringIs : String -> Int -> String -> Bool
nextStringIs good offset source =
    let
        ( newOffset, _, _ ) =
            Prim.isSubString good offset 0 0 source
    in
    newOffset /= badParse



-- CHOMP NUMBER STUFF


chompInt : Int -> String -> Result ( Int, Problem ) ( Int, L.Literal )
chompInt offset source =
    let
        stopOffset =
            chomp Char.isDigit offset source
    in
    if offset == stopOffset then
        Err ( stopOffset, Theories [] [] )
    else if nextCharIs '.' stopOffset source then
        chompFraction offset (stopOffset + 1) source
    else if nextChar isE stopOffset source then
        chompExponent offset (stopOffset + 1) source
    else if nextChar isBadIntEnd stopOffset source then
        Err ( stopOffset, BadNumberEnd )
    else
        Ok ( stopOffset, readInt offset stopOffset source )


chompZero : Int -> Int -> String -> Result ( Int, Problem ) ( Int, L.Literal )
chompZero offset zeroOffset source =
    if nextCharIs '.' zeroOffset source then
        chompFraction offset (zeroOffset + 1) source
    else if nextCharIs 'x' zeroOffset source then
        chompHex offset (zeroOffset + 1) source
    else if nextChar Char.isDigit zeroOffset source then
        Err ( zeroOffset, BadNumberZero )
    else
        Ok ( zeroOffset, L.IntNum 0 )


chompFraction : Int -> Int -> String -> Result ( Int, Problem ) ( Int, L.Literal )
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


chompExponent : Int -> Int -> String -> Result ( Int, Problem ) ( Int, L.Literal )
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


chompHex : Int -> Int -> String -> Result ( Int, Problem ) ( Int, L.Literal )
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



-- CHOMP COMMENTS


chompMultiComment :
    Int
    -> Int
    -> Int
    -> Int
    -> String
    -> ( Maybe Problem, Int, Int, Int )
chompMultiComment offset row col openComments source =
    if atNewLine offset source then
        chompMultiComment (offset + 1) (row + 1) 1 openComments source
    else if nextStringIs "{-" offset source then
        chompMultiComment (offset + 2) row (col + 2) (openComments + 1) source
    else if nextStringIs "-}" offset source then
        if openComments == 1 then
            ( Nothing, offset + 2, row, col + 2 )
        else
            chompMultiComment (offset + 2) row (col + 2) (openComments - 1) source
    else if nextChar isAny offset source then
        chompMultiComment (offset + 1) row (col + 1) openComments source
    else
        ( Just EndOfFile_Comment, offset, row, col )



-- CHOMP CHARACTER STUFF


chompEscape : Int -> String -> Problem -> Result Problem ( Int, Char )
chompEscape offset source problem =
    if atEnd offset source then
        Err problem
    else if nextCharIs 'n' offset source then
        Ok ( 2, '\n' )
    else if nextCharIs 't' offset source then
        Ok ( 2, '\t' )
    else if nextCharIs '"' offset source then
        Ok ( 2, '"' )
    else if nextCharIs '\'' offset source then
        Ok ( 2, '\'' )
    else if nextCharIs '\\' offset source then
        Ok ( 2, '\\' )
    else
        Err BadEscape



-- CHARACTER CHOMP HELPERS


is : a -> a -> Bool
is target =
    \char -> char == target


isAny : a -> Bool
isAny _ =
    True


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


atNewLine : Int -> String -> Bool
atNewLine offset source =
    Prim.isSubChar (is '\n') offset source == newLineParse


atEnd : Int -> String -> Bool
atEnd offset source =
    Prim.isSubChar isAny offset source == badParse



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
