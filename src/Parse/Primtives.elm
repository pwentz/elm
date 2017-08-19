module Parse.Primitives
    exposing
        ( Parser
        , run
          -- , try, deadend, hint, endOfFile
          -- , oneOf
          -- , symbol, underscore, keyword, keywords
          -- , lowVar, capVar, infixOp
          -- , getPosition, getCol
          -- , getContext, pushContext, popContext
          -- , getIndent, setIndent
          -- , SPos(..), whitespace
          -- , docComment, chompUntilDocs
          -- , string, character
          -- , digit, number
          -- , shaderSource, shaderFailure
          -- , kernelChunk, ChunkTag(..)
        )

import AST.Helpers as Help exposing (isSymbol)
import AST.Literal as L
import Char
import ParserPrimitives as Prim
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


type Parser a
    = Parser (State -> Step a)


type Step a
    = Good a State ParseError
    | Bad ParseError


type alias State =
    { source : String
    , offset : Int
    , indent : Int
    , context : List E.ContextStack
    , row : Int
    , col : Int
    }


noError : ParseError
noError =
    ParseError 0 0 (Theories [] [])


expect : Int -> Int -> E.ContextStack -> Theory -> ParseError
expect row col ctx theory =
    ParseError row col (Theories ctx [ theory ])


run : Parser a -> String -> Result (A.Located E.Error) a
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
        Good a _ _ ->
            Ok a

        Bad (ParseError row col problem) ->
            let
                pos =
                    R.Position row col

                region =
                    R.Region pos pos

                mkError overallRegion subRegion =
                    Err (A.A overallRegion (E.Parse subRegion problem))
            in
            case problem of
                BadOp _ (( _, start ) :: _) ->
                    mkError (R.Region start pos) (Just region)

                Theories (( _, start ) :: _) _ ->
                    mkError (R.Region start pos) (Just region)

                _ ->
                    mkError region Nothing



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


chompDigits : (Char -> Bool) -> Int -> String -> Result Int Int
chompDigits isValidDigit offset source =
    let
        newOffset =
            chomp isValidDigit offset source
    in
    -- no digits
    if newOffset == offset then
        Err newOffset
        -- ends with non-digit characters
    else if Prim.isSubChar isBadIntEnd newOffset source /= -1 then
        Err newOffset
        -- all valid digits!
    else
        Ok newOffset


isBadIntEnd : Char -> Bool
isBadIntEnd char =
    Char.isDigit char
        || Char.isUpper char
        || Char.isLower char
        || (char == '.')



-- CHOMP FLOAT STUFF


chompDotAndExp : Int -> String -> Result Int Int
chompDotAndExp offset source =
    let
        dotOffset =
            Prim.isSubChar isDot offset source
    in
    if dotOffset == -1 then
        chompExp offset source
    else
        chompExp (chomp Char.isDigit dotOffset source) source


isDot : Char -> Bool
isDot char =
    char == '.'


chompExp : Int -> String -> Result Int Int
chompExp offset source =
    let
        eOffset =
            Prim.isSubChar isE offset source
    in
    if eOffset == -1 then
        Ok offset
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
            Err expOffset
        else if Prim.isSubChar Char.isDigit expOffset source == -1 then
            Err expOffset
        else
            chompDigits Char.isDigit expOffset source


isE : Char -> Bool
isE char =
    char == 'e' || char == 'E'


isZero : Char -> Bool
isZero char =
    char == '0'


isPlusOrMinus : Char -> Bool
isPlusOrMinus char =
    char == '+' || char == '-'
