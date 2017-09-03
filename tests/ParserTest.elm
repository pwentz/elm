module ParserTest exposing (..)

import AST.Literal exposing (Literal(..))
import Expect exposing (Expectation)
import Parse.Primitives as P
import Reporting.Error.Syntax as E
import Reporting.Region as R
import Test exposing (..)


numbers : Test
numbers =
    describe "number parser" <|
        List.map (testRun P.number)
            [ "0" => Ok (IntNum 0)
            , "12345" => Ok (IntNum 12345)
            , "6789 " => Ok (IntNum 6789)
            , "4e9" => Ok (FloatNum 4.0e9)
            , "3E7" => Ok (FloatNum 3.0e7)
            , "2e-9" => Ok (FloatNum 2.0e-9)
            , "1E+3" => Ok (FloatNum 1.0e3)
            , "3.14" => Ok (FloatNum 3.14)
            , "1.21  " => Ok (FloatNum 1.21)
            , "0.67" => Ok (FloatNum 0.67)
            , "6.022e26" => Ok (FloatNum 6.022e26)
            , "0.23E4" => Ok (FloatNum 2.3e3)
            , "0xFFFFFFFF" => Ok (IntNum 0xFFFFFFFF)
            , "0x0040" => Ok (IntNum 0x40)
            , "0xbeef" => Ok (IntNum 0xBEEF)
            , "0xBEEF" => Ok (IntNum 0xBEEF)
            , "0x0123789" => Ok (IntNum 0x00123789)
            , ".1234" => Err (E.Theories [] [])
            , "1." => Err E.BadNumberDot
            , "1_" => Err E.BadNumberEnd
            , "1.2_" => Err E.BadNumberEnd
            , "1e" => Err E.BadNumberExp
            , "1e." => Err E.BadNumberExp
            , "0x" => Err E.BadNumberHex
            , "0x7.5" => Err E.BadNumberHex
            , "0123" => Err E.BadNumberZero
            ]


whitespace : Test
whitespace =
    describe "whitespace parser" <|
        List.map (testRun P.whitespace)
            [ "" => Ok (P.SPos (R.Position 1 1))
            , "      " => Ok (P.SPos (R.Position 1 7))
            , "  \n" => Ok (P.SPos (R.Position 2 1))
            , "\n  " => Ok (P.SPos (R.Position 2 3))
            , "{--}" => Ok (P.SPos (R.Position 1 5))
            , "{--}\n" => Ok (P.SPos (R.Position 2 1))
            , "{-{--}-}" => Ok (P.SPos (R.Position 1 9))
            , "{-\n\n-}\n" => Ok (P.SPos (R.Position 4 1))
            , "-- asdf\n" => Ok (P.SPos (R.Position 2 1))
            , "--     \n  " => Ok (P.SPos (R.Position 2 3))
            , "123" => Ok (P.SPos (R.Position 1 1))
            , "{-|-}" => Ok (P.SPos (R.Position 1 1))
            , "\t" => Err E.Tab
            , " \t" => Err E.Tab
            , "{-" => Err E.EndOfFile_Comment
            , "{-\n\n" => Err E.EndOfFile_Comment
            , "{-{--}" => Err E.EndOfFile_Comment
            ]


docComment : Test
docComment =
    describe "documentation comment parser" <|
        List.map (testRun P.docComment)
            [ "{-|hello-}" => Ok "hello"
            , "{-|yo-}   " => Ok "yo"
            , "{-|{-|-}-}   " => Ok "{-|-}"
            ]



-- HELPERS


testRun : P.Parser a -> ( String, Result E.Problem a ) -> Test.Test
testRun parser ( str, result ) =
    test ("can parse '" ++ str ++ "'") <|
        \_ ->
            P.run parser str
                |> Result.mapError .problem
                |> Expect.equal result


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
