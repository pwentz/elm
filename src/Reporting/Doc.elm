module Reporting.Doc
    -- Stand-in for Text-PrettyPrint-ANSI-Leijen
    -- https://hackage.haskell.org/package/ansi-wl-pprint-0.6.8.1/docs/Text-PrettyPrint-ANSI-Leijen.html
    -- — "This module is an extended implementation of the functional pretty printer given by Philip Wadler (1997)"
    --
    -- TODO
    exposing
        ( Doc
        , cat
        , dullred
        , dullyellow
        , empty
        , fillSep
        , hardline
        , indent
        , space
        , string
        , underline
        , vcat
        )


type Doc
    = Doc String



-- CREATE


empty : Doc
empty =
    Doc ""


hardline : Doc
hardline =
    Doc "\n"


space : Doc
space =
    Doc " "


string : String -> Doc
string =
    Doc



-- COMBINE


cat : List Doc -> Doc
cat =
    List.foldl (\(Doc next) (Doc acc) -> Doc (acc ++ next)) empty


fillSep : List Doc -> Doc
fillSep =
    cat


vcat : List Doc -> Doc
vcat =
    cat



-- MODIFIERS


indent : Int -> Doc -> Doc
indent n (Doc s) =
    String.split "\n" s
        |> String.join ("\n" ++ String.repeat n " ")
        |> Doc


dullred : Doc -> Doc
dullred doc =
    doc


dullyellow : Doc -> Doc
dullyellow doc =
    doc


underline : Doc -> Doc
underline doc =
    doc
