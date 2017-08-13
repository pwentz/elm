module Reporting.Doc
    exposing
        ( Doc
        , concat
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


concat : List Doc -> Doc
concat =
    List.foldl (\(Doc next) (Doc acc) -> Doc (acc ++ next)) empty


fillSep : List Doc -> Doc
fillSep =
    concat


vcat : List Doc -> Doc
vcat =
    concat



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
