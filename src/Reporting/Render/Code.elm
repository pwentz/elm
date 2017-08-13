module Reporting.Render.Code exposing (render)

import Prelude exposing (last, maybe)
import Reporting.Doc as Doc exposing (Doc)
import Reporting.Helpers as H
import Reporting.Region as R


(|>) : a -> (a -> b) -> b
(|>) a f =
    f a


(<==>) : Doc -> Doc -> Doc
(<==>) a b =
    Doc.concat [ a, Doc.hardline, b ]


render : Maybe R.Region -> R.Region -> String -> Doc
render maybeSubRegion ((R.Region start end) as region) source =
    let
        (R.Position startLine _) =
            start

        (R.Position endLine _) =
            end

        relevantLines =
            (String.lines source ++ [ "" ])
                |> List.take endLine
                |> List.drop (startLine - 1)
                |> List.map2 (,) (List.range startLine (endLine + 1))

        width =
            last relevantLines
                |> Maybe.withDefault ( 0, "" )
                |> Tuple.first
                |> toString
                |> String.length

        smallerRegion =
            maybe region identity maybeSubRegion
    in
    case makeUnderline width endLine smallerRegion of
        Nothing ->
            drawLines True width smallerRegion relevantLines Doc.empty

        Just underline ->
            drawLines False width smallerRegion relevantLines underline


makeUnderline : Int -> Int -> R.Region -> Maybe Doc
makeUnderline width realEndLine (R.Region (R.Position start c1) (R.Position end c2)) =
    if start /= end || end < realEndLine then
        Nothing
    else
        let
            spaces =
                String.repeat (c1 + width + 1) " "

            zigzag =
                String.repeat (max 1 (c2 - c1)) "^"
        in
        Just (Doc.concat [ Doc.string spaces, Doc.dullred (Doc.string zigzag) ])


drawLines : Bool -> Int -> R.Region -> List ( Int, String ) -> Doc -> Doc
drawLines addZigZag width (R.Region start end) sourceLines finalLine =
    let
        (R.Position startLine _) =
            start

        (R.Position endLine _) =
            end
    in
    List.foldr (<==>) finalLine <|
        List.map (drawLine addZigZag width startLine endLine) sourceLines


drawLine : Bool -> Int -> Int -> Int -> ( Int, String ) -> Doc
drawLine addZigZag width startLine endLine ( n, line ) =
    addLineNumber addZigZag width startLine endLine n (Doc.string line)


addLineNumber : Bool -> Int -> Int -> Int -> Int -> Doc -> Doc
addLineNumber addZigZag width start end n line =
    let
        number =
            if n < 0 then
                " "
            else
                toString n

        lineNumber =
            String.repeat (width - String.length number) " " ++ number ++ "|"

        spacer =
            if addZigZag && start <= n && n <= end then
                Doc.dullred (Doc.string ">")
            else
                Doc.string " "
    in
    Doc.concat [ Doc.string lineNumber, spacer, line ]
