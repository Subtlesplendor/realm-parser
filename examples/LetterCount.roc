app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "../package/main.roc",
}

import cli.Stdout

import parser.Parser.Utf8 as Parser exposing [Parser]

Letter : [A, B, C, Other]

# testInput = "AAABBC"

parseA : Parser Letter
parseA =
    Parser.string "A"
    |> Parser.map \_ -> A

parseB : Parser Letter
parseB =
    Parser.string "B"
    |> Parser.map \_ -> B

parseC : Parser Letter
parseC =
    Parser.string "C"
    |> Parser.map \_ -> C

parseOther : Parser Letter
parseOther =
    Parser.next
    |> Parser.map \_ -> Other

letterParser : Parser Letter
letterParser =
    Parser.oneOf [
        parseA,
        parseB,
        parseC,
        parseOther,
    ]

manyLettersParser : Parser (List Letter)
manyLettersParser =
    Parser.many letterParser

expect
    lettersInput = "AAAiBByAABBwBtCCCiAyArBBx" |> Str.toUtf8
    # lettersInput = "ABiC" |> Str.toUtf8
    res =
        Parser.run manyLettersParser lettersInput
        |> Result.map \lst ->
            lst |> List.dropIf \l -> l == Other
    res == Ok ([A, A, A, B, B, A, A, B, B, B, C, C, C, A, A, B, B])

main =
    lettersInput = "AAAiBByAABBwBtCCCiAyArBBx" |> Str.toUtf8
    # lettersInput = "A" |> Str.toUtf8
    res =
        Parser.run manyLettersParser lettersInput
        |> Result.map \lst ->
            lst |> List.dropIf \l -> l == Other
    check =
        res == Ok ([A, A, A, B, B, A, A, B, B, B, C, C, C, A, A, B, B])

    if check then
        Stdout.line "Test passed"
    else
        Stdout.line "Test failed"

# main =
#     lettersInput = "AAAiBByAABBwBtCCCiAyArBBx"
#     ifLetterA = \l -> l == A
#     when parseStr (many letterParser) lettersInput is
#         Ok letters ->
#             letters
#             |> List.keepIf ifLetterA
#             |> List.map \_ -> 1
#             |> List.sum
#             |> Num.toStr
#             |> \countLetterA -> Stdout.line "I counted \(countLetterA) letter A's!"

#         Err _ -> Stderr.line "Ooops, something went wrong parsing letters"

# Letter : [A, B, C, Other]

# letterParser : Parser (List U8) Letter
# letterParser =
#     input <- buildPrimitiveParser

#     valResult =
#         when input is
#             [] -> Err (ParsingFailure "Nothing to parse")
#             ['A', ..] -> Ok A
#             ['B', ..] -> Ok B
#             ['C', ..] -> Ok C
#             _ -> Ok Other

#     valResult
#     |> Result.map \val -> { val, input: List.dropFirst input }

# expect
#     input = "B"
#     parser = letterParser
#     result = parseStr parser input
#     result == Ok B

# expect
#     input = "BCXA"
#     parser = many letterParser
#     result = parseStr parser input
#     result == Ok [B, C, Other, A]
