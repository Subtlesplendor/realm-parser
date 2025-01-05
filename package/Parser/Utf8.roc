module [
    # Types
    Parser,
    DeadEnd,
    # Operating
    buildPrimitiveParser,
    run,
    # Primitives
    const,
    fail,
    problem,
    end,
    token,
    # Combinators
    map,
    map2,
    keep,
    skip,
    andThen,
    flatten,
    lazy,
    many,
    oneOrMore,
    alt,
    oneOf,
    between,
    sepBy,
    ignore,
    next,
    # Chompers
    chompIf,
    chompUntil,
    chompUntilEndOr,
    getChompedUtf8,
    getChompedStr,
    mapChompedUtf8,
    # Info
    getOffset,
    getSource,
    # Backtracking
    backtrackable,
    commit,
    # Looping
    loop,
    # Utf8 specific
    chompString,
    string,
    digit,
    decimal,
    alpha,
    alphaNumeric,
]

import Parser.Advanced.Generic as A

# -- PARSERS ------------------

Parser value : A.Parser Context U8 Problem value

State : A.State Context U8
PStep value : A.PStep Context U8 Problem value

DeadEnd : A.DeadEnd Context Problem

Context : {}

# -- PROBLEMS ------------------

Problem : [
    Expecting (List U8),
    ParsingFailure Str,
    BadUtf8,
    ExpectingEnd,
    OutOfBounds,
]

buildPrimitiveParser : (State -> PStep v) -> Parser v
buildPrimitiveParser = A.buildPrimitiveParser

# -- RUN ------------------
run : Parser a, List U8 -> Result a (List DeadEnd)
run = \parser, input ->
    if (Str.fromUtf8 input |> Result.isOk) then
        A.run parser input
        |> Result.mapErr \problems ->
            problems |> List.map problemToDeadEnd
    else
        Err [{ offset: 0, problem: BadUtf8, contextStack: [] }]

problemToDeadEnd : A.DeadEnd Context _ -> DeadEnd
problemToDeadEnd = \d ->
    { offset: d.offset, problem: d.problem, contextStack: [] }

# -- PRIMITIVES -----------

const : v -> Parser v
const = \value ->
    A.const value

problem : Str -> Parser *
problem = \msg ->
    A.problem (ParsingFailure msg)

fail : Parser *
fail =
    A.fail

end : Parser {}
end =
    A.end ExpectingEnd

# -- COMBINATORS ----------

map : Parser a, (a -> b) -> Parser b
map = \parser, mapper ->
    A.map parser mapper

map2 : Parser a, Parser b, (a, b -> d) -> Parser d
map2 = \first, second, mapper ->
    A.map2 first second mapper

keep : Parser (a -> b), Parser a -> Parser b
keep = \parserFunc, parser ->
    A.keep parserFunc parser

skip : Parser keep, Parser ignore -> Parser keep
skip = \parserKeep, parserSkip ->
    A.skip parserKeep parserSkip

andThen : Parser a, (a -> Parser b) -> Parser b
andThen = \parser, parserBuilder ->
    A.andThen parser parserBuilder

alt : Parser v, Parser v -> Parser v
alt = \first, second ->
    A.alt first second

oneOf : List (Parser v) -> Parser v
oneOf = \parsers ->
    A.oneOf parsers

lazy : ({} -> Parser v) -> Parser v
lazy = \thunk ->
    A.lazy thunk

many : Parser v -> Parser (List v)
many = \parser ->
    A.many parser

oneOrMore : Parser v -> Parser (List v)
oneOrMore = \parser ->
    A.oneOrMore parser

between : Parser v, Parser *, Parser * -> Parser v
between = \parser, open, close ->
    A.between parser open close

sepBy : Parser v, Parser * -> Parser (List v)
sepBy = \parser, separator ->
    A.sepBy parser separator

ignore : Parser v -> Parser {}
ignore = \parser ->
    A.ignore parser

flatten : Parser (Result v Problem) -> Parser v
flatten = \parser ->
    A.flatten parser

# next : Parser {}
next = A.next OutOfBounds

# ---- CHOMPERS -------

getChompedUtf8 : Parser * -> Parser (List U8)
getChompedUtf8 = \parser ->
    A.getChompedSource parser

getChompedStr : Parser * -> Parser Str
getChompedStr = \parser ->
    parser
    |> getChompedUtf8
    |> map \rawstr -> Str.fromUtf8 rawstr
    |> map \res ->
        res |> Result.mapErr \_ -> BadUtf8
    |> flatten

mapChompedUtf8 : Parser a, (List U8, a -> b) -> Parser b
mapChompedUtf8 = \parser, mapper ->
    A.mapChompedSource parser mapper

chompIf : (U8 -> Bool) -> Parser {}
chompIf = \predicate ->
    A.chompIf predicate (ParsingFailure "Byte does not match predicate.")

chompUntil : List U8 -> Parser {}
chompUntil = \tok ->
    A.chompUntil (toToken tok)

chompUntilEndOr : List U8 -> Parser {}
chompUntilEndOr = \raw ->
    A.chompUntilEndOr raw

# -- LOOP ---------

Step state a : A.Step state a

loop : state, (state -> Parser (Step state a)) -> Parser a
loop = \state, callback ->
    A.loop state callback

# -- BACKTRACKABLE ---------

backtrackable : Parser a -> Parser a
backtrackable = \parser ->
    A.backtrackable parser

commit : a -> Parser a
commit = \value ->
    A.commit value

# -- POSITION

getOffset : Parser U64
getOffset =
    A.getOffset

getSource : Parser (List U8)
getSource =
    A.getSource

# -- TOKEN

token : List U8 -> Parser {}
token = \tok ->
    A.token (tok |> toToken)

toToken : List U8 -> A.Token U8 Problem
toToken = \tok ->
    { tok, expecting: Expecting tok }

# Utf8 specific

chompString : List U8 -> Parser {}
chompString = \raw ->
    token raw

rwstr : List U8 -> Parser (List U8)
rwstr = \raw ->
    chompString raw
    |> getChompedUtf8

string : Str -> Parser Str
string = \str ->
    strToRawStr str
    |> rwstr
    |> map rawStrToStr
    |> flatten

alpha : Parser Str
alpha =
    chompIf \b ->
        (b >= 65 && b <= 90) # capital letters
        || (b >= 97 && b <= 122) # small letters
    |> oneOrMore
    |> getChompedStr

alphaNumeric : Parser Str
alphaNumeric =
    chompIf \b ->
        (b >= 65 && b <= 90) # capital letters
        || (b >= 97 && b <= 122) # small letters
        || (b >= 48 && b <= 57)
    |> oneOrMore
    |> getChompedStr

digit : Parser (List U8)
digit =
    chompIf (\b -> b >= 48 && b <= 57)
    |> getChompedUtf8

expect
    testInput = "1" |> Str.toUtf8
    run digit testInput == Ok testInput

decimal : Parser Str
decimal =
    string "."
    |> between (oneOrMore digit) (oneOrMore digit)
    |> getChompedStr

expect
    testInput = "132.1234" |> Str.toUtf8
    run decimal testInput == Ok "132.1234"

# --- Internal -------

rawStrToStr : List U8 -> Result Str Problem
rawStrToStr = \raw ->
    Result.onErr (Str.fromUtf8 raw) \_ ->
        Err (ParsingFailure "Failed to create Str from raw string (List U8).")

strToRawStr : Str -> List U8
strToRawStr = \str ->
    Str.toUtf8 str
