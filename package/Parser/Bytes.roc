module [
    Parser,
    DeadEnd, # Types
    buildPrimitiveParser,
    run, # Operating
    const,
    fail,
    problem,
    end,
    token, # Primitives
    map,
    map2,
    keep,
    skip,
    andThen,
    flatten, # Combinators
    lazy,
    many,
    oneOrMore,
    alt,
    oneOf,
    between,
    sepBy,
    ignore, # Combinators
    chompIf,
    chompWhile,
    chompUntil,
    chompUntilEndOr,
    getChompedBytes,
    mapChompedBytes, # Chompers
    getOffset,
    getSource, # Info
    backtrackable,
    commit, # Backtracking
    loop, # Looping
    chompBytes,
    chompByte,
]

import Parser.Advanced.Bytes as Advanced exposing [Byte, ByteList]

# -- PARSERS ------------------

# Byte : Advanced.Byte
# ByteList : Advanced.ByteList

Parser value : Advanced.Parser Context Problem value

DeadEnd : Advanced.DeadEnd Context Problem

Context : {}

# -- PROBLEMS ------------------

Problem : [
    UnexpectedByte,
    Expecting ByteList,
    ParsingFailure Str,
    ExpectingEnd,
]

# -- RUN ------------------
# To be refactored: do not reference internal types.
buildPrimitiveParser = Advanced.buildPrimitiveParser

run : Parser a, ByteList -> Result a (List DeadEnd)
run = \parser, input ->
    when Advanced.run parser input is
        Ok value ->
            Ok value

        Err problems ->
            Err (problems |> List.map problemToDeadEnd)

problemToDeadEnd : Advanced.DeadEnd Context _ -> DeadEnd
problemToDeadEnd = \d ->
    { offset: d.offset, problem: d.problem, contextStack: [] }

# -- PRIMITIVES -----------

const : v -> Parser v
const = \value ->
    Advanced.const value

problem : Str -> Parser *
problem = \msg ->
    Advanced.problem (ParsingFailure msg)

fail : Parser *
fail =
    Advanced.fail

end : Parser {}
end =
    Advanced.end ExpectingEnd

# -- COMBINATORS ----------

map : Parser a, (a -> b) -> Parser b
map = \parser, mapper ->
    Advanced.map parser mapper

map2 : Parser a, Parser b, (a, b -> d) -> Parser d
map2 = \first, second, mapper ->
    Advanced.map2 first second mapper

keep : Parser (a -> b), Parser a -> Parser b
keep = \parserFunc, parser ->
    Advanced.keep parserFunc parser

skip : Parser keep, Parser ignore -> Parser keep
skip = \parserKeep, parserSkip ->
    Advanced.skip parserKeep parserSkip

andThen : Parser a, (a -> Parser b) -> Parser b
andThen = \parser, parserBuilder ->
    Advanced.andThen parser parserBuilder

alt : Parser v, Parser v -> Parser v
alt = \first, second ->
    Advanced.alt first second

oneOf : List (Parser v) -> Parser v
oneOf = \parsers ->
    Advanced.oneOf parsers

lazy : ({} -> Parser v) -> Parser v
lazy = \thunk ->
    Advanced.lazy thunk

many : Parser v -> Parser (List v)
many = \parser ->
    Advanced.many parser

oneOrMore : Parser v -> Parser (List v)
oneOrMore = \parser ->
    Advanced.oneOrMore parser

between : Parser v, Parser *, Parser * -> Parser v
between = \parser, open, close ->
    Advanced.between parser open close

sepBy : Parser v, Parser * -> Parser (List v)
sepBy = \parser, separator ->
    Advanced.sepBy parser separator

ignore : Parser v -> Parser {}
ignore = \parser ->
    Advanced.ignore parser

flatten : Parser (Result v Problem) -> Parser v
flatten = \parser ->
    Advanced.flatten parser

# ---- CHOMPERS -------

chompIf : (Byte -> Bool) -> Parser {}
chompIf = \isGood ->
    Advanced.chompIf isGood UnexpectedByte

getChompedBytes : Parser * -> Parser ByteList
getChompedBytes = \parser ->
    Advanced.getChompedBytes parser

mapChompedBytes : Parser a, (ByteList, a -> b) -> Parser b
mapChompedBytes = \parser, mapper ->
    Advanced.mapChompedBytes parser mapper

chompWhile : (Byte -> Bool) -> Parser {}
chompWhile = \isGood ->
    Advanced.chompWhile isGood

chompUntil : ByteList -> Parser {}
chompUntil = \bytelist ->
    Advanced.chompUntil (toToken bytelist)

chompUntilEndOr : ByteList -> Parser {}
chompUntilEndOr = \bytelist ->
    Advanced.chompUntilEndOr bytelist

# -- LOOP ---------

Step state a : Advanced.Step state a

loop : state, (state -> Parser (Step state a)) -> Parser a
loop = \state, callback ->
    Advanced.loop state callback

# -- BACKTRACKABLE ---------

backtrackable : Parser a -> Parser a
backtrackable = \parser ->
    Advanced.backtrackable parser

commit : a -> Parser a
commit = \value ->
    Advanced.commit value

# -- POSITION

getOffset : Parser U64
getOffset =
    Advanced.getOffset

getSource : Parser ByteList
getSource =
    Advanced.getSource

# -- TOKEN

token : ByteList -> Parser {}
token = \bytelist ->
    Advanced.token (bytelist |> toToken)

toToken : ByteList -> Advanced.Token Problem
toToken = \tok ->
    { tok, expecting: Expecting tok }

# -- Byte specific

chompBytes : ByteList -> Parser {}
chompBytes = \bytelist ->
    token bytelist

chompByte : Byte -> Parser {}
chompByte = \b ->
    chompIf (\x -> x == b)
