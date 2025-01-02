module [
    Parser,
    DeadEnd,
    RawStr,
    RawChar, # Types
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
    getChompedRawStr,
    mapChompedRawStr, # Chompers
    getOffset,
    getSource, # Info
    backtrackable,
    commit, # Backtracking
    loop, # Looping
    chompString,
    chompChar,
    keyword,
    string,
]

import Parser.Advanced.Utf8 as Advanced

# -- PARSERS ------------------

RawChar : Advanced.RawChar
RawStr : Advanced.RawStr

Parser value : Advanced.Parser Context Problem value

State : Advanced.State Context
PStep value : Advanced.PStep Context Problem value

DeadEnd : Advanced.DeadEnd Context Problem

Context : {}

# -- PROBLEMS ------------------

Problem : [
    UnexpectedRawChar,
    Expecting RawStr,
    ExpectingKeyword RawStr,
    ParsingFailure Str,
    ExpectingEnd,
]

buildPrimitiveParser : (State -> PStep v) -> Parser v
buildPrimitiveParser = Advanced.buildPrimitiveParser

# -- RUN ------------------
run : Parser a, RawStr -> Result a (List DeadEnd)
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

chompIf : (RawChar -> Bool) -> Parser {}
chompIf = \isGood ->
    Advanced.chompIf isGood UnexpectedRawChar

getChompedRawStr : Parser * -> Parser RawStr
getChompedRawStr = \parser ->
    Advanced.getChompedRawStr parser

mapChompedRawStr : Parser a, (RawStr, a -> b) -> Parser b
mapChompedRawStr = \parser, mapper ->
    Advanced.mapChompedRawStr parser mapper

chompWhile : (RawChar -> Bool) -> Parser {}
chompWhile = \isGood ->
    Advanced.chompWhile isGood

chompUntil : RawStr -> Parser {}
chompUntil = \tok ->
    Advanced.chompUntil (toToken tok)

chompUntilEndOr : RawStr -> Parser {}
chompUntilEndOr = \raw ->
    Advanced.chompUntilEndOr raw

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

getSource : Parser RawStr
getSource =
    Advanced.getSource

# -- TOKEN

token : RawStr -> Parser {}
token = \tok ->
    Advanced.token (tok |> toToken)

toToken : RawStr -> Advanced.Token Problem
toToken = \tok ->
    { tok, expecting: Expecting tok }

# Utf8 specific

separators = [' ', '\n']

keyword : RawStr -> Parser {}
keyword = \rawstr ->
    Advanced.keyword separators { tok: rawstr, expecting: ExpectingKeyword rawstr }

chompString : RawStr -> Parser {}
chompString = \raw ->
    token raw

chompChar : RawChar -> Parser {}
chompChar = \b ->
    chompIf (\x -> x == b)

rwstr : RawStr -> Parser RawStr
rwstr = \raw ->
    chompString raw
    |> getChompedRawStr

string : Str -> Parser Str
string = \str ->
    strToRawStr str
    |> rwstr
    |> map rawStrToStr
    |> flatten

# --- Internal -------

rawStrToStr : RawStr -> Result Str Problem
rawStrToStr = \raw ->
    Result.onErr (Str.fromUtf8 raw) \_ ->
        Err (ParsingFailure "Failed to create Str from raw string (List U8).")

strToRawStr : Str -> RawStr
strToRawStr = \str ->
    Str.toUtf8 str
