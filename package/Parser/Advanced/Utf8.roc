module [
    Parser,
    DeadEnd,
    Step,
    Token,
    State,
    PStep,
    RawStr, # Types
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
    next,
    between,
    sepBy,
    ignore, # Combinators
    chompUntil,
    chompUntilEndOr,
    getChompedRawStr,
    mapChompedRawStr, # Chompers
    getOffset,
    getSource, # Info
    backtrackable,
    commit, # Backtracking
    loop, # Looping
]

import Parser.Advanced.Generic as Generic

# -- PARSERS ------------------

# A raw string is a list of bytes. Subcollections of these bytes
# represent different Utf8 characters.
RawStr : List U8

State context : Generic.State context U8
PStep context problem value : Generic.PStep context U8 problem value

Parser context problem value : Generic.Parser context U8 problem value

DeadEnd context problem : Generic.DeadEnd context problem

Token p : Generic.Token U8 p

# -- RUN ------------------
buildPrimitiveParser : (State c -> PStep c p v) -> Parser c p v
buildPrimitiveParser = Generic.buildPrimitiveParser

run : Parser c p a, RawStr -> Result a (List (DeadEnd c p))
run = \parser, src ->
    Generic.run parser src

# -- PRIMITIVES -----------

const : v -> Parser * * v
const = \value ->
    Generic.const value

problem : p -> Parser * p *
problem = \prob ->
    Generic.problem prob

fail : Parser * * *
fail =
    Generic.fail

end : p -> Parser * p {}
end = \expecting ->
    Generic.end expecting

# -- COMBINATORS ----------

map : Parser c p a, (a -> b) -> Parser c p b
map = \parser, mapper ->
    Generic.map parser mapper

map2 : Parser c p a, Parser c p b, (a, b -> d) -> Parser c p d
map2 = \first, second, mapper ->
    Generic.map2 first second mapper

keep : Parser c p (a -> b), Parser c p a -> Parser c p b
keep = \parseFunc, parseKeep ->
    Generic.keep parseFunc parseKeep

skip : Parser c p keep, Parser c p ignore -> Parser c p keep
skip = \parserKeep, parserSkip ->
    Generic.skip parserKeep parserSkip

andThen : Parser c p a, (a -> Parser c p b) -> Parser c p b
andThen = \parser, parserBuilder ->
    Generic.andThen parser parserBuilder

alt : Parser c p v, Parser c p v -> Parser c p v
alt = \first, second ->
    Generic.alt first second

oneOf : List (Parser c p v) -> Parser c p v
oneOf = \parsers ->
    Generic.oneOf parsers

lazy : ({} -> Parser c p v) -> Parser c p v
lazy = \thunk ->
    Generic.lazy thunk

many : Parser c p v -> Parser c p (List v)
many = \parser ->
    Generic.many parser

oneOrMore : Parser c p v -> Parser c p (List v)
oneOrMore = \parser ->
    Generic.oneOrMore parser

between : Parser c p v, Parser c p *, Parser c p * -> Parser c p v
between = \parser, open, close ->
    Generic.between parser open close

sepBy : Parser c p v, Parser c p * -> Parser c p (List v)
sepBy = \parser, separator ->
    Generic.sepBy parser separator

ignore : Parser c p v -> Parser c p {}
ignore = \parser ->
    Generic.ignore parser

flatten : Parser c p (Result v p) -> Parser c p v
flatten = \parser ->
    Generic.flatten parser

next : p -> Parser * p {}
next = Generic.next

# ---- CHOMPERS -------

getChompedRawStr : Parser c p * -> Parser c p RawStr
getChompedRawStr = \parser ->
    Generic.getChompedSource parser

mapChompedRawStr : Parser c p a, (RawStr, a -> b) -> Parser c p b
mapChompedRawStr = \parser, mapper ->
    Generic.mapChompedSource parser mapper

chompUntil : Token p -> Parser * p {}
chompUntil = \tok ->
    Generic.chompUntil tok

chompUntilEndOr : RawStr -> Parser c p {}
chompUntilEndOr = \raw ->
    Generic.chompUntilEndOr raw

# -- LOOP ---------

Step state a : Generic.Step state a

loop : state, (state -> Parser c p (Step state a)) -> Parser c p a
loop = \state, callback ->
    Generic.loop state callback

# # -- BACKTRACKABLE ---------

backtrackable : Parser c p a -> Parser c p a
backtrackable = \parser ->
    Generic.backtrackable parser

commit : a -> Parser * * a
commit = \value ->
    Generic.commit value

# -- POSITION

getOffset : Parser * * U64
getOffset =
    Generic.getOffset

getSource : Parser * * RawStr
getSource =
    Generic.getSource

# -- TOKEN

token : Token p -> Parser * p {}
token = \tok ->
    Generic.token tok
