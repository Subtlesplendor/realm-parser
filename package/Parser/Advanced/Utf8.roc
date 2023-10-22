interface Parser.Advanced.Utf8
    exposes [Parser, DeadEnd, Step, Token, RawChar, RawStr, #Types
             buildPrimitiveParser,
             run, #Operating
             const, fail, problem, end, token, #Primitives
             map, map2, keep, skip, andThen, flatten, #Combinators
             lazy, many, oneOrMore, alt, oneOf, between, sepBy, ignore, #Combinators
             chompIf, chompWhile, chompUntil, chompUntilEndOr, getChompedRawStr, mapChompedRawStr, #Chompers
             getOffset, getSource, # Info
             backtrackable, commit, # Backtracking
             loop, # Looping
             keyword
             ]
    imports [Parser.Advanced.Generic]


# -- PARSERS ------------------

RawChar: U8
RawStr: List U8

Parser context problem value : Parser.Advanced.Generic.Parser context RawChar problem value

DeadEnd context problem : Parser.Advanced.Generic.DeadEnd context problem


Token p : Parser.Advanced.Generic.Token RawChar p

# -- RUN ------------------
# To be refactored: do not reference internal types.
buildPrimitiveParser = Parser.Advanced.Generic.buildPrimitiveParser

run : Parser c p a, RawStr -> Result a (List (DeadEnd c p))
run = \parser, src ->
    Parser.Advanced.Generic.run parser src
        
# -- PRIMITIVES -----------

const : v -> Parser * * v
const = \value ->
     Parser.Advanced.Generic.const value

problem : p -> Parser * p *
problem = \prob ->
     Parser.Advanced.Generic.problem prob

fail : Parser * * *
fail = 
    Parser.Advanced.Generic.fail
    
end: p -> Parser * p {}
end = \expecting ->
    Parser.Advanced.Generic.end expecting


# # -- COMBINATORS ----------

map: Parser c p a, (a -> b) -> Parser c p b
map = \parser, mapper ->
    Parser.Advanced.Generic.map parser mapper

map2: Parser c p a, Parser c p b, (a, b -> d) -> Parser c p d
map2 = \first, second, mapper ->
    Parser.Advanced.Generic.map2 first second mapper

keep: Parser c p (a -> b), Parser c p a -> Parser c p b        
keep = \parseFunc, parseKeep ->
    Parser.Advanced.Generic.keep parseFunc parseKeep

skip: Parser c p keep, Parser c p ignore -> Parser c p keep
skip = \parserKeep, parserSkip ->
    Parser.Advanced.Generic.skip parserKeep parserSkip

andThen: Parser c p a, (a -> Parser c p b) -> Parser c p b
andThen = \parser, parserBuilder ->
    Parser.Advanced.Generic.andThen parser parserBuilder

alt: Parser c p v, Parser c p v -> Parser c p v
alt = \first, second ->
    Parser.Advanced.Generic.alt first second       

oneOf : List (Parser c p v) -> Parser c p v
oneOf = \parsers ->
    Parser.Advanced.Generic.oneOf parsers

lazy : ({} -> Parser c p v) -> Parser c p v
lazy = \thunk ->
    Parser.Advanced.Generic.lazy thunk   


many : Parser c p v -> Parser c p (List v)
many = \parser ->
    Parser.Advanced.Generic.many parser

oneOrMore : Parser c p v -> Parser c p (List v)
oneOrMore = \parser ->
    Parser.Advanced.Generic.oneOrMore parser

between : Parser c p v, Parser c p *, Parser c p * -> Parser c p v
between = \parser, open, close ->
    Parser.Advanced.Generic.between parser open close       

sepBy : Parser c p v, Parser c p * -> Parser c p (List v)
sepBy = \parser, separator ->
    Parser.Advanced.Generic.sepBy parser separator


ignore : Parser c p v -> Parser c p {}
ignore = \parser ->
    Parser.Advanced.Generic.ignore parser   


flatten : Parser c p (Result v p) -> Parser c p v
flatten = \parser ->
    Parser.Advanced.Generic.flatten parser

# # ---- CHOMPERS -------

chompIf: (RawChar -> Bool), p -> Parser * p {}
chompIf = \isGood, expecting ->
    Parser.Advanced.Generic.chompIf isGood expecting   


getChompedRawStr : Parser c p * -> Parser c p RawStr
getChompedRawStr = \parser ->
    Parser.Advanced.Generic.getChompedSource parser

mapChompedRawStr : Parser c p a, (RawStr, a -> b) -> Parser c p b
mapChompedRawStr = \parser, mapper ->
    Parser.Advanced.Generic.mapChompedSource parser mapper
       

chompWhile: (RawChar -> Bool) -> Parser c p {}
chompWhile = \isGood ->
    Parser.Advanced.Generic.chompWhile isGood


chompUntil : Token p -> Parser * p {}
chompUntil = \tok ->
    Parser.Advanced.Generic.chompUntil tok


chompUntilEndOr : RawStr -> Parser c p {}
chompUntilEndOr = \raw ->
    Parser.Advanced.Generic.chompUntilEndOr raw


# # -- LOOP ---------

Step state a : Parser.Advanced.Generic.Step state a

loop : state, (state -> Parser c p (Step state a)) -> Parser c p a
loop = \state, callback ->
    Parser.Advanced.Generic.loop state callback

# # -- BACKTRACKABLE ---------

backtrackable : Parser c p a -> Parser c p a
backtrackable = \parser ->
    Parser.Advanced.Generic.backtrackable parser

commit : a -> Parser * * a
commit = \value ->
    Parser.Advanced.Generic.commit value

# # -- POSITION

getOffset: Parser * * Nat
getOffset =
    Parser.Advanced.Generic.getOffset

getSource: Parser * * RawStr
getSource =
    Parser.Advanced.Generic.getSource

# -- TOKEN

token : Token p -> Parser * p {}
token = \tok ->
    Parser.Advanced.Generic.token tok

keyword: List RawChar, Token p -> Parser * p {}
keyword = \separators, tok ->
    Parser.Advanced.Generic.key separators tok
