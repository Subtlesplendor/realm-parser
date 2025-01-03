# realm-parser
A port of Elm's Parser library to Roc, empowered by Roc's type system. https://www.roc-lang.org/

If you are interested in a generic Roc parser library, see also [roc-parser](https://github.com/lukewilliamboswell/roc-parser), maintained by [Luke Boswell](https://github.com/lukewilliamboswell), which is easier to use, more stable and as been more thoroughly tested than this package.

## Structure
The most advanced parser is Advanced.Generic.Parser. It is as general as possible, and has as much functionality as possible.

To get started I would instead recommend to use the UTF8 or Bytes parser.

## This is a work in progress
I am currently experimenting with using this as a UTF8 parser for lexing/parsing in the frontend of a compiler. I expect things to change due to this usage. Will need to add some more tests as well.

The exposed quantities in the UTF8 parser is in particular likely to change, as I am refactoring it a bit.

## Differences to the Elm parser
This parser library works for parsing generic lists, not just strings. This means that some of the concepts of the Elm parser do not carry over. So the parsers now do not keep track of row, column, or indent --- because those do not make sense for arbitrary lists.

A slight complication is hence that to generate a nice error message that refers to e.g. a row and column requires a second pass through of the source to determine these quantities at the location of the error.
