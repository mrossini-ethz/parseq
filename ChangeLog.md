# Change log

Parseq uses [semantic versioning](http://semver.org/).

## Unreleased:

### Features

  * Repetitions in `(rep ...)` and `(and~~ ...)` are now allowed to be specified as `*` for `(0 nil)`, `+` for `(1 nil)` and `?` for `(0 1)`.
  * Now allowing unsigned bytes to be converted to strings by the `:string` processing option.
  * Rule names are now allowed as rule arguments.
  * Introduced parseq-specific conditions and added some more error checks.

### Bugfixes

  * The `:flatten`, `:string` and `:vector` processing options are now correctly applied to the result of the previous processing option rather than directly to the parse result.
  * Open ended repetitions in `(and~~ ...)` used to fail. Fixed.
  * Fixed a bug in the runtime dispatch of parse rules.

## Version 0.2.0 - 2016-09-17

### Features

  * Support for parsing sequences other than lists. Currently lists, trees, vectors and strings are supported.
  * Added new nonterminals:
    * Unordered sequences: `(and~ ...)` and `(and~~ ...)`
    * Repetitions: `(rep ...)`
    * Nested string: `(string ...)`
    * Nested vector: `(vector ...)`
  * Added new terminals:
    * `t`, `nil`, `char`, `byte`, `number`, `list`, `vector`, `string`
    * literal characters, numbers, vectors and strings
  * Added processing options:
    * `:string`, `:vector`
    * Semantic testing: `(:test ...)`, `(:not ...)`
  * Rule processing is now serial
  * Subsequence parsing using `:start` and `:end` in the call to `parseq`.
  * Added rule tracing
  * Detection of left recursion
  * Added `with-saved-rules` rule namespace command.

b28cb9133f8861baddb04dd9d284dd79cab26418

## Version 0.1.0 - 2016-08-28

### Features

  * Nonterminals:
    * Ordered sequences: `(and ...)`
    * Ordered choice: `(or ...)`
    * Negation: `(not ...)`
    * Greedy repetition: `(* ...)`
    * Greedy positive repetition: `(+ ...)`
    * Optional: `(? ...)`
    * Followed-by: `(& ...)`
    * Not-followed-by: `(! ...)`
    * Nested list: `(list ...)`
  * Terminals:
    * `symbol`, `form`
    * quoted symbols for specific symbols
  * Processing options:
    * Transformation: `(:constant ...)`, `(:destructure ...)`, `(:lambda ...)`, `(:function ...)`, `(:identity ...)`, `(:flatten)`
    * Variable binding: `(:let ...)`, `(:external ...)`
  * Parsing rule parameters
  * Separate namespace for parsing rules
  * Local namespaces for parsing rules

b165f6c8f27b590013164a1ae9408c4d3ff4c10c
