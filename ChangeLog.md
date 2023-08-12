# Change log

Parseq uses [semantic versioning](http://semver.org/).

## Version 0.6.0 - 2023-08-12:

### Features

  * Added methods to add custom terminals and operators
  * Introduced implicit OR function for operators `*`, `+`, `?`, `&`, `!`, `not` and `rep`.

### Bugfixes

  * Fixed backtracking for `and~` and `and~~` operators.
  * Runtime dispatch now more complete.
  * Fixed problem with `?` operator symbol.

### Other

  * Added a calculator/compiler example
  * Changed the implementation architecture to allow redefinition of terminals and operators.

## Version 0.5.1 - 2021-04-18:

### Bugfixes

  * Fixed string matching for different string types.

### Other

  * Now complying to ASDF's new system name requirements.

## Version 0.5.0 - 2017-07-25:

### Features

  * New terminals `stdchar`, `alpha`, `digit`, `alphanumeric`, `keyword`, `atom`, `cons` and `integer`.
  * New terminal `(char ...)` that accepts a character from the
    characters or character ranges in the string argument.
  * Added the processing option `(:choose ...)` that allows to pick items from the result.
  * Extended the `(list ...)`, `(string ...)` and `(vector ...)` parsing expressions to
    accept more than one argument (implicit `AND`).
  * Reduced the overhead of rule tracing (when disabled).

### Other

  * Added examples:
    * Parsing of RFC 5322 dates
    * Parsing of PNG images (binary file)
  * Updated examples:
    * Parsing of e-mail addresses
    * Parsing of URLs

## Version 0.4.1 - 2017-07-16:

### Bugfixes

  * Fixed error reporting in several cases:
    * When a rule fails due to `(:test ...)` or `(:not ...)` processing options and there are no alternatives.
    * When there are several alternatives of terminals, the list is now printed more nicely.
    * When parsing trees, the alternatives of terminals are not dropped anymore.
  * The omission of a symbol export was fixed.

### Other

  * Added an example that parses e-mail addresses

## Version 0.4.0 - 2017-06-27:

### Features

  * Meaningful parsing error messages based on sequence position and terminal expressions.
  * Internal improvement of the tree pointer abstraction.
  * Packrat parsing, optional for each rule. No overhead when disabled.

## Version 0.3.2 - 2017-06-27:

### Bugfixes

  * Added more elaborate tests for verifying the correctness of processing options in rule definitions.

## Version 0.3.1 - 2017-06-25:

### Bugfixes

  * Fixed the parsing of numbers, characters and strings if the sequence element is of a different type.
    This now only causes the parse to fail instead of signaling an error.

## Version 0.3.0 - 2017-06-19:

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
