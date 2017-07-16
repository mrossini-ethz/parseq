# PARSEQ

## Description
Parseq (pronounced [parsec](https://en.wikipedia.org/wiki/Parsec)) is a parsing library for common lisp.
It can be used for parsing lisp's sequences types: strings, vectors (e.g. binary data) and lists.
Furthermore, parseq is able to parse nested structures such as trees (e.g. lists of lists, lists of vectors, vectors of strings).

Parseq uses [parsing expression grammars](https://en.wikipedia.org/wiki/Parsing_expression_grammar) (PEG)
that can be defined through a simple interface.
Extensions to the standard parsing expressions are available.
Parsing expressions can be parameterised and made context aware.
Additionally, the definition of each parsing expression allows the arbitrary transformation of the parsing tree.

The library is inspired by [Esrap](https://nikodemus.github.io/esrap/) and uses a very similar interface.
No code is shared between the two projects, however.
The features of Esrap are are mostly included in parseq and complemented with additional, orthogonal features.
Any resemblance to [esrap-liquid](https://github.com/mabragor/esrap-liquid) is merely coincidental.

The library is still under development.
This means that some features are not yet implemented and that the interface and behaviour may change in the future.
See the warnings [below](#warnings).

### Features
Parseq provides the following features:

 * Parses strings, vectors (e.g. binary data) and lists
 * Allows parsing of sequences within sequences (e.g. trees, strings within lists, ...)
 * Simple interface, very similar to [Esrap](https://nikodemus.github.io/esrap/)
 * Provides many specific and non-specific terminal symbols
 * Implements the standard [PEG expressions](https://en.wikipedia.org/wiki/Parsing_expression_grammar) as well as useful extensions
 * [Packrat parsing](https://github.com/mrossini-ethz/parseq/wiki/Packrat-Parsing) can be enabled for individual PEG rules
 * Parsing expression rules are compiled
 * Parse tree transformations can be defined together with each PEG rule
 * Grammars can be made context aware:
   * Run parse results of a PEG rule through lisp code to influence parsing success
   * Share data between parse rules
 * Parsing rules can be parameterised
 * Uses separate namespace(s) for parse rules
 * Tracing of grammar rules
 * Meaningful parse error messages

## Basic usage

First, define a set of grammar rules:
```
(defrule foo () "foo")
(defrule bar () "bar")
(defrule foobar () (and foo bar))
```
The first argument to `(defrule ...)` is the nonterminal symbol that will represent the rule.
These symbols use a different namespace from everything else.
The second argument is a list of parameters that the rule takes (none in this example).
The third argument specifies the definition of the nonterminal symbol.
After the third argument, multiple [processing options](#processing-options) can be listed.

In this example, the nonterminal `foo` requires the parsing of the string `"foo"`.
The rule `foobar` combines the two rules `foo` and `bar` to match the string `"foobar"`, the list `("foo" "bar")` or the vector `#("foo" "bar")`.
The above example could alternatively be stated as
```
(defrule foobar () (and "foo" "bar"))
```
thus not requiring the rules `foo` and `bar`.

Parsing is initiated by calling
```
(parseq 'foobar sequence)
```
which will return the list `("foo" "bar")` as well as `T` if `sequence` is one of `"foobar"`, `("foo" "bar")` and `#("foo" "bar")`.
If parsing is not successful, `NIL` is returned.
The first argument to `(parseq ...)` is a nonterminal symbol defined through `defrule`.
Note that the symbol must be quoted.
The second argument is the sequence that should be parsed.
There are optional keyword parameters to `(parseq ...)`:

  * `:start`: the position in the sequence at which parsing should start
  * `:end`: the position in the sequence at which parsing should stop
  * `:junk-allowed`: when set to `t`, will avoid a parsing error, if the end is not reached
  * `:parse-error`: when set to `t`, a parsing error will signal a error

This concludes the basic usage of the library. Almost everything is done through `defrule` and `parseq`.
There are some extra arguments, however, that are explained below.

## Installation
Parseq is available with [quicklisp](https://www.quicklisp.org/beta/).
You can run
```
(ql:quickload :parseq)
```
in the REPL to download, install and load it.
To access the symbols from the package without the `parseq:` prefix you can type
```
(use-package :parseq)
```

Alternatively, the system is can be downloaded/cloned from [GitHub](https://github.com/mrossini-ethz/parseq).
The `master` branch will always point to the latest release.
If the system can be found through [ASDF](https://common-lisp.net/project/asdf/), the system can be loaded by typing the following expressions in the REPL:
```
(require :asdf)                ; unless already loaded
(asdf:load-system :parseq)
(use-package :parseq)          ; optional
```

## Terminals
Terminals (tokens) are the objects that actually appear in the parsed sequence.
The following types are item classes:

 * `symbol` stands for any lisp symbol
 * `form` matches literally everything
 * `char` matches any character
 * `stdchar` matches any standard character, see `(standard-char-p ...)`
 * `alpha` matches any alphabetic standard character
 * `digit` matches any numeric standard character
 * `alphanumeric` matches any alphanumeric standard character
 * `byte` matches any unsigned byte
 * `number` matches any number
 * `list` matches any list
 * `vector` matches any vector
 * `string` matches any string
 * `t` matches anything not `nil`
 * `nil` matches `nil` or an empty list

Literal values can be specified to match specific items or subsequences in the sequence being parsed:

 * `'foo` matches the symbol `foo`
 * `#\f` matches the character 'f'
 * `"foo"` matches the subsequence "foo" in a string or the item `"foo"` in a list or vector
 * `#(1 2 3)` matches the the subsequence `#(1 2 3)` in a vector or the item `#(1 2 3)` in a list
 * `5` matches the number 5

Terminal expressions allow for mor elaborate specifications:

 * `(char "a-zA-D7-9.,;<=>-")` matches any character from the characters or character ranges in the
   given string

More terminals may be available in later versions of parseq.

## Nonterminals
Nonterminal symbols represent parsing expressions that consist of terminals and/or other nonterminals.
They can be defined using the `defrule` macro.

## Standard expressions
These are the standard combinations of a parsing expression grammar.

### Sequence
```
(and subexpression ...)
```
The expression succeeds for a sequence if all subexpressions succeed in the given order.
It produces a list of the subexpression results.
On success, the amount of input consumed is determined by the subexpressions.

### Ordered choice
```
(or subexpression ...)
```
The subexpressions are tried in the given order and the result of the first one that succeeds is accepted.
It produces the result of the succeeding subexpression.
The amount of input consumed depends on the subexpression.
If none of the subexpressions match, the expression fails.

### Greedy repetition
```
(* subexpression)
```
Tries subexpression consecutively as many times as possible.
This expression always succeeds because zero repetitions are allowed.
A list of the succeeding matches is returned.
The amount of input consumed depends on the subexpression and the number of times it matches.

### Greedy positive repetition
```
(+ subexpression)
```
Like `(* subexpression)`, but at least one repetition is required.

### Optional
```
(? subexpression)
```
Consumes and returns whatever subexpression consumes if it succeeds.
This operation is greedy which means that if the subexpression matches, it will consume the corresponding input.
If the subexpression fails, no input is consumed and `NIL` returned.

### Followed-by predicate
```
(& subexpression)
```
Succeeds, if subexpression succeeds, but consumes no input.
The result of the subexpression is returned.

### Not-followed-by predicate
```
(! subexpression)
```
Succeeds if subexpression _does not_ succeed.
This expression consumes no input.
When successful, the next sequence item is returned.

## Extended expressions
For convenience, the standard expressions are extended by the following combination methods:

### Repetition
```
(rep 5 subexpression)
(rep (0 5) subexpression)
(rep (2 5) subexpression)
(rep (2 NIL) subexpression)
```
Succeeds, if subexpression matches exactly 5 times, up to 5 times, between 2 and 5 times, or at least 2 times, respectively.
Returns a list of the successful results.

The following abbreviations are allowed for repetitions:

| Abbreviation | Long notation | Meaning             |
| ------------ |:-------------:| ------------------- |
| `3`          | `(3 3)`       | Exactly 3 times     |
| `(3)`        | `(0 3)`       | Up to 3 times       |
| `*`          | `(0 NIL)`     | Any number of times |
| `+`          | `(1 NIL)`     | At least once       |
| `?`          | `(0 1)`       | Zero times or once  |

### Negation
```
(not subexpression)
```
Succeeds, if the subexpression _does not_ succeed.
When that happens, the rule consumes _exactly one_ item in the sequence and returns it.

### Sequence (unordered)
```
(and~ subexpression ...)
```
The expression succeeds for a sequence if all subexpressions succeed, in any order.
Note that the first subexpression in the list that matches a sequence item will be used.
The expression produces a list of the subexpression results (in the order in which they are listed in the rule definition) and consumes whatever the subexpressions consume.

An expression like `(and~ 'a 'b 'c 'd)` is actually equivalent to
```
(or (and 'a (or (and 'b (or (and 'c 'd) (and 'd 'c)))
                (and 'c (or (and 'b 'd) (and 'd 'b)))
                (and 'd (or (and 'b 'c) (and 'c 'b)))))
    (and 'b (or (and 'a (or (and 'c 'd) (and 'd 'c)))
                (and 'c (or (and 'a 'd) (and 'd 'a)))
                (and 'd (or (and 'a 'c) (and 'c 'a)))))
    (and 'c (or (and 'a (or (and 'b 'd) (and 'd 'b)))
                (and 'b (or (and 'a 'd) (and 'd 'a)))
                (and 'd (or (and 'a 'b) (and 'b 'a)))))
    (and 'd (or (and 'a (or (and 'b 'c) (and 'c 'b)))
                (and 'b (or (and 'a 'c) (and 'c 'a)))
                (and 'c (or (and 'a 'b) (and 'b 'a))))))
```

There is a variant of `and~` that is more flexible:
```
(and~~ (1 2 (1) (2 3) (4 nil) ...) subexpr-1 subexpr-2 subexpr-3 subexpr-4 subexpr-5 ...)
```
The first argument to `and~~` specifies how may times each subexpression is allowed to be repeated.
(See the `rep` operator above for a list of abbreviations.)
In this example, the first subexpression is required exactly once, the second one exactly twice, the third zero times or once, the fourth between 2 and 3 times and the fifth at least 4 times.
During parsing, the prioritisation of the subexpressions that are still applicable is from left to right.
The result is a list of lists:
The list is ordered in the same way that subexpressions are given in the rule definition.
The *n*-th list within the list contains the results of the *n*-th subexpression in the order in which they are found in the parsed expression.

### Nesting
```
(list subexpr-1 subexpr-2 ...)
(string subexpr-1 subexpr-2 ...)
(vector subexpr-1 subexpr-2 ...)
```
Succeeds if the current item in the sequence is a list/string/vector and its content matches the subexpressions in sequence.
Returns a list enclosing the subexpression results.

## Rule parameters
Often, rules are similar to each other. For example
```
(defrule html-a () "<a>")
(defrule html-b () "<b>")
(defrule html-span () <span>")
```
share the same pattern.
In parseq, this can be generalised with
```
(defrule html-tag (name) (and "<" name ">")
```
and used in parsing expressions as `(html-tag "a")` instead of having to define each rule separately.
The parameters can also be used in the parse options (see below).
Note, however, that since the rule parameter is unknown at compile time, a runtime dispatch will need to be performed on the parameter type.

It is possible to pass multiple paramters, keywords etc.
The full syntax of lambda lists is allowed.

## Processing options
There are several options that can be specified for each rule definition.
The options can have several effects which are described below.

### Transformation of parse results
The result from a parsing rule can be processed.
Example:
```
(defrule abcde () (and (and 'a 'b) 'c (and 'd 'e)))
(parseq 'abcde '(a b c d e))
```
would normally return `((a b) c (d e))`.
To process the result, options can be specified in the call to `defrule`.
For example, if you want the resulting list flattened, the rule can be altered to
```
(defrule abcde () (and (and 'a 'b) 'c (and 'd 'e)) (:flatten))
```
such that parsing `(a b c d e)` yields `(a b c d e)` instead of `((a b) c (d e))`.

You can specify how processing of the parsing result is done through multiple options (see below).
Additional options (such as `:test`) do not affect the parse result, but have other effects.
Note that the options are processed in sequence and the output of the previous option is input into the next option:
```
(defrule int+int^2 () (and number number) (:lambda (x y) (+ x y)) (:lambda (x) (expt x 2)))
```
This would return `25` when parsing the list `(2 3)`.

#### Constant result
```
(:constant 1)
```
This options returns `1` (or whatever you specify) if the rule succeeds, irrespective of the parsing result.

#### Lambda / Destructure
```
(:lambda (a b (c d) &rest z) ...)
(:destructure (a b (c d) &rest z) ...)
```
Destructures the parsing result according to the specified lambda list and binds the given variables.
The following forms can be used to process the result in whatever way.
The new parsing result is given by what the last form returns.
Note that `:lambda` and `:destructure` are actually synonyms.

#### Choose
```
(:choose 0 2 '(3 1))
```
Picks items from the parsing result using the given indices and returns them.
For the parsing result `(15 #\: 24 (#\: 57))` (which might be the parsing result of a time string), the above processing option would return `(15 24 57)`.
Non-existent items will result in `NIL` being returned for them.

#### Function
```
(:function #'+)
```
The parsing result is handed over to the function specified (here: `+`).
Note that the lambda list of the given function has to match the items in the parsing result.
The new parsing result is whatever the function returns.

#### Identity
```
(:identity t)
```
Returns the parsing result if the argument is not `NIL` and `NIL` otherwise.

#### Flatten
```
(:flatten)
```
Flattens the parsing result, i.e. `(a (b c (d) e) (f g))` becomes `(a b c d e f g)`.

#### String
```
(:string)
```
Flattens the parsing result and concatenates the list items to into a string, i.e. `(#\a ("bc" (#\d) 'e) (#\f #x67))` becomes `"abcdEfg"`.
The items that can be concatenated are strings, characters, unsigned bytes and symbols.

#### Vector
```
(:vector)
```
Flattens the parsing result and converts the resulting list into a vector, i.e. `(a (b c (d) e) (f g))` becomes `#(a b c d e f g)`.

### Parse result testing
These options do not affect the parse result, but can make the rule fail depending on their input.
If a rule fails because of such an option, the processing of subsequent options is skipped.

#### Test
```
(:test (x) (and (numberp x) (> x 10)))
```
Like `:lambda` or `:destructure`, except that the return value of the function body is used as a predicate to determine whether the rule succeeds or not.
Therefore, if the body of the test returns NIL, the rule fails.
Otherwise, the unaltered result is returned.
Note that the input to the test (function arguments) depends on the preceding processing options.

In the above example, the rule fails if the parse result is not an number greater than `10`.

The following rule matches any symbol except `baz`:
```
(defrule not-baz () symbol (:not (x) (eql x 'baz)))
```
This is not possible with `(not 'baz)` because that would allow terminals other than symbols, e.g. `5` (which is not a symbol).

#### Reverse test
```
(:not (x) (and (numberp x) (> x 10)))
```
Same as `:test`, but logically inverted.
In this example, the rule fails _if_ the parse result is an number greater than `10`.

### Variables
Rules can bind variables that can be accessed/modified by subexpressions.

#### Variable binding
```
(:let a b (c 10))
```
Binds the specified variables (dynamically).
Subexpressions (and subexpressions of subexpressions, etc) of the rule have access to these variables and can even modify them.
In order to access the variables, the subexpressions have to declare them using `:external` (see below).
If a subexpression binds the same variable with another `:let`, the previous binding is shadowed until the subexpression returns.
For an example of variable usage, see the section 'Using context' below.

#### External bindings
```
(:external a b c)
```
Declares the specified variables.
If the rule is called by a superior rule that binds these variables (using `:let`, see above), this rule can use and modify the variables.
It is an error if a rule using external variables is called when the variables are unbound (i.e. the rule must be called as a subexpression to a rule defining the variables).

### Packrat parsing
Packrat parsing can be enabled for a rule by using the `(:packrat t)` option.
See the [wiki page](https://github.com/mrossini-ethz/parseq/wiki/Packrat-Parsing) for more information.

## Using context
Parsing in parseq can be made context aware using two methods.

### Context through tests
A single rule can verify one part of its result against another.
For instance, if a tag is a name followed by a type and a value, then the rule
```
(defrule tag () (and string symbol form) (:test (name type value) (eql (type-of value) type)))
```
will check whether the value is indeed of the specified type. Otherwise the rule will fail.

### Context through variables
It is possible to make rules depend on the results of other rules.
This can be achieved through external variable bindings that are introduced with `(:let ...)` and used with `(:external ...)`.

Suppose the binary format of a file specifies that a string is stored as a byte indicating the length of the string followed by that number of characters.
A set of rules for parsing this could be:
```
(defrule string () (and length chars) (:let len))
(defrule length () byte (:external len) (:lambda (x) (setf len x)))
(defrule chars () (rep len byte) (:external len))
```
External variables can also be used from within `(:test ...)` or `(:not ...)` or even the parse expression.

## Rule tracing
Rules can be traced by calling
```
(trace-rule 'nonterminal-symbol)
```
which will print the information to standard output.
With the keyword argument `:recursive t`, all rules called from within the given rule will be traced as well.
Tracing can be turned off by calling
```
(untrace-rule 'nonterminal-symbol)
```

## Namespaces
You can use local namespaces for nonterminal symbols:
```
(with-local-rules
  (defrule ...)
  (defrule ...)
  (parseq ...))
```
Within the body of `with-local-rules` new rules can be defined that will be invisible outside.
Also, outside rules will be invisible within the body.

Instead of `with-local-rules` the macro `with-saved-rules` can be used:
```
(defrule a ...)
(defrule b ...)
(defrule ab () (and a b))
(with-saved-rules
  (defrule b ...)
  (parseq 'ab ...))
(parseq 'ab ...)
```
Within its body, rules from outside are still defined and can be redefined without affecting the outside.
The rules from outside are saved before entering the body and restored when the body returns.

## Upcoming features
These features _may_ be implemented in the future:

 * Short forms for combined nonterminals, e.g.
   * `(? (and ...))`
   * `(? (or ...))`
   or multiple arguments to `(? ...)` signifying either a sequence or a choice.
 * Support for streams (how?)
 * Speed and efficiency
 * Custom terminals
 * Custom non-terminal expressions
 * Custom sequences, i.e. parse _anything_

## Warnings
Please heed the following warnings:

 * The interface and behaviour of parseq are not yet frozen.
   New versions may break programs using the library.
 * The library should work with SBCL, CMUCL, ECL and CLISP.
   Other lisp implementations are untested.
 * Parseq comes with no warranty whatsoever.

## Licence
Parseq is distributed with the GNU General Public License, version 2:

Copyright (C) 2017 Marco Rossini

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License version 2 as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

You can also find the full licence [online](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html).

## Questions/Bugs/Help/Requests/Feedback etc.

If you have questions regarding parseq, found any bugs, would like to offer help, have a feature request, give feedback etc., feel free to contact me through GitHub.
