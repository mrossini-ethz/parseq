# PARSEQ

## Description
Parseq (pronounced [parsec](https://en.wikipedia.org/wiki/Parsec)) is a package for common lisp.
It can be used for parsing sequences: strings, binary data, lists and vectors.
Furthermore parseq is able to parse nested structures such as trees (lists of lists).
Parsing is controlled through grammar rules that can be defined through a simple interface and the rules can be parametrised.
In addition, the parsing result can be transformed and the grammar made context aware.

The package is inspired by [Esrap](https://nikodemus.github.io/esrap/) and uses a similar interface.
No code is shared between the two projects, however.
The features of esrap are are mostly included in parseq and complemented with additional, orthogonal features.
Any resemblance to [esrap-liquid](https://github.com/mabragor/esrap-liquid) is merely coincidental.

The package is still under development which means that some features are not yet implemented, that there may be a lot of bugs and that the interface and behaviour may change in the future. See the warnings below.

### Features
Parseq provides the follwing features:

 * parses strings, binary data, vectors and lists
 * allows for sequences within sequences (e.g. trees, strings within lists, ...)
 * simple interface, very similar to [Esrap](https://nikodemus.github.io/esrap/)
 * provides many specific and non-specific terminals
 * includes a large set of nonterminals
 * result transformation defined in parse rules
 * can be made context aware:
   * run parse result through lisp code for verification
   * share data between parse rules
 * parametrize parsing rules
 * uses separate namespace(s) for parse rules

## Basic usage

First, grammar rules should be defined:
```
(defrule foo () 'foo)
(defrule bar () 'bar)
(defrule foobar () (and foo bar))
```
The first argument to `defrule` is name of the nonterminal that the rule represents.
These names use a different namespace from everything else.
The second argument is a list of arguments that the rule takes (none in this example).
The third argument specifies what the terminal symbol should expand into.
In this example, the nonterminal `foo` expands into the lisp symbol `foo`.
The rule `foobar` combines the two rules `foo` and `bar` and matches the list `(foo bar)` or the vector `#(foo bar)`.
The above example could alternatively be stated as
```
(defrule foobar () (and 'foo 'bar))
```
thus not requiring the rules `foo` and `bar`.

Parsing is initiated by calling
```
(parseq 'foobar '(foo bar))
```
which will return the list `(foo bar)` as well as `T` for success.
If the parse is not successful, `NIL` is returned.

This concludes the basic usage of the package. Everything is done through `defrule` and `parseq`.
There are some extra arguments, however, that are explained below.

## Terminals
Terminal are the objects that the parse rules finally expand into.

 * `symbol` stands for any lisp symbol
 * `form` matches literally everything
 * `char` matches any character
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

More terminals may be available in later versions of parseq.

## Nonterminals
The following nonterminals are available:

### Rules
Any rule defined with `defrule` is a nonterminal and can be used through its name.

### Sequence (ordered)
```
(and subexpression ...)
```
The expression succeeds for a sequence if all subexpressions succeed in order.
It produces a list of the subexpression results.

### Sequence (unordered)
```
(and~ subexpression ...)
```
The expression succeeds for a sequence if all subexpressions succeed, in any order.
It produces a list of the subexpression results (in the order in which they are listed in the rule definition).

There is a variant of `and~` that is more flexible:
```
(and~~ (1 2 (1) (2 3) (4 nil) ...) subexpr-1 subexpr-2 subexpr-3 subexpr-4 subexpr-5 ...)
```
The first argument to `and~~` specifies how may times a subexpression is allowed to be repeated in the unordered sequence.
In this example, the first subexpression is required exactly once, the second one exactly twice, the third zero times or once (`(N)` is short for `(0 N)`), the fourth between 2 and 3 times and the fifth at least 4 times.
The result is a list of lists:
The list is ordered in the same way that subexpressions are given in the rule definition.
The *n*-th list within the list contains the results of the *n*-th subexpression in the order in which they are found in the parsed expression.

### Ordered choice
```
(or subexpression ...)
```
The subexpressions are tried and the result of the first one that succeeds is returned.

### Negation
```
(not subexpression)
```
Succeeds, if the subexpression _does not_ succeed.
When that happens, the rule consumes one item in the sequence and returns it.

### Greedy repetition
```
(* subexpression)
```
Tries subexpression consecutively as many times as possible.
Always succeeds because zero repetitions are allowed.
Returns a list of the succeeding matches.

### Greedy positive repetition
```
(+ subexpression)
```
Like `(* subexpression)`, but at least one repetition is required.

### Repetition
```
(rep 5 subexpression)
(rep (5) subexpression)
(rep (2 5) subexpression)
```
Succeeds, if subexpression matches exactly 5 times, up to 5 times, or between 2 and 5 times, respectively.
Returns a list of the successful results.

### Optional
```
(? subexpression)
```
Consumes and returns whatever subexpression consumes if it succeeds.
If the subexpression fails, no input is consumed and `NIL` returned.

### Followed-by
```
(& subexpression)
```
Succeeds, if subexpression succeeds, but consumes no input.
The result of the subexpression is returned.

### Not-followed-by
```
(! subexpression)
```
Succeeds if subexpression _does not_ succeed.
Consumes no input and the subexpression which _does not_ match the subexpression is returned.

### Nesting
```
(list subexpression)
(string subexpression)
(vector subexpression)
```
Succeeds if the current item in the sequence is a list/string/vector and its content matches the subexpression.
Returns a list with the subexpression result.

## Rule arguments
Often, rules are similar to each other. For example
```
(defrule html-a () "<a>")
(defrule html-b () "<b>")
(defrule html-span () <span>")
```
can be shortened to
```
(defrule html-tag (name) (and "<" name ">")
```
and called through `(html-tag "a")` instead of calling `html-a`.
Multiple arguments are possible.
The lambda list specified in `defrule` may be destructured in the future.

## Processing options
The result from a parsing rule can be processed.
Multiple options can be specified in the call to `defrule`.
Example:
```
(defrule abcde (and (and a b) c (and d e)))
```
would normally return `((a b) c (d e))` when parsing `(a b c d e)`.
Calling
```
(defrule abcde (and (and a b) c (and d e)) (:flatten))
```
yields `(a b c d e)`.

You can specify how processing of the parsing result is done through `:constant`, `:lambda`, `:destructure`, `:function`, `:flatten`, and `:identity`.
Additional options (`:test`, `:not`, `:let`, `:external`) do not affect the parse result, but have other effects.
Note that the options are processed in sequence and the output of the previous option is input into the next option:
```
(defrule int++ form (:lambda (x) (1+ x)) (:lambda (x) (1+ x)))
```
This would return `4` if the input was `(2)`, for example.

### Transformation of parse results

#### Constant result
```
(:constant 1)
```
This options returns `1` (or whatever you specify) if the rule succeeds, irrespective of the parsing result.

#### Lambda / Destructure
```
(:lambda (x) ...)
(:destructure (a b (c d) &rest z) ...)
```
Destructures the parsing result according to the specified lambda list and binds the given variables.
The following forms can be used to process the result in whatever way.
The new parsing result is given by what the last form returns.
`:lambda` and `:destructure` are actually one and the same thing.

#### Function
```
(:function func)
```
The parsing result is handed over to the function specified (here: `func`).
Note that the lambda list of the given function has to match the number of items in the parsing result.

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
Flattens the parsing result and concatenates the list items to into a string, i.e. `(#\a (#\b #\c (#\d) #\e) (#\f #\g))` becomes `"abcdefg"`.
The list items that can be concatentated are strings, characters and symbols.

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
Makes the rule fail if the last form specified in the test fails.
In this case, the rule fails if the parse result is not an integer greater than `10`.
Note, that the input to the test depends on the preceding processing options.

#### Antitest
```
(:not (x) (and (numberp x) (> x 10)))
```
Same as `:test`, but logically inverted.
In this case, the rule fails _if_ the parse result is an integer greater than `10`.

### Variables
Rules can bind variables that can be accessed/modified by subexpressions.

#### Variable binding
```
(:let a b (c 10))
```
Binds the specified variables.
Subexpressions (and subexpressions of subexpressions, etc) of the rule have access to these variables and can even modify them.
In order to do this they have to declare them using `:external` (see below).

#### External bindings
```
(:external a b c)
```
Declares the specified variables.
If the rule is called by a superior rule that binds these variables (using `:let`, see above), this rule can use and modify the variables.

## Using context
Parse rules in parseq can be made context aware.
The processing options `:test` and `:not` control, whether a parse result is accepted or not.
For instance, the following rule matches any symbol except `baz`:
```
(defrule no-baz () symbol (:not (x) (eql x 'baz)))
```
The tests can be as complex as needed.

In order to make one rule affect another rule, variable bindings can be used.
Suppose the binary format of a file specifies that a string is stored as a byte indicating the length of the string followed by the string characters.
A set of rules to parse this would be:
```
(defrule length () byte (:external len) (:lambda (x) (setf len x)))
(defrule chars () (rep len byte) (:external len))
(defrule string () (and byte length) (:let len))
```
If a subexpression binds the same variable with another `:let`, the previous binding is shadowed until the subexpression is completed.

## Rule tracing
Rules can be traced by calling
```
(trace-rule 'rule-name)
```
which will print the information to standard output.
If the keyword argument `:recursive` is set to `T`, all rules called within the given rule will be traced as well.
Tracing can be turned off by calling
```
(untrace-rule 'rule-name)
```

## Namespaces
You can use local namespaces for rule names:
```
(with-local-rules
  (defrule ...)
  (defrule ...)
  (parseq ...))
```
Within the body of `with-local-rules` new rules can be defined that will be invisible outside.
Also, outside rules will be invisible within the body.
Instead of `with-local-rules` the macro `with-saved-rules` can be used.
Within its body, rules from outside are still defined and can be redefined without affecting the outside:
The rules from outside are saved before entering the body and restored when the body returns.

## Upcoming features
These features _may_ be implemented in the future:

 * short forms for combined nonterminals, e.g.
   * `(? (and ...))`
   * `(? (or ...))`
 * support for streams
 * custom terminals
 * custom non-terminals
 * custom sequences, i.e. parse _anything_

## Warnings
Please heed the following warnings:

 * The interface and behaviour of parseq are not yet frozen. New versions may break programs using the package. If you intend to use the package, please let me know so I can inform you of any changes.
 * The package should work with SBCL, CMUCL and ECL. Other lisps are untested.
 * Parseq comes with no warranty whatsoever.

## Licence
Parseq is distributed with the GNU General Public License, version 2.
You can find it [here](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html).

## Questions/Bugs/Help/Requests/Feedback etc.

If you have questions regarding parseq, found any bugs, would like to offer help, have a feature request, give feedback etc., feel free to contact me by E-Mail.
You can find my address by cloning the repository from GitHub and looking through the log.
