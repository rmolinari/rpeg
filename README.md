# RPeg

RPeg is a Ruby port of [LPeg](http://www.inf.puc-rio.br/~roberto/lpeg/), Lua's pattern-matching library based on
[Parsing Expression Grammars](https://en.wikipedia.org/wiki/Parsing_expression_grammar) (PEGs).

This project doesn't contain documentation of the library's functionality. For that, see the LPeg page, keeping in mind the
differences in the Ruby port, described below. For a theoretical justification of the use of PEGs for pattern matching and a lot of
detail of the internal design of LPeg, see [[Ierusalimschy]](#refereces).

## Why You Should Use RPeg

PEGs are flexible and expressive and, once complexity reaches a certain level, tend to be much more readable than regular
expressions. PEGs are also more powerful than regular expressions, though the various ad hoc extensions to regexes - such as in
PCRE - close the gap. The LPeg documentation and the Wikipedia article give some examples of what is possible.

Being able to use and combine patterns as Ruby objects allows us to build up complex patterns step by step. This makes the code
easier to read and maintain.

## Why You Should Not Use RPeg

I wrote RPeg as learning exercise and for my own illumination. I was interested in how regular expressions can be implemented
efficiently using a virtual machine (see [[Cox]](#references)) and stumbled on Ierusalimschy's paper. I found that paper fascinating
and decided to try to implement the algorithm in Ruby.

### It is slow

Very slow.

Ruby is interpreted language. So is Lua, but almost all of LPeg is implemented in C, and this makes LPeg very fast. Ierusalimschy's
paper, from 2008, states that LPeg can search a large string (the full text of the King James Bible) for "Alpha " in about 40
milliseconds. RPeg, on more modern hardware[^1], takes 5.4 seconds (!) for the same task. I have profiled my code as best I can and
don't think it will get any faster.

Of course, Ruby can call C code just as well as Lua can, but I am not going to attempt to write RPeg in C. The LPeg code is very
carefully written to do all of the necessary memory managment, and it gets especially complicated in the implemention of "runtime
captures". I have no interest in attempting this for RPeg.

### It is not industrial-strength

As much as I could I implemented LPeg as described in the Ieuraselimschy paper, but this only got me so far. There is a great deal
of cleverness in LPeg, performing optimizations when a pattern is compiled for the bespoke VM, when analyzing patterns for errors,
and for dozen of other things. So, most of RPeg's code was written while carefully reading the LPeg sources. This was mostly
educational, but in a few cases I simply couldn't understand what the LPeg code was doing, and was reduced to blindly following the
logic step-by-step, without a clear picture of what was "really" going on. This was unsatisfying, and left me worried about the
soundness of my code.

I have ported most of LPeg's (extensive) test suite and it all passes, but this is not a battle-hardened product.

While I have made efforts to follow LPeg's functionality as closely as I can, all bugs in RPeg are my responsibility.

## Using RPeg

Patterns in RPeg are much like they are in LPeg.

``` ruby
    require 'rpeg'

    # Pattern to match strings of balanced parentheses
    patt1 = RPEG.P( [ "(" * ((1 - RPEG.S("()")) + RPEG.V(0))**0 * ")" ] )
    patt2 = patt1 * -1

    puts patt2.match "(()()(()))"  # 10
    puts patt2.match "(()()(())"  # nil, i.e., no match
```

The examples in the LPeg documentation will work once modified for the syntax of RPeg.

TODO: add some actual RPeg examples.

## Differences Between RPeg and LPeg

Efforts have been made to keep RPeg's syntax as close to LPeg's as possible. But there are necessarily some differences enforced by
Ruby.

### Indexing

Lua indexes strings and arrays (tables) from 1, while Ruby indexes from zero. RPeg follows the Ruby way. This means that

- `match` functions return the Ruby-style index of the end of the matched substring
- "open" rules in grammars using numeric references use 0-indexing
- other contexts in which an integer is used as index - such as argument captures - are 0-indexed

### 'And' patterns

Given a pattern `p`, RPeg forms its "and" pattern using `+p` where LPeg uses `#p`.

Using unary `+` doesn't read very well in practice, even though unary `-` is OK for "not" patterns. I think this is because
binary `+` is much more common in patterns than binary `-`. But the other unary operators are no good.

- `&` is a unary operator in Ruby but the parser appears to restrict it to syntactic sugar for `#to_proc`.
- `-` is used for "not" pattern formation.
- `!` must be left untouched as a logical operator.
- `~` works, but is too easy to mistake for `-`.

### Grammars

Grammars are represented in LPeg with Lua tables, which are sort of a cross between arrays and hash tables. After some
experimentation, RPeg allows grammars to be specified using an array or a hash table.

If an array is given then the nonterminals aren't named and all open calls must use numeric indices. The first element of the
array is either

- a non-negative integer 0, 1, 2, ... and specifies the (rule of the) initial nonterminal among the remaining elements with
  indices reckoned _without_ that initial integer
- something else, which is interpreted as the pattern for the initial nonterminal

Otherwise the grammar is defined with a Hash. The keys are the nonterminal symbols and the values are the rule patterns.

- the keys must be symbols or strings (which are converted to symbols). No rule can use :initial or "initial" as
  nonterminal.
- the open calls can refer either to the nonterminals (as strings or symbols) or to rule indices as they appear in the hash,
  ignoring the :initial key (if present)
- :initial/"initial" can appear as a key in the hash and its value specifies the initial nonterminal.
  - if it is a non-zero integer it gives the index of the initial terminal's rule, reckoned without the presence of the :initial
    key itself.
  - if it is a symbol or a string it specifies the initial nonterminal directly

TODO: some examples

### Table captures

Table captures - defined with `#Ct` - return instances of a special `TableCapture` class, which mimics a small part of Lua's table
functionality. Other approaches have been tried and haven't worked well.

### Function captures

Various kinds of captures involve calling a function (proc) provided by client code. For example, the construction `patt / fn` takes
the captures made by patt and passes them as arguments to fn. Then the values returned by fn become the captures of the
expression.

Lua is better than Ruby at distinguishing between a function that returns multiple values and one that returns a single value that
is an array. In RPeg, returns from function in contexts like this are treated as follows:

- `[1, 2, 3]`: multiple captures, 1, 2, 3.
  - this is the natural interpretation as it's the standard way that a Ruby function returns multiple values
- `[[1, 2, 3]]`: a single capture that is the array `[1, 2, 3]`.
- nil: no captures
  - even if the function says something like "return nil", the capture code has no way to distinguish between that and a
    function that returns nothing
- `[nil]`: a single capture with value nil
  - the weirdest case, but I don't see an alternative
- otherwise, the single value returned by the function is the single captured value.

## TODOs

- make this into a useable README
- turn the code into a gem


# References
- [Ierusalimschy] Ierusalimschy, R., _Text Pattern-Matching Tool based on Parsing Expression Grammars_, Software: Practice and Experience, 39(3):221-258, Wiley, 2009, https://doi.org/10.1002/spe.892, http://www.inf.puc-rio.br/~roberto/docs/peg.pdf (retrieved 2022-01-??).
- [Cox] Cox, R., _Regular Expression Matching: the Virtual Machine Approach_, https://swtch.com/~rsc/regexp/regexp2.html.


[^1]: A 2016 Macbook Pro
