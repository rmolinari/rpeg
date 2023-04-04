# RPeg

RPeg is a Ruby port of [LPeg](http://www.inf.puc-rio.br/~roberto/lpeg/), Lua's pattern-matching library based on
[Parsing Expression Grammars](https://en.wikipedia.org/wiki/Parsing_expression_grammar) (PEGs).

It is distributed under the MIT license.

It is available as a gem: https://rubygems.org/gems/rpeg.

## Why You Should Use RPeg

PEGs are flexible and expressive and, once complexity reaches a certain level, tend to be more readable than regular
expressions and easier to document.   From the LPeg docs:

> On the one hand, the result is usually much more verbose than the typical encoding of patterns using the so called regular
> expressions (which typically are not regular expressions in the formal sense). On the other hand, first-class patterns allow much
> better documentation (as it is easy to comment the code, to break complex definitions in smaller parts, etc.) and are extensible,
> as we can define new functions to create and compose patterns.

PEGs are also more powerful than regular expressions, though the various ad hoc extensions to regexes - such as in PCRE - close the
gap. The LPeg documentation and the Wikipedia article give some examples of what is possible.

For a theoretical justification of the use of PEGs for pattern matching and many details of the internal design of LPeg, see
[[Ierusalimschy]](#references).

## Why You Should Not Use RPeg

I wrote RPeg as learning exercise and to satisfy my curiosity. I was interested in how regular expressions can be implemented
efficiently using a virtual machine - see [[Cox]](#references) - and stumbled on Ierusalimschy's paper. I found that paper
fascinating and decided to try to implement the algorithm in Ruby.

### It is slow

Very slow.

Ruby is an interpreted language. So is Lua, but almost all of LPeg is implemented in C, and this makes LPeg very
fast. Ierusalimschy's paper, from 2008, states that LPeg can search a large string (the full text of the King James Bible) for
"Alpha " in about 40 milliseconds. RPeg, on more modern hardware (2016 MacBook Pro), takes 5.4 seconds (!) for the same task. I have
profiled my code as best I can and don't think it will get any faster.

Of course, Ruby can call C code just as well as Lua can, but I am not planning to write RPeg in C. The LPeg code is very carefully
written to do all of the necessary memory managment, and it gets especially hairy in the implemention of "runtime captures".

### It is not industrial-strength

As much as I could, I implemented LPeg as described in the Ieuraselimschy paper, but this only got me so far. There is a great deal
of cleverness in LPeg, performing optimizations when a pattern is compiled for the bespoke VM, when analyzing patterns for errors,
and for dozen of other things. So, most of RPeg's code was written while carefully reading the LPeg sources, mostly written in
C. This was very educational, but in a few cases I simply couldn't understand what the LPeg code was doing, and was reduced to
blindly following the logic step-by-step, without a clear picture of what was "really" going on. This was unsatisfying, and left me
worried about the soundness of my code.

I have ported most of LPeg's (extensive) test suite and it all passes, but this is not a battle-hardened product.

I have tried hard to follow LPeg's functionality as closely as I can, but all bugs in RPeg are my responsibility.

## Using RPeg

Patterns in RPeg are much as they are in LPeg.

Here is a table of basic patterns, mostly repeated from the LPeg documentation.

| Operator         | Description                                |
|:-----------------|:-------------------------------------------|
| `RPEG.P(string)` | Matches `string` literally                 |
| `RPEG.P(num)`    | Match n characters (for non-negative n)    |
| `RPEG.P(-num)`   | Match only if there are _not_ n or more characters left. Equivalent to `"" - RPEG.P(num)` |
| `RPEG.S(str)`    | Match any character in `str`, which can actually be a `String` or a `Set` |
| `RPEG.R('xy')`    | Match any character in the range `'x'..'y'`. |
| `patt ** n`      | Match at least n repetitions of `patt`. (LPeg uses `patt^n` here.) |
| `patt ** -n`     | Match at most n repetitions of `patt`      |
| `patt1 * patt2`  | Match `patt1` following by `patt2`         |
| `patt1 + patt2`  | Match `patt1` or `patt2` (ordered choice)  |
| `patt1 - patt2`  | Match `patt1` if `patt2` does not match    |
| `-patt`          | Equivalent to `"" - patt`: `patt` does not match here |
| `+patt`          | Match `patt` but consume no input. (LPeg uses `#patt` here.) |
| `RPEG.B(patt)`   | Match `patt` "behind", i.e., before, the current position and consume no input |

There are many examples in the LPeg documentation [here](https://www.inf.puc-rio.br/~roberto/lpeg/#ex). Here is an LPeg example:

``` lua
local lpeg = require "lpeg"

-- matches a word followed by end-of-string
p = lpeg.R"az"^1 * -1

print(p:match("hello"))        --> 6
print(lpeg.match(p, "hello"))  --> 6
print(p:match("1 hello"))      --> nil
```

In Ruby we have

``` ruby
require 'rpeg'

p = RPEG.R("az")**1 * -1

puts p.match("hello")           --> 5    # the match ends at index 5, which is off the end
                                         # of the string. LPeg returns 6 because strings are
                                         # 1-based in Lua
puts RPEG.match(p, "hello")     --> 5
puts p.match("1 hello")         --> nil  # no match; RPeg matches are anchored to the
                                         # start of string
```

All of the LPeg examples work with RPeg once the necessary syntactic changes have been made.

## Grammars

We can build up and transform patterns incrementally, but for more powerful recursive patterns we need _grammars_. LPeg represents
grammars with _tables_, which are sort of a cross between arrays and hash tables. See (the LPeg
docs)[https://www.inf.puc-rio.br/~roberto/lpeg/#grammar] for an introduction.

Because we don't have tables in Ruby, RPeg allows grammars to be specified with arrays or hash tables.

Here is an example grammar from the LPeg docs. It matches strings that have equal numbers of `a`'s and `b`'s
``` lua
equalcount = lpeg.P{
  "S";   -- initial rule name
  S = "a" * lpeg.V"B" + "b" * lpeg.V"A" + "",
  A = "a" * lpeg.V"S" + "b" * lpeg.V"A" * lpeg.V"A",
  B = "b" * lpeg.V"S" + "a" * lpeg.V"B" * lpeg.V"B",
} * -1
```
The keys `S`, `A`, and `B` are the names of rules, and `lpeg.V<key>` refers to the given rule. Note how the initial rule name is
specified first.

In RPeg we can specify the same grammar with a hash table:

``` ruby
equalcount = RPEG.P( {
  initial: :S,  # initial rule name
  S: "a" * RPEG.V(:B) + "b" * RPEG.V(:A) + "",
  A: "a" * RPEG.V(:S) + "b" * RPEG.V(:A) * RPEG.V(:A),
  B: "b" * RPEG.V(:S) + "a" * RPEG.V(:B) * RPEG.V(:B)
} ) * -1

pp equalcount.match "ababab" # -> 6
pp equalcount.match "abbbaa" # -> 6
pp equalcount.match "aabba"  # -> nil
```
We specify the initial rule with the `:initial` key and can use symbols for rule names and references (strings in this context are
converted to symbols). The pattern `-1` matches only at the end of the string.

We can also specify the grammar in an array.

``` ruby
equalcount_arr = RPEG.P( [
  0,            # initial rule index
  "a" * RPEG.V(2) + "b" * RPEG.V(1) + "",
  "a" * RPEG.V(0) + "b" * RPEG.V(1) * RPEG.V(1),
  "b" * RPEG.V(0) + "a" * RPEG.V(2) * RPEG.V(2)
] ) * -1
```
Now the rules don't have names and are referred to by their indices in the array. If the first element of the array is a
non-negative integer then it is dropped and refers to the index of the initial rule. (The indices are reckoned after the
first value is dropped.) Otherwise the first rule is the initial rule.

## Captures

Captures are a powerful part of LPeg:
> A capture is a pattern that produces values (the so called semantic information) according to what it matches. LPeg offers several
> kinds of captures, which produces values based on matches and combine these values to produce new values. Each capture may produce
> zero or more values.

Captures are beyond the scope of this readme. See their [documentation in LPeg](https://www.inf.puc-rio.br/~roberto/lpeg/#captures).
RPeg follows the LPeg functionality with the following differences.

### Table captures

Lua's tables - mixtures of arrays and hashtables - are very flexible and LPeg takes advantange of that. Ruby doesn't have quite what
we need but Table captures - defined with `RPEG#Ct` - return instances of a special `TableCapture` class, which mimics a small part
of Lua's table functionality. Other approaches have been tried and haven't worked well.

TODO: give a description of `TableCapture` and an example.

### Function captures

Various kinds of captures involve calling a function (i.e., a proc) provided by client code. For example, the construction `patt /
fn` takes the captures made by patt and passes them as arguments to fn. Then the values returned by fn become the captures of the
expression.

Lua is better than Ruby at distinguishing between multiple return values and a single return value that is an array. In RPeg,
returns from function in contexts like this are treated as follows:

- `[1, 2, 3]`: multiple captures, 1, 2, 3.
  - this is the natural interpretation as it's the standard way that a Ruby function returns multiple values
- `[[1, 2, 3]]`: a single capture that is the array `[1, 2, 3]`.
- nil: no captures
  - even if the function says something like "return nil", the capture code has no way to distinguish between that and a
    function that returns nothing
- `[nil]`: a single capture with value nil
  - the weirdest case, but I don't see an alternative
- otherwise, the single value returned by the function is the single captured value.

But see #3 and #4.

Other approaches have been tried and haven't worked well in practice.

## Other Differences Between RPeg and LPeg

Efforts have been made to keep RPeg's syntax as close to LPeg's as possible. But there are necessarily some differences enforced by
Ruby. They have already been noted in passing.

### Indexing

Lua indexes strings and arrays (tables) from 1, while Ruby indexes from zero. RPeg follows the Ruby way. This means that

- "open" rules in grammars using numeric references use 0-indexing
- other contexts in which an integer is used as index - such as argument captures - are 0-indexed

### 'And' patterns

Given a pattern `p`, RPeg forms its "and" pattern using `+p` where LPeg uses `#p`. (This pattern matches if pattern p follows, but
it doesn't consume any input.)

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

Some experimentation has been done this is the best way I've found.

## The RPEG::RE module

This module provides a more compact, regex-style format for patterns in RPeg. See the [LPeg
documentation](https://www.inf.puc-rio.br/~roberto/lpeg/re.html). For example, here is the grammar that matches strings with equal
numbers of `a`'s and `b`'s again:

``` ruby
equalcount_re = RPEG::RE.compile(<<~GRAMMAR
  S <- "a" B / "b" A / ""
  A <- "a" S / "b" A A
  B <- "b" S / "a" B B
GRAMMAR
) * -1

pp equalcount_re.match "ababab"  # -> 6
pp equalcount_re.match "abbbaa"  # -> 6
pp equalcount_re.match "aabba"   # -> nil
```

The RE format is the same as it is in LPeg and close to how formal PEGs are written ([PEG-wiki](#references)).

# References
- [Ierusalimschy] Ierusalimschy, R., _Text Pattern-Matching Tool based on Parsing Expression Grammars_, Software: Practice and Experience, 39(3):221-258, Wiley, 2009, https://doi.org/10.1002/spe.892, http://www.inf.puc-rio.br/~roberto/docs/peg.pdf (retrieved 2022-01-??).
- [Cox] Cox, R., _Regular Expression Matching: the Virtual Machine Approach_, https://swtch.com/~rsc/regexp/regexp2.html.
- [PEG-wiki] _Parsing expression grammar_ (2023-01-23) in _Wikipedia_. https://en.wikipedia.org/wiki/Parsing_expression_grammar
