require 'test/unit'
require_relative 'pattern'
require_relative 're'

require 'byebug'

# These are tests extracted from test.lua in the LPEG sources.
# - #match in LPEG reports the index after the end of the match, as in the local RPEG code. But Lua strings are 1-based and so the
#   results there are one greater than we get here.
class TestsFromLpegCode < Test::Unit::TestCase
  def tests_for_some_basic_optimizations
    assert_equal 1, (m.P(false) + "a").match("a")
    assert_equal 0, (m.P(true) + "a").match("a")
    assert_equal nil, ("a" + m.P(false)).match("b")
    assert_equal 0, ("a" + m.P(true)).match("b")

    assert_equal nil, (m.P(false) * "a").match("a")
    assert_equal 1, (m.P(true) * "a").match("a")
    assert_equal nil, ("a" * m.P(false)).match("a")
    assert_equal 1, ("a" * m.P(true)).match("a")

    assert_equal nil, (+m.P(false) * "a").match("a")
    assert_equal 1, (+m.P(true) * "a").match("a")
    assert_equal nil, ("a" * +m.P(false)).match("a")
    assert_equal 1, ("a" * +m.P(true)).match("a")
  end

  def test_simple_patterns
    assert m.match(3, "aaaa")
    assert m.match(4, "aaaa")
    assert_nil m.match(5, "aaaa")
    assert m.match(-3, "aa")
    assert_nil m.match(-3, "aaa")
    assert_nil m.match(-3, "aaaa")
    assert_nil m.match(-4, "aaaa")
    assert m.P(-5).match("aaaa")

    assert_equal 1, m.match("a", "alo")
    assert_equal 2, m.match("al", "alo")
    assert_nil m.match("alu", "alo")
    assert_equal 0, m.match(true, "")
  end

  def test_charset_calculations
    eqcharset(m.S(""), m.P(false))
    eqcharset(upper, m.R("AZ"))
    eqcharset(lower, m.R("az"))
    eqcharset(upper + lower, m.R("AZ", "az"))
    eqcharset(upper + lower, m.R("AZ", "cz", "aa", "bb", "90"))
    eqcharset(digit, m.S("01234567") + "8" + "9")
    eqcharset(upper, letter - lower)
    eqcharset(m.S(""), m.R())
    assert(cs2str(m.S("")) == "")

    eqcharset(m.S("\0"), "\0")
    eqcharset(m.S("\1\0\2"), m.R("\0\2"))
    eqcharset(m.S("\1\0\2"), m.R("\1\2") + "\0")
    eqcharset(m.S("\1\0\2") - "\0", m.R("\1\2"))
  end

  def test_predicates
    # -- tests for predicates
    assert_nil m.match(-m.P("a") * 2, "alo")
    assert_equal 2, m.match(- -m.P("a") * 2, "alo")
    assert_equal 2, m.match(+m.P("a") * 2, "alo")
    assert_equal 2, m.match(++m.P("a") * 2, "alo")
    assert_nil m.match(++m.P("c") * 2, "alo")

    assert_equal "a..a.", m.match(m.Cs((++m.P("a") * 1 + m.P(1)/".")**0), "aloal")
    assert_equal "a..a.", m.match(m.Cs((+((+m.P("a"))/"") * 1 + m.P(1)/".")**0), "aloal")
    assert_equal "a..a.", m.match(m.Cs((- -m.P("a") * 1 + m.P(1)/".")**0), "aloal")
    assert_equal "a..a.", m.match(m.Cs((-((-m.P("a"))/"") * 1 + m.P(1)/".")**0), "aloal")
  end

  def test_coercion
    # -- tests for non-pattern as arguments to pattern functions

    p = [ ('a' * m.V(0))**-1 ] * m.P('b') * [ 'a' * m.V(1), m.V(0)**-1 ]
    assert_equal 6, m.match(p, "aaabaac")

    p = m.P('abc') * 2 * -5 * true * 'de'  # -- mix of numbers and strings and booleans
    assert_equal 7, p.match("abc01de")
    assert_nil p.match("abc01de3456")

    p = 'abc' * (2 * (-5 * (true * m.P('de'))))

    assert_equal 7, p.match("abc01de")
    assert_nil p.match("abc01de3456")

    p = [m.V(1), m.P("abc")] *
        (m.P({ initial: "xx", xx: m.P("xx") }) + { initial: "x", x: m.P("a") * m.V("x") + "" })
    assert_equal 6, p.match("abcaaaxx")
    assert_equal 5, p.match("abcxx")
  end

  def test_look_behind
    # -- look-behind predicate
    assert_nil m.match(m.B('a'), 'a')
    assert_equal 1, m.match(1 * m.B('a'), 'a')
    assert_nil m.match(m.B(1), 'a')
    assert_equal 1, m.match(1 * m.B(1), 'a')
    assert_equal 0, m.match(-m.B(1), 'a')
    assert_nil m.match(m.B(250), 'a' * 250)
    assert_equal 250, m.match(250 * m.B(250), 'a' * 250)

    # -- look-behind with an open call
    assert_match_raises_error("pattern may not have fixed length", ->{ m.B(m.V('S1')) }, '')

    # we don't enforce such a limit
    # checkerr("too long to look behind", m.B, 260)

    b = +letter * -m.B(letter) + -letter * m.B(letter)
    x = m.Ct({ S: (b * m.Cp())**-1 * (1 * m.V(0) + m.P(true)) })
    assert_equal [0, 2, 3, 6, 8, 9], m.match(x, 'ar cal  c')
    assert_equal [1, 3, 4, 7], m.match(x, ' ar cal  ')
    assert_equal [], m.match(x, '   ')
    assert_equal [0, 6], m.match(x, 'aloalo')

    assert_equal 0, m.match(b, "a")
    assert_equal 1, m.match(1 * b, "a")
    assert_nil m.B(1 - letter).match("")
    assert_equal 0, (-m.B(letter)).match("")

    # The second argument, 4, to m.B doesn't make sense. Perhaps in LPEG lua is just ignoring the second argument
    # assert_equal 4, (4 * m.B(letter, 4)).match("aaaaaaaa")
    assert_equal 4, (4 * m.B(letter)).match("aaaaaaaa")
    assert_nil (4 * m.B(+letter * 5)).match("aaaaaaaa")
    assert_equal 4, (4 * -m.B(+letter * 5)).match("aaaaaaaa")

    #-- look-behind with grammars
    assert_nil m.match('a' * m.B({ x: m.P(3) }), 'aaa')
    assert_nil m.match('aa' * m.B({ x: m.P('aaa') }), 'aaaa')
    assert_equal 3, m.match('aaa' * m.B({ x: m.P('aaa') }), 'aaaaa')
  end

  def test_word_constructs
    assert (word**0 * -1).match("alo alo")
    assert m.match(word**1 * -1, "alo alo")
    assert m.match(word**2 * -1, "alo alo")
    assert_nil m.match(word**3 * -1, "alo alo")

    assert_nil m.match(word**-1 * -1, "alo alo")
    assert m.match(word**-2 * -1, "alo alo")
    assert m.match(word**-3 * -1, "alo alo")

    assert m.match(digit**0 * letter * digit * eos, "1298a1")
    assert_nil m.match(digit**0 * letter * eos, "1257a1")

    b = [ "(" * (((1 - m.S("()")) + +m.P("(") * m.V(0))**0) * ")" ]

    assert m.match(b, "(al())()")
    assert_nil m.match(b * eos, "(al())()")
    assert m.match(b * eos, "((al())()(é))")
    assert_nil m.match(b, "(al()()")

    assert_nil m.match(letter**1 - "for", "foreach")
    assert m.match(letter**1 - ("for" * eos), "foreach")
    assert_nil m.match(letter**1 - ("for" * eos), "for")
  end

  # test.lua ll.894-911
  def test_isnullable
    nullable?(m.P("x")**-4)
    assert_equal 2, m.match(((m.P(0) + 1) * m.S("al"))**0, "alo")
    assert_equal 2, m.match((("x" + +m.P(1))**-4 * m.S("al"))**0, "alo")
    nullable?("")
    nullable?(m.P("x")**0)
    nullable?(m.P("x")**-1)
    nullable?(m.P("x") + 1 + 2 + m.P("a")**-1)
    nullable?(-m.P("ab"))
    nullable?(- -m.P("ab"))
    nullable?(+ +(m.P("ab") + "xy"))
    nullable?(- +(m.P("ab")**0))
    nullable?(+ -m.P("ab")**1)
    nullable?(+m.V(3))
    nullable?(m.V(3) + m.V(1) + m.P('a')**-1)
    nullable?([m.V(1) * m.V(2), m.V(2), m.P(0)])
    assert_equal 2, m.match(m.P([0, m.V(1) * m.V(2), m.V(2), m.P(1)])**0, "abc")
    assert_equal 0, m.match(m.P("")**-3, "a")
  end

  # test.lua ll.918-943
  def test_bad_grammar
    bad_grammar = lambda do |grammar, expected_pattern = ""|
      expected_pattern = /#{expected_pattern}/ if expected_pattern.is_a?(String)
      assert_raise_message(expected_pattern) { m.P(grammar) }
    end

    # Some of these have been converted to use array notation for grammars
    bad_grammar.call([ m.V(0) ], "may be left-recursive")
    bad_grammar.call([ m.V(1) ], "rule '1'")   # invalid non-terminal
    bad_grammar.call([ m.V("x") ], "rule 'x'")   # invalid non-terminal
    bad_grammar.call({ S: m.V({}) }, "rule '{}'")   # invalid non-terminal
    bad_grammar.call({ S: +m.P("a") * m.V(0) }, "rule 'S'")  # left-recursive
    bad_grammar.call({ S: -m.P("a") * m.V(0) }, "rule 'S'")  # left-recursive
    bad_grammar.call({ S: -1 * m.V(0) }, "rule 'S'")  # left-recursive
    bad_grammar.call({ S: -1 + m.V(0) }, "rule 'S'")  # left-recursive
    bad_grammar.call({ S: 1 * m.V(1), T: m.V(1) }, "rule 'T'") # left-recursive
    bad_grammar.call([ 1 * m.V(1)**0, m.P(0) ], "rule '__0'") # inf. loop
    bad_grammar.call({ S: m.V(1), T: m.V(2)**0, U: m.P("") }, "rule 'T'") # inf. loop
    bad_grammar.call({ S: m.V(1) * m.V(2)**0, T: m.V(2)**0, U: m.P("") }, "rule 'S'") # inf. loop
    bad_grammar.call({ S: +(m.V(0) * 'a') }, "rule 'S'") # inf. loop
    bad_grammar.call({ S: -(m.V(0) * 'a') }, "rule 'S'") # inf. loop
    bad_grammar.call({ x: m.P('a')**-1 * m.V("x") }, "rule 'x'") # left recursive
    bad_grammar.call({ x: m.P('a') * m.V("y")**1, y: +m.P(1) }, "rule 'x'")

    assert_equal 1, m.match({ x: 'a' * -m.V(0) }, "aaa")
    assert_nil m.match({ x: 'a' * -m.V(0) }, "aaaa")

    # -- good x bad grammars
    m.P([('a' * m.V(0))**-1])
    m.P([-('a' * m.V(0))])
    m.P([('abc' * m.V(0))**-1])
    m.P([-('abc' * m.V(0))])
    bad_grammar.call([+m.P('abc') * m.V(0)])
    bad_grammar.call([-('a' + m.V(0))])
    m.P([+('a' * m.V(0))])
    bad_grammar.call([+('a' + m.V(0))])
    m.P([m.B([m.P('abc')]) * 'a' * m.V(0)])
    bad_grammar.call([m.B([m.P('abc')]) * m.V(0)])
    bad_grammar.call([('a' + m.P('bcd'))**-1 * m.V(0)])
  end

  def test_optimizations
    # -- test for alternation optimization
    assert_equal 1, m.match(m.P("a")**1 + "ab" + m.P("x")**0, "ab")
    assert_equal 2, m.match((m.P("a")**1 + "ab" + m.P("x")**0 * 1)**0, "ab")
    assert_equal 0, m.match(m.P("ab") + "cd" + "" + "cy" + "ak", "98")
    assert_equal 2, m.match(m.P("ab") + "cd" + "ax" + "cy", "ax")
    assert_equal 2, m.match("a" * m.P("b")**0 * "c"  + "cd" + "ax" + "cy", "ax")
    assert_equal 2, m.match((m.P("ab") + "cd" + "ax" + "cy")**0, "ax")
    assert_equal 2, m.match(m.P(1) * "x" + m.S("") * "xu" + "ay", "ay")
    assert_equal 3, m.match(m.P("abc") + "cde" + "aka", "aka")
    assert_equal 2, m.match(m.S("abc") * "x" + "cde" + "aka", "ax")
    assert_equal 3, m.match(m.S("abc") * "x" + "cde" + "aka", "aka")
    assert_equal 3, m.match(m.S("abc") * "x" + "cde" + "aka", "cde")
    assert_equal 3, m.match(m.S("abc") * "x" + "ide" + m.S("ab") * "ka", "aka")
    assert_equal 2, m.match("ab" + m.S("abc") * m.P("y")**0 * "x" + "cde" + "aka", "ax")
    assert_equal 3, m.match("ab" + m.S("abc") * m.P("y")**0 * "x" + "cde" + "aka", "aka")
    assert_equal 3, m.match("ab" + m.S("abc") * m.P("y")**0 * "x" + "cde" + "aka", "cde")
    assert_equal 3, m.match("ab" + m.S("abc") * m.P("y")**0 * "x" + "ide" + m.S("ab") * "ka", "aka")
    assert_equal 2, m.match("ab" + m.S("abc") * m.P("y")**0 * "x" + "ide" + m.S("ab") * "ka", "ax")
    assert_equal 3, m.match(m.P(1) * "x" + "cde" + m.S("ab") * "ka", "aka")
    assert_equal 3, m.match(m.P(1) * "x" + "cde" + m.P(1) * "ka", "aka")
    assert_equal 3, m.match(m.P(1) * "x" + "cde" + m.P(1) * "ka", "cde")
    assert_equal 2, m.match(m.P("eb") + "cd" + m.P("e")**0 + "x", "ee")
    assert_equal 2, m.match(m.P("ab") + "cd" + m.P("e")**0 + "x", "abcd")
    assert_equal 3, m.match(m.P("ab") + "cd" + m.P("e")**0 + "x", "eeex")
    assert_equal 2, m.match(m.P("ab") + "cd" + m.P("e")**0 + "x", "cd")
    assert_equal 0, m.match(m.P("ab") + "cd" + m.P("e")**0 + "x", "x")
    assert_equal 0, m.match(m.P("ab") + "cd" + m.P("e")**0 + "x" + "", "zee")
    assert_equal 2, m.match(m.P("ab") + "cd" + m.P("e")**1 + "x", "abcd")
    assert_equal 3, m.match(m.P("ab") + "cd" + m.P("e")**1 + "x", "eeex")
    assert_equal 2, m.match(m.P("ab") + "cd" + m.P("e")**1 + "x", "cd")
    assert_equal 1, m.match(m.P("ab") + "cd" + m.P("e")**1 + "x", "x")
    assert_equal 0, m.match(m.P("ab") + "cd" + m.P("e")**1 + "x" + "", "zee")
    assert_nil m.match(("aa" * m.P("bc")**-1 + "aab") * "e", "aabe")

    assert_equal 3, m.match("alo" * (m.P("\n") + -1), "alo")

    # -- optimizations with optional parts
    assert_equal 0, m.match(("ab" * -m.P("c"))**-1, "abc")
    assert_equal 0, m.match(("ab" * +m.P("c"))**-1, "abd")
    assert_equal 0, m.match(("ab" * m.B("c"))**-1, "ab")
    assert_equal 6, m.match(("ab" * m.P("cd")**0)**-1, "abcdcdc")

    assert_equal 2, m.match(m.P("ab")**-1 - "c", "abcd")

    p = ('Aa' * ('Bb' * ('Cc' * m.P("Dd")**0)**0)**0)**-1
    assert_equal 20, p.match("AaBbCcDdBbCcDdDdDdBb")

    # -- Bug in peephole optimization of LPeg 0.12 (IJmp -> ICommit)
    # -- the next grammar has an original sequence IJmp -> ICommit -> IJmp L1
    # -- that is optimized to ICommit L1
    p = m.P({ x: (m.P([m.P('abc')]) + 'ayz') * m.V('y'), y: m.P('x') })
    assert(p.match('abcx') == 4 && p.match('ayzx') == 4 && p.match('abc').nil?)
  end

  def test_const_capture
    # test.lua l.170
    # -- bug in LPeg 0.12  (nil value does not create a 'ktable')
    assert_equal nil, m.match(m.Cc(nil), "")

    # test.lua l.176
    assert_equal [10, 20, 30, 1], m.match(m.Cc(10, 20, 30) * 'a' * m.Cp(), 'aaa')
    assert_equal [0, 10, 20, 30, 1], m.match(m.Cp() * m.Cc(10, 20, 30) * 'a' * m.Cp(), 'aaa')
    assert_equal [0, 10, 20, 30, 1], m.match(m.Ct(m.Cp() * m.Cc(10, 20, 30) * 'a' * m.Cp()), 'aaa')
    assert_equal [0, 7, 8, 10, 20, 30, 1], m.match(m.Ct(m.Cp() * m.Cc(7, 8) * m.Cc(10, 20, 30) * 'a' * m.Cp()), 'aaa')
    assert_equal [1, 2, 3, 4], m.match(m.Cc() * m.Cc() * m.Cc(1) * m.Cc(2, 3, 4) * m.Cc() * 'a', 'aaa')
    assert_equal [0, 4], m.match(m.Cp() * letter**1 * m.Cp(), "abcd")

    # test.lua l.240
    assert_equal 1, m.match(m.Cc(0) * m.P(10) + m.Cc(1) * "xuxu", "xuxu")
    assert_equal 0, m.match(m.Cc(0) * m.P(10) + m.Cc(1) * "xuxu", "xuxuxuxuxu")

    # test.lua l.314
    # -- tests for capture optimizations
    assert_equal 4, m.match((m.P(3) +  4 * m.Cp()) * "a", "abca")
    assert_equal [2, 5], m.match(((m.P("a") + m.Cp()) * m.P("x"))**0, "axxaxx")

    # test.lua l.559
    p1 = -m.P('a') * m.Cc(1) + -m.P('b') * m.Cc(2) + -m.P('c') * m.Cc(3)
    assert_equal 2, p1.match('a')
    assert_equal 1, p1.match('')
    assert_equal 1, p1.match('b')

    p = -m.P('a') * m.Cc(10) + +m.P('a') * m.Cc(20)
    assert_equal 20, p.match('a')
    assert_equal 10, p.match('')
    assert_equal 10, p.match('b')

    # test.lau l.788
    # -- bug in 0.12.2: ktable with only nil could be eliminated when joining
    # -- with a pattern without ktable
    #
    # We don't use ktable things in RPEG but may as well do the test
    assert_nil (m.P("aaa") * m.Cc(nil)).match("aaa")

    # test.lua l.838
    # This looks like a test that the value in a constant match can be a method
    fn = lambda { |x| x }
    assert_equal fn, m.Cc(fn).match("")
  end

  def test_argument_capture
    # test.lua l.677
    # -- tests for argument captures
    assert_match_raises_error("Invalid argument", -> {m.Carg}, 0)
    assert_match_raises_error("Invalid argument", -> {m.Carg}, -1)
    assert_match_raises_error("Invalid argument", -> {m.Carg}, 2**18)
    assert_match_raises_error("absent extra argument #1", m.Carg(1), 'a', 0)
    assert_equal a_lambda, m.match(m.Carg(1), 'a', 1, a_lambda)
    assert_equal [10, 20], m.match(m.Carg(1) * m.Carg(2), '', 0, 10, 20)

    assert_equal(
      2 * 1001 + 3 * 100,
      m.match(
        m.Cmt(
          m.Cg(m.Carg(3), "a") *
          m.Cmt(
            m.Cb("a"),
            lambda do |s, i, x|
              assert(s == "a" && i == 0)
              [i, x + 1]
            end
          ) * m.Carg(2),
          lambda do |s, i, a, b, c = nil|
            assert(s == "a" && i == 0 && c.nil?)
            [i, 2 * a + 3 * b]
          end
        ) * "a",
        "a", 0, false, 100, 1000
      )
    )
  end

  def test_simple_captures
    assert_equal %w[123 d], m.match(m.C(digit**1 * m.Cc("d")) + m.C(letter**1 * m.Cc("l")), "123")
    assert_equal %w[abcd l], m.match(m.C(digit**1 * m.Cc("d")) + m.C(letter**1 * m.Cc("l")), "abcd")

    assert_equal ["abc", "a", "bc", "b", "c", "c", ""], m.match([m.C(m.C(1) * m.V(0) + -1)], "abc")

    # -- bug in 0.12 ('hascapture' did not check for captures inside a rule)
    #
    # What does this mean?
    #
    # Oh. I think it is just the fact that we don't actually consume any of the subject here: we are just testing that the string is
    # matched by S1, and not actually consuming the string. So we don't have a capture in the result and just return 0 for an empty
    # match.
    patt1 = m.P({
                  S: +m.V('S1'), # -- rule has capture, but '#' must ignore it
                  S1: m.C('abc') + 3
                })
    assert_equal 0, patt1.match('abc')

    # -- bug: loop in 'hascaptures'
    #
    # Note: I haven't implemented hascaptures yet
    patt2 = m.C(-m.P({ S: m.P('x') * m.V(0) + m.P('y') }))
    assert_equal "", patt2.match("xxx")

    # -- test for small capture boundary
    #
    # The Lua test says "#m.match". On a string the # operator returns the length
    [250, 260].each do |i|
      assert_equal i, m.match(m.C(i), 'a' * i).length

      # TODO: why should the match return the full string instead of two copies of the string?
      #
      # Maybe a simple capture over a simple capture like this is collapsed to one capture, because otherwise the two values would
      # always be the same.
      assert_equal i, m.match(m.C(m.C(i)), 'a' * i).length
    end

    # -- tests for any*n and any*-n
    [1, 550, 13].each do |n|
      x1 = 'x' * (n - 1)
      x = "#{x1}a"
      assert_nil m.P(n).match(x1)
      assert_equal n, m.P(n).match(x)
      assert_equal(3, m.match(m.P(n) + "xxx", x1)) if n >= 4

      assert_equal x, m.C(n).match(x)
      assert_equal x, m.C(m.C(n)).match(x)
      assert_equal 0, m.P(-n).match(x1)
      assert_nil m.P(-n).match(x)
      assert_equal(20, m.match(m.Cc(20) * ((n - 13) * m.P(10)) * 3, x)) if n >= 13

      n3 = n / 3
      assert_equal n3, m.match(n3 * m.Cp() * n3 * n3, x)
    end

    # -- true values
    assert_equal 0, m.P(0).match("x")
    assert_equal 0, m.P(0).match("")
    assert_equal "", m.C(0).match("x")

    assert_equal "abcd", m.match(m.C(m.P(2)**1), "abcde")

    # Is this a dangling pattern?
    # p = m.Cc(0) * 1 + m.Cc(1) * 2 + m.Cc(2) * 3 + m.Cc(3) * 4
  end

  def test_fixed_length
    # -- 'and' predicate using fixed length
    p = m.C(+("a" * (m.P("bd") + "cd")) * 2)
    assert_equal "ac", p.match("acd")

    p = +m.P({ S: "a" * m.V(1), T: m.P("b") }) * 2
    assert_equal 2, p.match("abc")

    p = +(m.P("abc") * m.B("c"))
    assert_equal 0, p.match("abc")
    assert_nil p.match("ab")

    p = m.P({ S: "a" * m.V(1), T: m.P("b")**1 })
    assert_match_raises_error("pattern may not have fixed length", -> { m.B(p) }, nil)

    p = "abc" * (m.P("b")**1 + m.P("a")**0)
    assert_match_raises_error("pattern may not have fixed length", -> { m.B(p) }, nil)
  end

  def test_group_capture
    # -- tests for groups
    p = m.Cg(1) # -- no capture
    assert_equal 'x', p.match('x')

    p = m.Cg(m.P(true) / ->(*) {} * 1) # -- no value
    assert_equal 'x', p.match('x')

    p = m.Cg(m.Cg(m.Cg(m.C(1))))
    assert_equal 'x', p.match('x')
    p = m.Cg(m.Cg(m.Cg(m.C(1))**0) * m.Cg(m.Cc(1) * m.Cc(2)))
    assert_equal ['a', 'b', 'c', 1, 2], p.match('abc')
  end

  def test_backref_captures
    assert_match_raises_error("back reference 'x' not found", m.Cb('x'), '')
    assert_match_raises_error("back reference 'b' not found", m.Cg(1, 'a') * m.Cb('b'), 'a')

    p = m.P(true)
    (1..10).each { |i| p = p * m.Cg(1, i) }
    (1..10).each do |i|
      patt = p * m.Cb(i)
      subject = 'abcdefghij'
      assert_equal subject[i - 1, 1], patt.match(subject)
    end
  end

  def test_table_captures
    # -- test for table captures
    assert_equal [], m.match(m.Ct(letter**1), "alo")

    t, n = m.match(m.Ct(m.C(letter)**1) * m.Cc("t"), "alo")
    assert_equal "t", n
    assert_equal "alo", t.unpack.join

    t = m.match(m.Ct(m.C(m.C(letter)**1)), "alo")
    assert_equal "alo;a;l;o", t.unpack.join(";")

    t = m.match(m.Ct(m.C(m.C(letter)**1)), "alo")
    assert_equal "alo;a;l;o", t.unpack.join(";")

    t = m.match(m.Ct(m.Ct((m.Cp() * letter * m.Cp())**1)), "alo")
    assert_equal "0;1;1;2;2;3", t[0].unpack.join(";")

    assert_equal %w[alo a o], m.match(m.Ct(m.C(m.C(1) * 1 * m.C(1))), "alo")

    p = m.Ct(m.Cg(m.Cc(10), "hi") * m.C(1)**0 * m.Cg(m.Cc(20), "ho"))
    assert_equal ({ "hi" => 10, "ho" => 20 }), p.match('')

    assert_equal ( {"hi" => 10, "ho" => 20, 0 => 'a', 1 => 'b', 2 => 'c'} ), p.match('abc')

    # -- non-string group names
    p = m.Ct(m.Cg(1, a_lambda) * m.Cg(1, 23.5) * m.Cg(1, Kernel))
    assert_equal ( { a_lambda => 'a', 23.5 => 'b', Kernel => 'c'} ), p.match('abcdefghij')

    # -- a large table capture
    #
    big = 1_000
    # big = 10_000 # Note: this is slow! About 800 ms
    t = m.match(m.Ct(m.C('a')**0), "a" * big)
    assert_equal big, t.size
    assert_equal 'a', t.unpack.first
    assert_equal 'a', t.unpack.last

    p = m.Cg(m.C(1) * m.C(1), "k") * m.Ct(m.Cb("k"))
    t = p.match("ab")
    assert_equal %w[a b], t
  end

  def test_fold_captures
    # -- accumulator capture
    f = ->(x, _) { x + 1 }
    g = ->(x, y) { x * y.length * 2 }
    assert_equal 7, m.match(m.Cf(m.Cc(0) * m.C(1)**0, f), "alo alo")
    assert_equal 128, m.match(m.Cf(m.Cc(1) * m.C(1)**0, g), "alo alo")

    # Test that we get only the first term of the first capture, and when there are no other captures that the accumulator is not
    # called.
    f = ->(_, _) { raise :bogus }
    assert_equal 1, m.match(m.Cf(m.Cc(1, 2, 3), f), "")

    # the equivalent of Lua's rawset, I think.
    f = ->(h, x, y) { h[x] = y; h }
    p = m.Cf(m.Cc({}) * m.Cg(m.C(m.R("az")**1) * "=" * m.C(m.R("az")**1) * ";")**0, f)
    assert_equal ({ "a" => "b", "c" => "du", "xux" => "yuy"}), p.match("a=b;c=du;xux=yuy;")

    # -- errors in accumulator capture

    # -- no initial capture
    assert_match_raises_error("no initial value", m.Cf(m.P(5), a_lambda), 'aaaaaa')
    # -- no initial capture (very long match forces fold to be a pair open-close)
    assert_match_raises_error("no initial value", m.Cf(m.P(500), a_lambda), 'a' * 600)

    # -- nested capture produces no initial value
    assert_match_raises_error("no initial value", m.Cf(m.P(1) / {}, a_lambda), "alo")
  end

  def test_numbered_captures
    # -- tests for numbered captures
    p = m.C(1)
    assert_equal 'a', m.match(m.C(m.C(p * m.C(2)) * m.C(3)) / 3, "abcdefgh")
    assert_equal 'abcdef', m.match(m.C(m.C(p * m.C(2)) * m.C(3)) / 1, "abcdefgh")
    assert_equal 'bc', m.match(m.C(m.C(p * m.C(2)) * m.C(3)) / 4, "abcdefgh")
    assert_equal 6, m.match(m.C(m.C(p * m.C(2)) * m.C(3)) / 0, "abcdefgh")

    assert_equal %w[a efg h], m.match(p * (m.C(p * m.C(2)) * m.C(3) / 4) * p, "abcdefgh")
  end

  def test_function_capturte
    # -- tests for Function Replacements
    #
    # f = function (a, ...) if a ~= "x" then return {a, ...} end end
    #
    # Issue: Lua distinguishes between returning multiple values,
    #
    #    return x, y
    #
    # and returning a single table containing multiple values,
    #
    #    return {x, y}
    #
    # The intention of the Lua tests is to wrap the arguments in a table, returning a single value. I think this means we need to do
    # a "double wrap" of the value. The code in push_function_replacement will count the results and get 1, as desired.
    f = lambda do |a, *rest|
      [[a, *rest]] if a != "x"
    end

    assert_equal %w[a b c], m.match(m.C(1)**0/f, "abc")
    assert_equal [%w[a b c]], m.match(m.C(1)**0/f/f, "abc")
    assert_equal [["abc"]], m.match(m.P(1)**0/f/f, "abc")  #-- no capture
    assert_equal [["abc"], 3], m.match((m.P(1)**0/f * m.Cp()) / f, "abc")
    assert_equal [["a", "b", "c"], 3],  m.match((m.C(1)**0/f * m.Cp())/f, "abc")
    assert_equal [3], m.match((m.C(1)**0/f * m.Cp())/f, "xbc")
    assert_equal %w[abc a b c], m.match(m.C(m.C(1)**0)/f, "abc")

    g = ->(*a) { [1, *a] }
    assert_equal [1, 1, "a", "b", "c"], m.match(m.C(1)**0/g/g, "abc")
    assert_equal [1, 1, nil, nil, 4, nil, 3, nil, nil], m.match(m.Cc(nil,nil,4) * m.Cc(nil,3) * m.Cc(nil, nil) / g / g, "")

    assert_equal ["a", "ax", "b", "bx", "c", "cx"], m.match((m.C(1) / ->(x) { [x, x + "x"] })**0, "abc")
  end

  def test_query_capture
    assert_equal 10, m.match(m.C(m.C(1)**0) / { "abc" => 10 }, "abc")
    assert_equal 10, m.match(m.C(1)**0 / { "a" => 10 }, "abc")
    assert_equal 40, m.match(m.S("ba")**0 / { "ab" => 40 }, "abc")
    assert_equal [40], m.match(m.Ct((m.S("ba") / { "a" => 40 })**0), "abc")

    assert_equal ".bc....e", m.match(m.Cs((m.C(1) / { "a" => ".", "d" => ".." })**0), "abcdde")
    assert_equal "abcdde", m.match(m.Cs((m.C(1) / { "f" => "." })**0), "abcdde")
    assert_equal "abc..e", m.match(m.Cs((m.C(1) / { "d" => "." })**0), "abcdde")
    assert_equal "abcdd.", m.match(m.Cs((m.C(1) / { "e" => "." })**0), "abcdde")
    assert_equal "..+.+", m.match(m.Cs((m.C(1) / { "e" => ".", "f" => "+" })**0), "eefef")
    assert_equal "abcdde", m.match(m.Cs((m.C(1))**0), "abcdde")
    assert_equal "abcdde", m.match(m.Cs(m.C(m.C(1)**0)), "abcdde")
    assert_equal "bcdde", m.match(1 * m.Cs(m.P(1)**0), "abcdde")

    assert_equal "abcdde", m.match(m.Cs((m.C('0') / 'x' + 1)**0), "abcdde")
    assert_equal "xabxbx", m.match(m.Cs((m.C('0') / 'x' + 1)**0), "0ab0b0")
    assert_equal "3xax3", m.match(m.Cs((m.C('0') / 'x' + m.P(1) / { "b" => 3 })**0), "b0a0b")
    assert_equal (-3), m.match(m.P(1) / '%0%0' / { "aa" => -3 } * 'x', 'ax')
    assert_equal (-3), m.match(m.C(1) / '%0%1' / { "aa" => 'z' } / { "z" => -3 } * 'x', 'ax')
    assert_equal "a%a", m.match(m.C("a") / "%1%%%0", "a")
    assert_equal ".xx.xx.xx.xx", m.match(m.Cs((m.P(1) / ".xx")**0), "abcd")
    assert_equal "300 - abc ", m.match(m.Cp() * m.P(3) * m.Cp()/"%2%1%1 - %0 ", "abcde")

    assert_equal "a", m.match(m.P(1) / "%0", "abc")
    assert_match_raises_error("invalid capture index", m.P(1) / "%1", "abc")
    assert_match_raises_error("invalid capture index", m.P(1) / "%9", "abc")

    assert_equal "0", m.match(m.Cs(m.Cc(0) * (m.P(1) / "")), "4321")

    assert_equal "abcd", m.match(m.Cs((m.P(1) / "%0")**0), "abcd")
    assert_equal "a.ab.bc.cd.d", m.match(m.Cs((m.P(1) / "%0.%0")**0), "abcd")
    assert_equal "a.abca.ad", m.match(m.Cs((m.P("a") / "%0.%0" + 1)**0), "abcad")

    pat = m.C(1)
    pat *= pat; pat *= pat; pat = pat * pat * m.C(1) / "%9 - %1"
    assert_equal "9 - 1", pat.match("1234567890")

    # -- too many captures (just ignore extra ones)
    p = m.C(1)**0 / "%2-%9-%0-%9"
    assert_equal "1-8-01234567890123456789-8", p.match("01234567890123456789")
    s = "12345678901234567890" * 20
    assert_equal "9-1-#{s}-3", m.match(m.C(1)**0 / '%9-%1-%0-%3', s)

    # -- string captures with non-string subcaptures
    p = m.Cc('alo') * m.C(1) / "%1 - %2 - %1"
    assert_equal 'alo - x - alo', p.match('x')

    assert_match_raises_error("invalid capture value (a TrueClass)", m.Cc(true) / "%1", "a")

    # l = 10_000 # slow: about 500 ms
    l = 1_000
    s = 'a' * l + 'b' * l + 'c' * l
    p = (m.C(m.P('a')**1) * m.C(m.P('b')**1) * m.C(m.P('c')**1)) / '%3%2%1'
    assert_equal 'c' * l + 'b' * l + 'a' * l, p.match(s)
  end

  def test_substitution_captures
    pi = "3.14159 26535 89793 23846 26433 83279 50288 41971 69399 37510"
    assert_equal m.match(m.Cs((m.P("1") / "a" + m.P("5") / "b" + m.P("9") / "c" + 1)**0), pi),
                 m.match(m.Cs((m.P(1) / { "1" => "a", "5" => "b", "9" => "c" })**0), pi)
  end

  def test_tail_calls
    # -- tests for Tail Calls
    p = m.P([ 'a' * m.V(0) + '' ])
    assert_equal 1000, p.match('a' * 1000)

    # -- create a grammar for a simple DFA for even number of 0s and 1s
    # --
    # --  ->1 <---0---> 2
    # --    ^           ^
    # --    |           |
    # --    1           1
    # --    |           |
    # --    V           V
    # --    3 <---0---> 4
    # --
    # -- this grammar should keep no backtracking information
    #
    # I think this claim relies on the headfail/follow optimization that the LPEG code does and my code doesn't.

    p = m.P([
              '0' * m.V(1) + '1' * m.V(2) + -1,
              '0' * m.V(0) + '1' * m.V(3),
              '0' * m.V(3) + '1' * m.V(0),
              '0' * m.V(2) + '1' * m.V(1),
            ])

    assert p.match("00" * 10_000)
    assert p.match("01" * 10_000)
    assert p.match("011" * 10_000)
    assert_nil p.match(("011" * 10_000) + "1")
    assert_nil p.match("011" * 10_001)
  end

  def test_matchtime_captures
    id = ->(s, i, *rest) { [true, *rest] }

    assert_equal "xyb#{98.chr}+#{68.chr}y",
                 m.Cmt(
                   m.Cs(
                     (m.Cmt(m.S('abc') / { "a" => 'x', 'c' => 'y' }, id) +
                                            m.R('09')**1 / ->(str) { str.to_i.chr } +
                      m.P(1)
                     )**0
                   ),
                   id
                 ).match("acb98+68c")

    patt = m.P({
                 S: m.V('atom') * space + m.Cmt(m.Ct("(" * space * (m.Cmt(m.V('S')**1, id) + m.P(true)) * ")" * space), id),
                 atom: m.Cmt(m.C(m.R("AZ", "az", "09")**1), id)
               })
    x = patt.match"(a g () ((b) c) (d (e)))"
    assert_equal ['a', 'g', [], [['b'], 'c'], ['d', ['e']]], x

    x = (m.Cmt(1, id)**0).match('a' * 500)
    assert_equal 500, x.length

    # A toy numerical parser
    number = m.C(m.R("09")**1) * space
    factorOp = m.C(m.S("+-")) * space
    termOp = m.C(m.S("*/")) * space
    open = "(" * space
    close = ")" * space

    f_factor = lambda do |v1, op, v2, d = nil|
      d.must_be nil
      return v1 + v2 if op == "+"
      v1 - v2
    end

    f_term = lambda do |v1, op, v2, d = nil|
      d.must_be nil
      return v1 * v2 if op == "*"
      v1 / v2
    end

    g = m.P(
      { initial: :Exp,
        Exp: m.Cf(m.V("Factor") * m.Cg(factorOp * m.V("Factor"))**0, f_factor),
        Factor: m.Cf(m.V("Term") * m.Cg(termOp * m.V("Term"))**0, f_term),
        Term: number / ->(e) { e.to_i } + open * m.V("Exp") * close
      }
    )

    g = space * g * -1

    [" 3 + 5*9 / (1+1) ", "3+4/2", "3+3-3- 9*2+3*9/1-  8"].each do |expr|
      assert_equal eval(expr), m.match(g, expr)
    end

    # P(function)
    p = m.P('a') * ->(s, i, *) { s[i] == 'b' and i + 1 } + 'acd'
    assert_equal 2, p.match('abc')
    assert_equal 3, p.match('acd')

    id = lambda do |s, i, x|
      return [i, 1, 3, 7] if x == 'a'
      [nil, 2, 4, 6, 8]
    end

    p = ((m.P(id) * 1 + m.Cmt(2, id) * 1  + m.Cmt(1, id) * 1))**0
    assert_equal "137" * 4, p.match('abababab').join

    ref = ->(s, i, x, *) { m.match(x, s, i - x.length) }
    assert_equal 3, m.Cmt(m.P(1)**0, ref).match('alo')
    assert_equal 3, (m.P(1) * m.Cmt(m.P(1)**0, ref)).match('alo')
    assert_nil (m.P(1) * m.Cmt(m.C(1)**0, ref)).match('alo')

    ref = ->(s, i, x) { [i == x.to_i && i, 'xuxu'] }

    assert m.Cmt(1, ref).match('1')
    assert_nil m.Cmt(1, ref).match('0')
    assert m.Cmt(m.P(1)**0, ref).match('02')

    ref = ->(_s, i, a, b) { [i, a.upcase] if a == b }
    p = m.Cmt(m.C(m.R("az")**1) * "-" * m.C(m.R("az")**1), ref)
    p = (any - p)**0 * p * any**0 * -1
    assert_equal "BC", p.match('abbbc-bc ddaa')

    # Lua-style "long strings"
    c = '[' * m.Cg(m.P('=')**0, "init") * '[' *
        ([
           m.Cmt(
             ']' * m.C(m.P('=')**0) * ']' * m.Cb("init"),
             ->(_x, _y, s1, s2) { s1 == s2 }
           ) + 1 * m.V(0)
         ]) / 0

    assert_equal 17, c.match('[==[]]====]]]]==]===[]')
    assert_equal 13, c.match('[[]=]====]=]]]==]===[]')
    assert_nil c.match('[[]=]====]=]=]==]===[]')

    # -- match-time captures cannot be optimized away
    touch = 0
    f = m.P(->(*) { touch = touch + 1; true })
    check = lambda do |n = 1|
      assert_equal n, touch
      touch = 0
    end

    assert_nil m.match(f * false + 'b', 'a'); check.call
    assert_nil m.match(f * false + 'b', ''); check.call
    assert_equal 1, m.match( (f * 'a')**0 * 'b', 'b'); check.call
    assert_nil m.match( (f * 'a')**0 * 'b', ''); check.call
    assert_equal 1, m.match( (f * 'a')**-1 * 'b', 'b'); check.call
    assert_nil m.match( (f * 'a')**-1 * 'b', ''); check.call
    assert_nil m.match( ('b' + f * 'a')**-1 * 'b', ''); check.call
    assert_nil m.match( (m.P('b')**-1 * f * 'a')**-1 * 'b', ''); check.call
    assert_nil m.match( (-m.P(1) * m.P('b')**-1 * f * 'a')**-1 * 'b', ''); check.call
    assert_nil m.match( (f * 'a' + 'b')**-1 * 'b', ''); check.call
    assert_equal 1, m.match(f * 'a' + f * 'b', 'b'); check.call(2)
    assert_equal 1, m.match(f * 'a' + f * 'b', 'a'); check.call(1)
    assert_equal 1, m.match(-f * 'a' + 'b', 'b'); check.call(1)
    assert_nil m.match(-f * 'a' + 'b', ''); check.call(1)

    p = (m.P(->(*) { [true, "a"] }) * 'a' +
         m.P(->(s, i, *) { [i, "aa", 20] }) * 'b' +
         m.P(->(s, i, *) { [i, "aaa"] if i <= s.length} ) * 1)**0

    assert_equal ['a', 'aa', 20, 'a', 'aaa', 'aaa'],  p.match('abacc')
  end

  def test_re
    assert_equal 1, RE.match("a", ".")
    assert_equal 0, RE.match("a", "''")
    assert_equal 0, RE.match("", " ! . ")

    assert_nil RE.match("a", " ! . ")
    assert_equal 4, RE.match("abcde", "  ( . . ) * ")
    assert_equal 4, RE.match("abbcde", " [a-c] +")
    assert_equal 6, RE.match("0abbc1de", "'0' [a-c]+ '1'")
    assert_equal 7, RE.match("0zz1dda", "'0' [^a-c]+ 'a'")
    assert_equal 4, RE.match("abbc--", " [a-c] + +")
    assert_equal 1, RE.match("abbc--", " [ac-] +")
    assert_equal 6, RE.match("abbc--", " [-acb] + ")
    assert_nil RE.match("abbcde", " [b-z] + ")
    assert_equal 6, RE.match("abb\"de", '"abb"["]"de"')
    assert_equal 'eee', RE.match("abceeef", "'ac' ? 'ab' * 'c' { 'e' * } / 'abceeef' ")
    assert_equal 7, RE.match("abceeef", "'ac'? 'ab'* 'c' { 'f'+ } / 'abceeef' ")

    assert_equal 2, RE.match("aaand", "[a]^2")

    assert_equal [3, 4, 6], RE.match("abceefe", "( ( & 'e' {} ) ? . ) * ")
    assert_equal [3, 4, 6], RE.match("abceefe", "((&&'e' {})? .)*")
    assert_equal [3, 4, 6], RE.match("abceefe", "( ( ! ! 'e' {} ) ? . ) *")
    assert_equal [3, 4, 6], RE.match("abceefe", "(( & ! & ! 'e' {})? .)*")

    assert_equal 4, RE.match("cccx" , "'ab'? ('ccc' / ('cde' / 'cd'*)? / 'ccc') 'x'+")
    assert_equal 3, RE.match("cdx" , "'ab'? ('ccc' / ('cde' / 'cd'*)? / 'ccc') 'x'+")
    assert_equal 7, RE.match("abcdcdx" , "'ab'? ('ccc' / ('cde' / 'cd'*)? / 'ccc') 'x'+")

    assert_equal 3, RE.match("abc", "a <- (. a)?")
    b = "balanced <- '(' ([^()] / balanced)* ')'"
    assert RE.match("(abc)", b)
    assert RE.match("(a(b)((c) (d)))", b)
    assert_nil RE.match("(a(b ((c) (d)))", b)

    b = RE.compile 'balanced <- "(" ([^()] / balanced)* ")"'
    assert_equal b, m.P(b)
    assert b.match("((((a))(b)))")

    g = <<~GRAMM
      S <- "0" B / "1" A / ""   -- balanced strings
      A <- "0" S / "1" A A      -- one more 0
      B <- "1" S / "0" B B      -- one more 1
    GRAMM
    assert_equal 8, RE.match("00011011", g)

    g = <<~GRAMM
      S <- ("0" B / "1" A)*
      A <- "0" / "1" A A
      B <- "1" / "0" B B
    GRAMM
    assert_equal 8, RE.match("00011011", g)
    assert_equal 8, RE.match("000110110", g)
    assert_equal 2, RE.match("011110110", g)
    assert_equal 0, RE.match("000110010", g)

    s = "aaaaaaaaaaaaaaaaaaaaaaaa"
    assert_equal 3, RE.match(s, "'a'^3")
    assert_equal 0, RE.match(s, "'a'^0")
    assert_equal s.length, RE.match(s, "'a'^+3")
    assert_nil RE.match(s, "'a'^+30")
    assert_equal s.length, RE.match(s, "'a'^-30")
    assert_equal 5, RE.match(s, "'a'^-5")
    (0...(s.length)).each do |i|
      assert RE.match(s, "'a'^+#{i}") >= i
      assert RE.match(s, "'a'^-#{i + 1}") <= i + 1 # careful! In the Lua tests we start with i == 1, but -0 == 0
      assert_equal i, RE.match(s, "'a'^#{i}")
    end
    assert_equal 18, RE.match("01234567890123456789", "[0-9]^3+")

    assert_equal "4560123", RE.match("01234567890123456789", "({....}{...}) -> '%2%1'")

    assert_equal %w[0 1 2 3 4 5 6 7 8 9], RE.match("0123456789", "{| {.}* |}")
    assert_equal "0101", RE.match("012345", "{| (..) -> '%0%0' |}")[0]

    assert_equal "c", RE.match("abcdef", "( {.} {.} {.} {.} {.} ) -> 3")
    assert_equal "d", RE.match("abcdef", "( {:x: . :} {.} {.} {.} {.} ) -> 3")
    assert_equal 5, RE.match("abcdef", "( {:x: . :} {.} {.} {.} {.} ) -> 0")

    assert_nil RE.match("abcdef", "{:x: ({.} {.} {.}) -> 2 :} =x")
    assert RE.match("abcbef", "{:x: ({.} {.} {.}) -> 2 :} =x")

    # -- tests for comments in 're'
    e = RE.compile <<~'GRAMM'
      A  <- _B   -- \t \n %nl .<> <- -> --
      _B <- 'x'  --
    GRAMM
    assert_equal 1, e.match('xy')

    defs = {digits: m.R("09"), letters: m.R("az"), _: m.P("__")}
    e = RE.compile("%letters (%letters / %digits)*", defs)
    assert_equal 4, e.match("x123")
    e = RE.compile("%_", defs)
    assert_equal 2, e.match("__")

    # Not used in the LPEG tests
    # e = compile(<<~GRAM
    #                S <- A+
    #                A <- %letters+ B
    #                B <- %digits+
    #             GRAM
    #             , defs)

    math = { sin: ->(s) { Math.sin(Float(s)) } }
    e = RE.compile("{[0-9]+'.'?[0-9]*} -> sin", math)
    assert_equal Math.sin(2.34), e.match("2.34")

    c = RE.compile(
      <<~GRAM
        longstring <- '[' {:init: '='* :} '[' close
        close <- ']' =init ']' / . close
      GRAM
    )
    assert_equal 16, c.match('[==[]]===]]]]==]===[]')
    assert_equal 13, c.match('[[]=]====]=]]]==]===[]')
    assert_nil c.match('[[]=]====]=]=]==]===[]')

    c = RE.compile(" '[' {:init: '='* :} '[' (!(']' =init ']') .)* ']' =init ']' !. ")
    assert c.match('[==[]]===]]]]==]')
    assert c.match('[[]=]====]=][]==]===[]]')
    assert_nil c.match('[[]=]====]=]=]==]===[]')

    # Note: when a Lua function f(foo) returns mlutiple values x, y the expression f(foo) == x is true
    assert_equal 3, RE.find("hi alalo", "{:x:..:} =x").first
    assert_equal 3, RE.find("hi alalo", "{:x:..:} =x", 3).first
    assert_nil RE.find("hi alalo", "{:x:..:} =x", 4)
    assert_equal 5, RE.find("hi alalo", "{'al'}", 4).first
    assert_equal 7, RE.find("hi aloalolo", "{:x:..:} =x").first
    assert_equal 10, RE.find("alo alohi x x", "{:word:%w+:}%W*(=word)!%w").first

    # -- re.find discards any captures
    a, b, c = RE.find("alo", "{.}{'o'}")
    assert_equal 1, a
    assert_equal 2, b
    assert_nil c

    match = lambda do |s, p|
      i, e = RE.find(s,p)
      return s[i..e] if i
    end
    assert_equal "alo", match.call("alo alo", '[a-z]+')
    assert_nil match.call("alo alo", '{:x: [a-z]+ :} =x')
    assert_equal "alo alo", match.call("alo alo", "{:x: [a-z]+ :} ' ' =x")

    assert_equal "xlo xlo", RE.gsub("alo alo", "[abc]", "x")
    assert_equal ". .", RE.gsub("alo alo", "%w+", ".")
    assert_equal "hI, hOw ArE yOU", RE.gsub("hi, how are you", "[aeiou]", ->(c) { c.upcase } )

    s = 'hi [[a comment[=]=] ending here]] and [=[another]]=]]'
    c = RE.compile " '[' {:i: '='* :} '[' (!(']' =i ']') .)* ']' { =i } ']' "
    assert_equal 'hi  and =]', RE.gsub(s, c, "%2")
    assert_equal s, RE.gsub(s, c, "%0")
    assert_equal "=", RE.gsub('[=[hi]=]', c, "%2")

    assert_equal 0, RE.find("", "!.").first
    assert_equal 3, RE.find("alo", "!.").first

    # TODO: make this work.
    #
    # Problem: Lua's table is a Hashtable/array mashup. In particular, t.tag = foo adds apparently foo to the table without a key
    # when tag is nill, sort of like appending to an array. But the way we represent table captures in RPEG is messy and should be
    # rethought. It's constructions like this that suggest we should always return a hash, with a special key :anon for the array of
    # anonymous captures.
    #
    # addtag = lambda do |s, i, t, tag = nil|
    #   # This is a problem due to how we return 'table' captures. Ugh.
    #   t[:tag] = tag
    #   [i, t]
    # end

    # grammer = <<~GRAM
    #   doc <- block !.
    #   block <- (start {| (block / { [^<]+ })* |} end?) => addtag
    #   start <- '<' {:tag: [a-z]+ :} '>'
    #   end <- '</' { =tag } '>'
    # GRAM

    # c = RE.compile(grammer, { addtag: })
    # x = c.match('<x>hi<b>hello</b>but<b>totheend</x>')
    # assert_equal ({ tag: 'x', 0 => 'hi', 1 => { tag: 'b', 0 => 'hello' }, 2 => 'but', 3 => ['totheend'] }), x

    # -- test for folding captures
    grammar = <<~GRAM
      S <- (number (%s+ number)*) ~> add
      number <- %d+ -> tonumber
    GRAM
    c = RE.compile(
      grammar,
      { tonumber: ->(str) { Integer(str) }, add: ->(a,b) { a + b } }
    )
    assert_equal 3 + 401 + 50, c.match("3 401 50")

    # -- tests for look-ahead captures
    assert_equal ["", "alo", ""], RE.match("alo", "&(&{.}) !{'b'} {&(...)} &{..} {...} {!.}")

    assert_equal "AallooAalloo",
                 RE.match(
                   "aloalo",
                   "{~ (((&'al' {.}) -> 'A%1' / (&%l {.}) -> '%1%1') / .)* ~}"
                 )

    # -- bug in 0.9 (and older versions), due to captures in look-aheads
    x = RE.compile " {~ (&(. ([a-z]* -> '*')) ([a-z]+ -> '+') ' '*)* ~} "
    assert_equal "+ +", x.match("alo alo")

    # -- valid capture in look-ahead (used inside the look-ahead itself)
    x = RE.compile ' S <- &({:two: .. :} . =two) {[a-z]+} / . S '
    assert_equal "aloalo", x.match("hello aloaLo aloalo xuxu")

    # -- nested grammars
    p = RE.compile(
      <<~GRAM
        s <- a b !.
        b <- ( x <- ('b' x)? )
        a <- ( x <- 'a' x? )
      GRAM
    )
    assert p.match('aaabbb')
    assert p.match('aaa')
    assert_nil p.match('bbb')
    assert_nil p.match('aaabbba')

    # -- testing nested substitutions x string captures

    p = RE.compile(
      <<~GRAM
        text <- {~ item* ~}
        item <- macro / [^()] / '(' item* ')'
        arg <- ' '* {~ (!',' item)* ~}
        args <- '(' arg (',' arg)* ')'
        macro <- ('apply' args) -> '%1(%2)'
               / ('add' args) -> '%1 + %2'
               / ('mul' args) -> '%1 * %2'
      GRAM
    )
    assert_equal "a * b + f(x)", p.match("add(mul(a,b), apply(f,x))")

    rev = RE.compile(" R <- (!.) -> '' / ({.} R) -> '%2%1' ")
    assert_equal "9876543210", rev.match("0123456789")

    # -- testing groups
    assert_equal ["a", "bc", "b", "c", "c", ""], RE.match("abc", "{:S <- {:.:} {S} / '':}")

    assert_equal ({ "a" => "1", "b" => "2", "c" => "4" }), RE.match("1234", "{| {:a:.:} {:b:.:} {:c:.{.}:} |}")
    assert_equal ({ "a" => "1", "b" => "2", "c" => "4" }), RE.match("1234", "{|{:a:.:} {:b:{.}{.}:} {:c:{.}:}|}")

    # TODO: test this once the table capture format is settled
    # t = re.match("12345", "{| {:.:} {:b:{.}{.}:} {:{.}{.}:} |}")
    # checkeq(t, {"1", b="2", "4", "5"})

    assert_equal %w[1 23 4 5], RE.match("12345", "{| {:.:} {:{:b:{.}{.}:}:} {:{.}{.}:} |}")
    assert_equal %w[1 23 4 5], RE.match("12345", "{| {:.:} {{:b:{.}{.}:}} {:{.}{.}:} |}")
  end

  # For isolating a failing test. Run with the -n flag to ruby.
  def test_onceler
  end

  # Notes on possible targets for profiling
  #  See "big" in test_table_captures

  # Helpers to make it easier to use the tests copied from the Lua code
  def m
    Pattern
  end

  # If pattern is a lambda call it; otherwise just use it
  def assert_match_raises_error(message, pattern, *args)
    message = /#{Regexp.quote message}/ if message.is_a?(String)
    assert_raise_message(message) { m.P(pattern.is_a?(Proc) ? pattern.call : pattern).match(*args) }
  end

  def a_lambda
    @a_lambda ||= ->(x) { x }
  end

  def nullable?(patt)
    m.P(patt).nullable?
  end

  # Convert a charset pattern to a string
  def cs2str(c)
    m.match(m.Cs((c + m.P(1)/"")**0), allchars)
  end

  # Check that two charset patterns describe equal sets
  def eqcharset(c1, c2)
    assert_equal cs2str(c1), cs2str(c2)
  end

  # All 1-byte character codes in a string
  def allchars; @all_chars ||= m::FULL_CHAR_SET.join; end

  def digit; m.S("0123456789"); end
  def upper; m.S("ABCDEFGHIJKLMNOPQRSTUVWXYZ"); end
  def lower; m.S("abcdefghijklmnopqrstuvwxyz"); end
  def letter; m.S("") + upper + lower; end
  def alpha; letter + digit + m.R(); end
  def word; alpha**1 * (1 - alpha)**0; end
  def eos; m.P(-1); end
  def any; m.P(1); end
  def space; m.S(" \t\n")**0; end
end
