require 'test/unit'
require_relative 'pattern'

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
    # TODO: implement enough support for these
    #
    # The LPEG tests use these to check that (internal) charset calculations are correct. Extracting the charsets from inside
    # patterns apparently required captures - which is what the tests use - but we don't have those yet. See eqcharset in the Lua
    # test code

    # eqcharset(m.S"", m.P(false))
    # eqcharset(upper, m.R("AZ"))
    # eqcharset(lower, m.R("az"))
    # eqcharset(upper + lower, m.R("AZ", "az"))
    # eqcharset(upper + lower, m.R("AZ", "cz", "aa", "bb", "90"))
    # eqcharset(digit, m.S"01234567" + "8" + "9")
    # eqcharset(upper, letter - lower)
    # eqcharset(m.S(""), m.R())
    # assert(cs2str(m.S("")) == "")

    # eqcharset(m.S"\0", "\0")
    # eqcharset(m.S"\1\0\2", m.R"\0\2")
    # eqcharset(m.S"\1\0\2", m.R"\1\2" + "\0")
    # eqcharset(m.S"\1\0\2" - "\0", m.R"\1\2")
  end

  def test_predicates
    # -- tests for predicates
    assert_nil m.match(-m.P("a") * 2, "alo")
    assert_equal 2, m.match(- -m.P("a") * 2, "alo")
    assert_equal 2, m.match(+m.P("a") * 2, "alo")
    assert_equal 2, m.match(++m.P("a") * 2, "alo")
    assert_nil m.match(++m.P("c") * 2, "alo")
    # TODO: uncomment when we support the / operator for captures
    # assert_equal "a..a.", m.match(m.Cs((++m.P("a") * 1 + m.P(1)/".")^0), "aloal")
    # assert_equal "a..a.", m.match(m.Cs((+((+m.P"a")/"") * 1 + m.P(1)/".")^0), "aloal")
    # assert_equal "a..a.", m.match(m.Cs((- -m.P("a") * 1 + m.P(1)/".")^0), "aloal")
    # assert_equal "a..a.", m.match(m.Cs((-((-m.P"a")/"") * 1 + m.P(1)/".")^0), "aloal")

    # -- look-behind predicate
    assert_nil m.match(m.B('a'), 'a')
    assert_equal 1, m.match(1 * m.B('a'), 'a')
    assert_nil m.match(m.B(1), 'a')
    assert_equal 1, m.match(1 * m.B(1), 'a')
    assert_equal 0, m.match(-m.B(1), 'a')
    assert_nil m.match(m.B(250), 'a' * 250)
    assert_equal 250, m.match(250 * m.B(250), 'a' * 250)
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

    # Note we use index 0 rather than 1 because Lua is 1-based
    b = { A: "(" * (((1 - m.S("()")) + +m.P("(") * m.V(0))**0) * ")" }

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
    isnullable(m.P("x")**-4)
    assert_equal 2, m.match(((m.P(0) + 1) * m.S("al"))**0, "alo")
    assert_equal 2, m.match((("x" + +m.P(1))**-4 * m.S("al"))**0, "alo")
    isnullable("")
    isnullable(m.P("x")**0)
    isnullable(m.P("x")**-1)
    isnullable(m.P("x") + 1 + 2 + m.P("a")**-1)
    isnullable(-m.P("ab"))
    isnullable(- -m.P("ab"))
    isnullable(+ +(m.P("ab") + "xy"))
    isnullable(- +(m.P("ab")**0))
    isnullable(+ -m.P("ab")**1)
    isnullable(+m.V(3))
    isnullable(m.V(3) + m.V(1) + m.P('a')**-1)
    isnullable({ S: m.V(1) * m.V(2), T: m.V(2), U: m.P(0) })
    assert_equal 2, m.match(m.P({ S: m.V(1) * m.V(2), T: m.V(2), U: m.P(1) })**0, "abc")
    assert_equal 0, m.match(m.P("")**-3, "a")
  end

  # test.lua ll.918-943
  def test_bad_grammar
    bad_grammar = lambda do |grammar, expected_pattern|
      expected_pattern = /#{expected_pattern}/ if expected_pattern.is_a?(String)
      assert_raise_message(expected_pattern) { m.P(grammar) }
    end

    bad_grammar.call({ S: m.V(0) }, "rule 'S'")
    bad_grammar.call({ S: m.V(1) }, "rule '1'")   # invalid non-terminal
    bad_grammar.call({ S: m.V("x") }, "rule 'x'")   # invalid non-terminal
    bad_grammar.call({ S: m.V({}) }, "rule '{}'")   # invalid non-terminal
    bad_grammar.call({ S: +m.P("a") * m.V(0) }, "rule 'S'")  # left-recursive
    bad_grammar.call({ S: -m.P("a") * m.V(0) }, "rule 'S'")  # left-recursive
    bad_grammar.call({ S: -1 * m.V(0) }, "rule 'S'")  # left-recursive
    bad_grammar.call({ S: -1 + m.V(0) }, "rule 'S'")  # left-recursive
    bad_grammar.call({ S: 1 * m.V(1), T: m.V(1) }, "rule 'T'") # left-recursive
    bad_grammar.call({ S: 1 * m.V(1)**0, T: m.P(0) }, "rule 'S'") # inf. loop
    bad_grammar.call({ S: m.V(1), T: m.V(2)**0, U: m.P("") }, "rule 'T'") # inf. loop
    bad_grammar.call({ S: m.V(1) * m.V(2)**0, T: m.V(2)**0, U: m.P("") }, "rule 'S'") # inf. loop
    bad_grammar.call({ S: +(m.V(0) * 'a') }, "rule 'S'") # inf. loop
    bad_grammar.call({ S: -(m.V(0) * 'a') }, "rule 'S'") # inf. loop
    bad_grammar.call({ x: m.P('a')**-1 * m.V("x") }, "rule 'x'") # left recursive
    bad_grammar.call({ x: m.P('a') * m.V("y")**1, y: +m.P(1) }, "rule 'x'")

    assert_equal 1, m.match({ x: 'a' * -m.V(0) }, "aaa")
    assert_nil m.match({ x: 'a' * -m.V(0) }, "aaaa")
  end

  def test_const_capture
    # test.lua l.170
    # -- bug in LPeg 0.12  (nil value does not create a 'ktable')
    assert_equal nil, m.match(m.Cc(nil), "")

    # test.lua l.176
    assert_equal [10, 20, 30, 1], m.match(m.Cc(10, 20, 30) * 'a' * m.Cp(), 'aaa')
    assert_equal [0, 10, 20, 30, 1], m.match(m.Cp() * m.Cc(10, 20, 30) * 'a' * m.Cp(), 'aaa')
    # assert_equal [0, 10, 20, 30, 1], m.match(m.Ct(m.Cp() * m.Cc(10, 20, 30) * 'a' * m.Cp()), 'aaa')
    # assert_equal [0, 7, 8, 10, 20, 30, 1], m.match(m.Ct(m.Cp() * m.Cc(7, 8) * m.Cc(10, 20, 30) * 'a' * m.Cp()), 'aaa')
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

    # assert(m.match(m.Cmt(m.Cg(m.Carg(3), "a") *
    #                      m.Cmt(m.Cb("a"), function (s,i,x)
    #                                         assert(s == "a" and i == 1);
    #                                         return i, x+1
    #                                       end) *
    #                      m.Carg(2), function (s,i,a,b,c)
    #                                   assert(s == "a" and i == 1 and c == nil);
    # 				  return i, 2*a + 3*b
    #                                 end) * "a",
    #                "a", 1, false, 100, 1000) == 2*1001 + 3*100)
  end

  def test_simple_captures
    assert_equal %w[123 d], m.match(m.C(digit**1 * m.Cc("d")) + m.C(letter**1 * m.Cc("l")), "123")
    assert_equal %w[abcd l], m.match(m.C(digit**1 * m.Cc("d")) + m.C(letter**1 * m.Cc("l")), "abcd")

    # $do_it = true
    assert_equal ["abc", "a", "bc", "b", "c", "c", ""], m.match({ S: m.C(m.C(1) * m.V(0) + -1) }, "abc")

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

    # TODO: comment out when we have /-captures
    # p = m.Cg(m.P(true)/function () end * 1)   -- no value
    # assert(p:match('x') == 'x')

    p = m.Cg(m.Cg(m.Cg(m.C(1))))
    assert_equal 'x', p.match('x')
    p = m.Cg(m.Cg(m.Cg(m.C(1))**0) * m.Cg(m.Cc(1) * m.Cc(2)))
    assert_equal ['a', 'b', 'c', 1, 2], p.match('abc')
  end

  # Helpers to make it easier to use the tests copied from the Lua code
  def m
    Pattern
  end

  # If pattern is a lambda call it; otherwise just use it
  def assert_match_raises_error(message, pattern, *args)
    message = /#{message}/ if message.is_a?(String)
    assert_raise_message(message) { m.P(pattern.is_a?(Proc) ? pattern.call : pattern).match(*args) }
  end

  def isnullable(patt)
    m.P(patt).nullable?
  end

  def a_lambda
    @a_lambda ||= ->(x) { x }
  end

  def digit; m.S("0123456789"); end
  def upper; m.S("ABCDEFGHIJKLMNOPQRSTUVWXYZ"); end
  def lower; m.S("abcdefghijklmnopqrstuvwxyz"); end
  def letter; m.S("") + upper + lower; end
  def alpha; letter + digit + m.R(); end
  def word; alpha**1 * (1 - alpha)**0; end
  def eos; m.P(-1); end
end
