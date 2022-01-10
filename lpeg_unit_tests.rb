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
    isnullable(- +m.P("ab")**0)
    isnullable(+ -m.P("ab")**1)
    isnullable(+m.V(3))
    isnullable(m.V(3) + m.V(1) + m.P('a')**-1)
    isnullable({ S: m.V(1) * m.V(2), T: m.V(2), U: m.P(0) })
    assert_equal 2, m.match(m.P({ S: m.V(1) * m.V(2), T: m.V(2), U: m.P(1) })**0, "abc")
    assert_equal 0, m.match(m.P("")**-3, "a")
  end

  ## ^^
  ## - up to l.151 in test.lua

  # Helpers to make it easier to use the tests copied from the Lua code
  def m
    Pattern
  end

  def isnullable(patt)
    assert Analysis.nullable?(patt)
  end

  def digit; m.S("0123456789"); end
  def upper; m.S("ABCDEFGHIJKLMNOPQRSTUVWXYZ"); end
  def lower; m.S("abcdefghijklmnopqrstuvwxyz"); end
  def letter; m.S("") + upper + lower; end
  def alpha; letter + digit + m.R(); end
  def word; alpha**1 * (1 - alpha)**0; end
  def eos; m.P(-1); end
end
