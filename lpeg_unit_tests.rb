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

    # Note we use index 0 rather than 1 because Lua is 1-based
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

    # TODO: comment out when we have /-captures
    # p = m.Cg(m.P(true)/function () end * 1)   -- no value
    # assert(p:match('x') == 'x')

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
    assert_equal "alo", t.join

    t = m.match(m.Ct(m.C(m.C(letter)**1)), "alo")
    assert_equal "alo;a;l;o", t.join(";")

    t = m.match(m.Ct(m.C(m.C(letter)**1)), "alo")
    assert_equal "alo;a;l;o", t.join(";")

    t = m.match(m.Ct(m.Ct((m.Cp() * letter * m.Cp())**1)), "alo")
    assert_equal "0;1;1;2;2;3", t[0].join(";")

    assert_equal %w[alo a o], m.match(m.Ct(m.C(m.C(1) * 1 * m.C(1))), "alo")

    p = m.Ct(m.Cg(m.Cc(10), "hi") * m.C(1)**0 * m.Cg(m.Cc(20), "ho"))
    assert_equal ({"hi" => 10, "ho" => 20}), p.match('')
    assert_equal ({"hi" => 10, "ho" => 20, 0 => 'a', 1 => 'b', 2 => 'c'}), p.match('abc')

    # -- non-string group names
    p = m.Ct(m.Cg(1, a_lambda) * m.Cg(1, 23.5) * m.Cg(1, Kernel))
    assert_equal ({ a_lambda => 'a', 23.5 => 'b', Kernel => 'c'}), p.match('abcdefghij')

    # -- a large table capture
    #
    big = 1_000
    # big = 10_000 # Note: this is slow! About 800 ms
    t = m.match(m.Ct(m.C('a')**0), "a" * big)
    assert_equal big, t.size
    assert_equal 'a', t.first
    assert_equal 'a', t.last

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

  # For isolating a failing test. Run with the -n flag to ruby.
  def test_onceler
    p = m.P([
              '0' * m.V(1) + '1' * m.V(2) + -1,
              '0' * m.V(0) + '1' * m.V(3),
              '0' * m.V(3) + '1' * m.V(0),
              '0' * m.V(2) + '1' * m.V(1),
            ])

    assert p.match("00" * 10_000)
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
  def allchars; @allchars ||= (0..255).map(&:chr).join; end

  def digit; m.S("0123456789"); end
  def upper; m.S("ABCDEFGHIJKLMNOPQRSTUVWXYZ"); end
  def lower; m.S("abcdefghijklmnopqrstuvwxyz"); end
  def letter; m.S("") + upper + lower; end
  def alpha; letter + digit + m.R(); end
  def word; alpha**1 * (1 - alpha)**0; end
  def eos; m.P(-1); end
end
