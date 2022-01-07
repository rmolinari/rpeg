require 'test/unit'
require_relative 'pattern'

require 'byebug'

class PatternTest < Test::Unit::TestCase
  def test_one_char_pattern
    patt = Pattern.S("abc")

    assert_equal 1, patt.match("axy")
    assert_equal nil, patt.match("dxy")
    assert_equal nil, patt.match("yxa") # matching is from the start of string
  end

  def test_string_pattern
    patt = Pattern.P("abc")

    assert_equal nil, patt.match("axy")
    assert_equal 3, patt.match("abcxyz")
    assert_equal nil, patt.match("xabcyz")
  end

  def test_any_n_chars
    # any 3 characters
    patt = Pattern.P(3)

    assert_equal nil, patt.match("")
    assert_equal nil, patt.match("a")
    assert_equal nil, patt.match("ab")
    assert_equal 3, patt.match("abcd")
    assert_equal 3, patt.match("abcde")
  end

  def test_ranges
    # any lower case letter
    patt = Pattern.R("az")

    assert_equal 1, patt.match("axy")
    assert_equal 1, patt.match("xya")
    assert_equal nil, patt.match("AXY")
    assert_equal nil, patt.match("AxY") # matching is from the start of string
  end

  def test_basic_concat
    patt1 = Pattern.P("x")
    patt2 = Pattern.R("09")
    patt = patt1 * patt2

    assert_equal 2, patt.match("x0")
    assert_equal 2, patt.match("x4")
    assert_equal 2, patt.match("x9")
    assert_nil patt.match("x")
    assert_nil patt.match("9")
    assert_nil patt.match("xx")
    assert_nil patt.match("a9")
  end

  def test_basic_choice
    patt1 = Pattern.R("az")
    patt2 = Pattern.R("09")

    patt3 = Pattern.P("abc")

    # Matches either a lower case letter or a digit
    patt_a = patt1 + patt2

    # Matches "abc" or a digit
    patt_b = patt3 + patt2

    assert_equal 1, patt_a.match("xY")
    assert_equal 1, patt_a.match("x2")
    assert_equal 1, patt_a.match("1x")
    assert_equal 1, patt_a.match("1X")
    assert_nil patt_a.match("X") #upper case
    assert_nil patt_a.match("!")

    assert_equal 3, patt_b.match("abc0")
    assert_equal 1, patt_b.match("0abc")
    assert_nil patt_b.match("a")
    assert_nil patt_b.match("ab")
    assert_nil patt_b.match("ab0")
  end

  def test_simple_repetition
    patt = Pattern.P("a")

    rep_patts = []

    (0..10).each do |count|
      p = (rep_patts[count] ||= patt ** count)
      p1 = (rep_patts[count+1] ||= patt ** (count+1))

      assert_equal count, p.match("a" * count), "match failed for repeat count #{count}"
      assert_equal nil, p1.match("a" * count), "match failed for repeat count #{count}"
    end
  end

  def test_alphanum_identifiers
    alpha = Pattern.R("az") + Pattern.R("AZ") + Pattern.S("_")
    digit = Pattern.R("09")
    alphanum = alpha + digit

    identifier = alpha * alphanum**0

    assert_equal 5, identifier.match("a1_23")
    assert_equal 4, identifier.match("a123%%")
    assert_equal nil, identifier.match("123")
  end
end
