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
end
