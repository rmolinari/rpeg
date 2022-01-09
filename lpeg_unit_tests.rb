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
    assert_equal 2, ("a" * m.P(true)).match("a")

    assert_equal nil, (+m.P(false) * "a").match("a")
    assert_equal 1, (+m.P(true) * "a").match("a")
    assert_equal nil, ("a" * +m.P(false)).match("a")
    assert_equal 1, ("a" * +m.P(true)).match("a")
  end

  def m
    Pattern
  end
end
