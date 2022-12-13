#!/usr/bin/env ruby

require 'byebug'

require_relative 'rpeg'
require_relative 're'

# @text = ARGF.each_line.to_a.join("\n")

# def search(p)
#   p = RE.compile(p)
#   RPEG.P [p + 1 * RPEG.V(0)]
# end

# Experiment to parse and return lists of integers, allowing nesting
#
# - []
# - [1, 2, 3]
# - [[1,2,3], 4, [[[[[1]]]]]]
#
# etc.

grammar = RPEG.P(
  {
    initial: :list,
    space: RPEG.S(" \t")**0,
    number: RPEG.R('09')**1 / ->(str) { str.to_i },
    separator: RPEG.P(',') * RPEG.V('space'),
    term: RPEG.V('number') + RPEG.V('list'),
    sequence: (RPEG.V('term') * (RPEG.V('separator') * RPEG.V('sequence'))**-1) + RPEG.P(true),
    # Wart: we need to clone the accumulator in the capture function because the empty array is only ever created once, when we call
    # RPEG.Cc. So to avoid simply cramming things into the same array every time we need to clone it in the accumulator.
    list: RPEG.Cf(RPEG.Cc([]) * RPEG.S('[') * RPEG.V('sequence') * RPEG.S(']'), ->(acc, v) { acc.clone << v }),
  }
)
nested_things = grammar * -1

# byebug
pp nested_things.match "[1,2,3]"
pp nested_things.match "[[1], 2, 3]"
pp nested_things.match "[[[[[1], 1], 1], 1], 1]"

# string = RPEG.P('Alpha ')
# searcher = search(string)

# start = Time.now
# puts searcher.match(@text)

# puts "Found in #{Time.now - start} s"

# Pattern to match balanced parentheses
# patt1 = RPEG.P( [ "(" * ((1 - RPEG.S("()")) + RPEG.V(0))**0 * ")" ] )
# patt2 = patt1 * -1

# puts patt2.match "(()()(()))"  # 10
# puts patt2.match "(()()(())"  # nil
# puts patt2.match "()z"
