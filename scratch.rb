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
    # Wart: We need to initialize the accumulator in the reduce function. If we try RPEG.Cc([]) then the accumulator array is set up
    # once and for all when the capture is triggered, and everything, at every level, is appended to that array, which is a mess. So
    # we capture nil and bootstrap with an empty array when we fold in the first value.
    list: RPEG.Cf(RPEG.Cc(nil) * RPEG.S('[') * RPEG.V('sequence') * RPEG.S(']'), ->(acc, v) { (acc || []) << v })
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
