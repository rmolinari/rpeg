#!/usr/bin/env ruby

require 'byebug'

require_relative '../lib/rpeg'

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

m = RPEG

grammar = m.P(
  {
    initial: :list,
    space: m.S(" \t")**0,
    number: m.R('09')**1 / ->(str) { str.to_i },
    separator: m.P(',') * m.V('space'),
    term: m.V('number') + m.V('list'),
    sequence: (m.V('term') * (m.V('separator') * m.V('sequence'))**-1) + m.P(true),
    # Wart: We need to initialize the accumulator in the reduce function. If we try m.Cc([]) then the accumulator array is set up
    # once and for all when the capture is triggered, and everything, at every level, is appended to that array, which is a mess. So
    # we capture nil and bootstrap with a fresh empty array when we fold in the first value.
    #
    # Note that we need to capture the nil first up so that the reducer can detect that we are just starting the list. If we don't
    # capture nil and instead try something like ->(acc, v) { Array(acc) << v } then, if the first element of the list is itself a
    # list, we end up splicing it in (like a flatten) rather than adding it as the first element of the list.
    #
    # But this is still broken! If we are parsing the empty list "[]" then the reducer will never get called and the result of the
    # parse is just nil, which is wrong.
    #
    # So try this: group all the captures - even zero - in a table, and then map that table to an Array. Because of another wart -
    # the need to distingish between a function that represents multiple captures [1,2,3] and one that represents a single capture
    # that is an array [[1,2,3]] - we have to wrap the result
    list: RPEG.Ct(RPEG.S('[') * RPEG.V('sequence') * RPEG.S(']')) / ->(table) { table.is_a?(Enumerable) ? [table] : [table.unpack] }
  }
)
nested_things = grammar * -1

# byebug
pp nested_things.match "[]"
pp nested_things.match "[1,2,3]"
pp nested_things.match "[[1], 2, 3]"
pp nested_things.match "[[[[[1], 1], 1], 1], 1]"

equalcount = RPEG.P( {
  initial: :S,                         # initial rule name
  S: "a" * RPEG.V(:B) + "b" * RPEG.V(:A) + "",
  A: "a" * RPEG.V(:S) + "b" * RPEG.V(:A) * RPEG.V(:A),
  B: "b" * RPEG.V(:S) + "a" * RPEG.V(:B) * RPEG.V(:B)
} ) * -1

pp equalcount.match "ababab" # -> 6
pp equalcount.match "abbbaa" # -> 6
pp equalcount.match "aabba"  # -> nil

puts
equalcount_arr = RPEG.P( [
  0,                         # initial rule index
  "a" * RPEG.V(2) + "b" * RPEG.V(1) + "",
  "a" * RPEG.V(0) + "b" * RPEG.V(1) * RPEG.V(1),
  "b" * RPEG.V(0) + "a" * RPEG.V(2) * RPEG.V(2)
]) * -1

pp equalcount_arr.match "ababab" # -> 6
pp equalcount_arr.match "abbbaa" # -> 6
pp equalcount_arr.match "aabba"  # -> nil

# Try something similar with the RE library. CURRENT COMPLETELY BROKEN
# re_grammar = <<~GRAM
#   list <- (('' -> nul) '[' sequence ']') ~> append
#   sequence <- (term (separator sequence)?)?
#   term <- number / list
#   separator <- ',' space
#   space <- ' '*
#   number <- [0-9]+ -> to_int
# GRAM
# int_parser = RE.compile(
#   re_grammar,
#   {
#     nul: ->(*) { :nil },
#     to_int: ->(s) { Integer(s) },
#     append: ->(acc, v) { (acc == :nil ? [] : acc) << v},
#   }
# ) * -1

# pp int_parser.match("[]")

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
