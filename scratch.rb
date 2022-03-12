#!/usr/bin/env ruby

require 'byebug'

require_relative 'rpeg'
require_relative 're'

# @text = ARGF.each_line.to_a.join("\n")

# def search(p)
#   p = RE.compile(p)
#   RPEG.P [p + 1 * RPEG.V(0)]
# end

# string = RPEG.P('Alpha ')
# searcher = search(string)

# start = Time.now
# puts searcher.match(@text)

# puts "Found in #{Time.now - start} s"

# Pattern to match balanced parentheses
patt1 = RPEG.P( [ "(" * ((1 - RPEG.S("()")) + RPEG.V(0))**0 * ")" ] )
patt2 = patt1 * -1

puts patt2.match "(()()(()))"  # 10
puts patt2.match "(()()(())"  # nil
