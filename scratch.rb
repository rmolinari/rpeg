#!/usr/bin/env ruby

require 'byebug'

require_relative 'rpeg'
require_relative 're'

@text = ARGF.each_line.to_a.join("\n")

def search(p)
  p = RE.compile(p)
  RPEG.P [p + 1 * RPEG.V(0)]
end

string = RPEG.P('Jesus')
searcher = search(string)

start = Time.now
puts searcher.match(@text)

puts "Found in #{Time.now - start} s"
