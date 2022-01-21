#!/usr/bin/env ruby

require_relative 'pattern'

def assert(result)
  raise "assertion failed" unless result
end

m = Pattern

p = m.P(true)

len = 500
len.times do
  p = 1 * p
end

assert p.match('1' * len)
assert !p.match('1' * (len - 1))
