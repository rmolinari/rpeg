#!/usr/bin/env ruby

require_relative 'pattern'

def assert(result)
  raise "assertion failed" unless result
end

def assert_nil(result)
  assert !result
end

m = Pattern

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
