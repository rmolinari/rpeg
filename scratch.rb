#!/usr/bin/env ruby

class UnaryOps
  def initialize(x)
    @x = x
  end

  define_method(",") do
    @x + 1
  end

  def &
    @x + 3
  end

  define_method(",") do
    @x + 1
  end
end

foo = UnaryOps.new(1)

puts foo.send(",")
puts &foo
