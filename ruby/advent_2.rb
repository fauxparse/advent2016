#!/usr/bin/env ruby

class Decoder
  attr_reader :x, :y

  def initialize(x = 2, y = 2)
    @x = x
    @y = y
  end

  def move(char)
    case char
    when 'U' then @y = [y - 1, 1].max
    when 'D' then @y = [y + 1, 3].min
    when 'L' then @x = [x - 1, 1].max
    when 'R' then @x = [x + 1, 3].min
    end
  end

  def digit
    ((y - 1) * 3 + x).to_s
  end
end

instructions = File.readlines('./input/2.txt').map(&:chomp)
# instructions = %w(ULL RRDDD LURDL UUUUD)

decoder = Decoder.new
code = instructions.map do |line|
  line.chars.each(&decoder.method(:move))
  decoder.digit
end.join

puts code
