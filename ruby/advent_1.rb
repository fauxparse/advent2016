#!/usr/bin/env ruby

class Location
  attr_reader :x, :y, :direction

  def initialize(x = 0, y = 0, direction = 0)
    @x = x
    @y = y
    @direction = direction
  end

  def left
    Location.new(x, y, (direction + 3) % 4)
  end

  def right
    Location.new(x, y, (direction + 1) % 4)
  end

  def forwards(blocks = 1)
    case direction
    when 0 then Location.visit(x, y - blocks, 0)
    when 1 then Location.visit(x + blocks, y, 1)
    when 2 then Location.visit(x, y + blocks, 2)
    when 3 then Location.visit(x - blocks, y, 3)
    end
  end

  def distance
    x.abs + y.abs
  end

  def process(instruction)
    blocks = instruction[1..-1].to_i
    turned = case instruction[0]
    when "L" then left
    when "R" then right
    else raise "Couldn’t process #{instruction}"
    end
    blocks.times { turned = turned.forwards }
    turned
  end

  def coordinates
    [x, y]
  end

  def self.visit(x, y, direction)
    Location.new(x, y, direction).tap do |location|
      visited(location) unless @first_visited
    end
  end

  def self.visited(location)
    @visited ||= {}
    if @visited[location.coordinates]
      puts "already visited #{location.coordinates} (#{location.distance})"
      @first_visited = true
    end
    @visited[location.coordinates] = true
  end
end

instructions = STDIN.read.chomp.split(/,\s*/)

final = instructions.inject(Location.new, :process)

puts final.coordinates.inspect
puts final.distance
