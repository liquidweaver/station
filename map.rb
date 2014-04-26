#!/usr/local/bin/ruby
require 'set'

all_objects = Set.new
STDIN.read.split("\n").each do |line|
  match = /"(\w+)" = \((.*?)\)/.match(line)
  if match
    ident, object_string = match.captures
    object_string.split(',').each do |object|
      removed_state = /^([^\{]+)/.match(object).captures[0]
      puts removed_state if removed_state.include? '{'
      all_objects.add removed_state
    end
  end
end

# all_objects.each { |o| puts o }
