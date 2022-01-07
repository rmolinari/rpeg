# See the README file for a little context. And also:
#
#   http://www.inf.puc-rio.br/~roberto/lpeg/#func
#

require 'set'
require 'must_be'

# This class is intended to play the same role as LPEG's lpeg module. I don't yet have any real understanding of how that code works
# so this code is liable to change a lot.
class Pattern
  NODE_TYPES = %i[charset char string any].freeze

  attr_reader :type, :data, :program

  class << self
    protected :new

    # Match any character in string (regarded as a set of characters)
    def S(string)
      new(:charset, string.chars)
    end

    # Match string literally, as a sequence of chars
    # when arg is a non-negative integer, match exactly that many chars
    def P(arg)
      case arg
      when String
        new(:string, arg)
      when Integer
        raise "Negative value not allowed for #P" unless arg >= 0
        new(:any, arg)
      end
    end

    # Given a 2-char string xy, the ASCII range x..y
    def R(str)
      raise "Bad data #{str} for Pattern#R" unless str.is_a?(String) && str.size == 2

      new(:charset, (str[0])..(str[1]))
    end
  end

  # Return the index just after the matching prefix of str or null if there is no match
  def match(str)
    @program ||= Compiler.new(self).compile

    machine = ParsingMachine.new(@program, str)
    machine.run

    if machine.success?
      machine.final_index
    else
      nil
    end
  end

  def initialize(type, data)
    raise "Bad node type #{type}" unless NODE_TYPES.include?(type)

    @type = type
    @data = data

    @program = nil
  end
end

class Compiler
  def initialize(pattern)
    @pattern = pattern.must_be_a Pattern

    @next_label = 0

    @first_label = get_next_label
    @first_element = true
  end

  # 2-pass: first time through we generate an array of [label, op_code, arg1, arg2] tuples. label may well be nil.
  # On the second pass we resolve the (symbolic) labels to integers and then convert the references to relative offset
  def compile
    link(first_pass)
  end

  private

  def link(with_symbolics)
    # First go through and generate line numbers for each label
    label_lines = {}
    with_symbolics.each_with_index do |command, line|
      label, = command
      next unless label

      raise "Label #{label} has already been seen" if label_lines[label]
      label_lines[label] = line
    end

    prog = []

    # Now pass through again and work out the offsets
    with_symbolics.each_with_index do |command, line|
      _, op, *args = command

      args = args.map do |arg|
        fst, = arg
        if fst == :label
          absolute = label_lines[arg]
          raise "#{arg} does not appear as a label but #{command} uses is" unless absolute
          absolute - line
        else
          arg
        end
      end

      prog << [op, *args]
    end

    prog
  end

  def first_pass
    # brainless for now
    prog = case @pattern.type
           when :charset, :string, :any
             simple_construction
           else
             raise "Don't know how to generate code for type #{@pattern.type}"
           end

    # give an address to the first line of prog
    prog[0][0] = @first_label

    last_label = get_next_label

    full_prog = [
      [nil, :call, @first_label],
      [nil, :jump, last_label]
    ]
    full_prog += prog
    full_prog << [last_label, :end]
  end

  def simple_construction
    prog = case @pattern.type
           when :charset
             [[nil, :charset, Set.new(@pattern.data)]]
           when :string
             @pattern.data.chars.map { [nil, :char, _1] }
           when :any
             [[nil, :any, @pattern.data]]
           else
             raise "#{@pattern.type} is not a simple construction"
           end

    prog << [nil, :return]
  end

  def get_next_label
    @next_label += 1
    [:label, @next_label]
  end
end

class ParsingMachine
  OP_CODES = %i[char charset any call jump end fail return]
  # subject is the string to match against

  attr_reader :final_index

  def initialize(program, subject)
    @program = program.clone.freeze
    @prog_len = @program_size
    @subject = subject.clone.freeze

    @current_instruction = 0
    @current_subject_position = 0
    @stack = []
    @capture_list = []
  end

  def success?
    @success
  end

  def run
    loop do
      if @current_instruction == :fail
        # special handling
        if @stack.empty?
          @success = false
          return
        end

        top = @stack.pop
        if top.is_a?(Numeric)
          # nothing more to do
        else
          p, i, c = top
          @current_instruction = p
          @current_subject_position = i
          @capture_list = c
        end

        next
      end

      raise "current instruction #{@current_instruction} is out of range" unless (0...@prog_len).include?(@current_instruction)

      op_code, arg1, _arg2 = @program[@current_instruction]
      raise "bad opcode #{op_code}" unless OP_CODES.include?(op_code)

      match_char_p = lambda do |success|
        if success
          @current_instruction += 1
          @current_subject_position += 1
        else
          @current_instruction = :fail
        end
      end

      case op_code
      when :char
        # arg1 is a single character
        match_char_p.call(arg1 == @subject[@current_subject_position])
      when :charset
        match_char_p.call(arg1.include?(@subject[@current_subject_position]))
      when :any
        # arg1 is the number of chars we are looking for
        if @current_subject_position + arg1 < @subject.size
          @current_instruction += 1
          @current_subject_position += arg1
        else
          @current_instruction = :fail
        end
      when :jump
        # arg1 is an offset for the label to jump to
        @current_instruction += arg1
      when :call
        # arg1 is an offset for the label to call.
        #
        # Call is like jump, but we push the return address onto the stack first
        @stack.push(@current_instruction + 1)
        @current_instruction += arg1
      when :return
        ret_address = @stack.pop
        raise "Nothing on stack for return" unless ret_address
        raise "Expected instruction pointer on stack for return" unless ret_address.is_a?(Integer)
        @current_instruction = ret_address
      when :end
        @success = true
        @final_index = @current_subject_position
        return
      else
        raise "Unsupported op code #{op_code}"
      end
    end
  end
end
