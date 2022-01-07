# See the README file for a little context. And also:
#
#   http://www.inf.puc-rio.br/~roberto/lpeg/#func
#

require 'set'
require 'must_be'
require 'ostruct'

# This class is intended to play the same role as LPEG's lpeg module. I don't yet have any real understanding of how that code works
# so this code is liable to change a lot.
class Pattern
  NODE_TYPES = %i[charset char string any concat ordered_choice].freeze

  attr_reader :type, :data, :program

  class << self
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

  ########################################
  # Operator overloading
  #
  # The LPEG library makes heavy use of operator overriding in Lua to combine patterns in a convenient way. We will follow.

  # p1 * p2 is matches p1 followed by p2
  def *(other)
    check_type(other)
    Pattern.new(:concat, [self, other])
  end

  # p1 + p2 is ordered choice: if p1 matches we match, otherwise try matching on p2
  def +(other)
    check_type(other)

    # TODO: if self and other are both :charset, combine the sets rather than form an :ordered_choice
    Pattern.new(:ordered_choice, [self, other])
  end

  private def check_type(other)
    raise "Cannot coerce #{pattern} into a Pattern" unless other.is_a? Pattern
  end

  def initialize(type, data)
    raise "Bad node type #{type}" unless NODE_TYPES.include?(type)

    @type = type
    @data = data

    @program = nil
  end
end

class Compiler
  class State
    attr_reader :first_pass_code

    def initialize
      @next_idx = 0
      # In the first pass instructions have the form [label, op, arg1, arg2] where label is nil or a symbolic label
      @first_pass_code = []
      @next_label = 0
    end

    # Add an instruction to the (first-pass) program we are building
    #
    # It will have the given opcode and arguments.
    #
    # If label is truthy we ensure that the new command has an associated label.
    # - If label is an actual label value, use it
    # - otherwise, if the new command already has one (from an earlier rule) we use it
    # - otherwise we generate a new one
    #
    # It is an error to pass in a label if the next instruction already has a different label
    #
    # If the new instruction has a label we return it. Otherwise we return nil.
    def add_instruction(op_code, arg1 = nil, arg2 = nil, label: false)
      next_instr = ensure_next_instr
      lbl = next_instr.first

      if label?(label)
        raise "Cannot specify label #{label} when next instruction already has label #{lbl}" if lbl && label != lbl

        lbl = label
      elsif label
        lbl ||= next_label
      end

      next_instr[0...] = [lbl, op_code, arg1, arg2]
      @next_idx += 1

      lbl
    end

    # Add the given label to the next instruction to be generated. Raise if there is already one
    #
    # If the argument is nil, create a new label if necessary.
    #
    # In any case, return the label that is used.
    def add_label_to_next(label)
      next_instr = ensure_next_instr
      raise "Cannot add label to next instruction if there is already one" if next_instr[0]

      label ||= get_next_label
      next_instr[0] = label

      label
    end

    def next_label
      res = [:label, @next_label]
      @next_label += 1
      res
    end

    private def label?(arg)
      Array(arg).first == :label
    end

    private def ensure_next_instr
      @first_pass_code[@next_idx] ||= []
    end
  end

  def initialize(pattern)
    @top_pattern = pattern.must_be_a Pattern

    @next_label = 0
    @first_element = true

    @compile_state = State.new
  end

  # 2-pass: first time through we generate an array of [label, op_code, arg1, arg2] tuples. label may well be nil.
  # On the second pass we resolve the (symbolic) labels to integers and then convert the references to relative offset
  def compile
    gen_first_pass_for(@top_pattern)

    # Attach an :end
    @compile_state.add_instruction(:end)

    link
  end

  private

  def check_args(pattern, expected_len = 2)
    raise "expected exactly #{expected_len} arguments" unless pattern.data.size == expected_len
  end

  def gen_first_pass_for(pattern)
    # brainless for now
    case pattern.type
    when :charset, :string, :any, :concat
      basic_construction(pattern)
    when :ordered_choice
      gen_ordered_choice(pattern)
    else
      raise "Don't know how to generate code for type #{pattern.type}"
    end
  end

  # As described in the Ierusamimschy paper, ordered choice p1+p2 generates this code:
  #
  #     Choice L1
  #     <p1>
  #     Commit L2
  # L1: <p2>
  # L2: ...
  #
  # TODO: optimization
  # There are optimizations available for multiple choices p1 + p2 + p3 + ..., based on the fact that + is left-associative in Lua
  # (and Ruby) while we get more efficient code if we analyze it as a right-associative combination.
  #
  # For now just do the straightforward thing
  def gen_ordered_choice(pattern)
    check_args(pattern)
    p1, p2 = pattern.data

    l1 = @compile_state.next_label
    l2 = @compile_state.next_label

    @compile_state.add_instruction(:choice, l1)
    gen_first_pass_for(p1)
    @compile_state.add_instruction(:commit, l2)
    @compile_state.add_label_to_next(l1)
    gen_first_pass_for(p2)
    @compile_state.add_label_to_next(l2)
  end

  def basic_construction(pattern)
    case pattern.type
    when :charset
      @compile_state.add_instruction(:charset, Set.new(pattern.data))
    when :string
      pattern.data.chars.each do |ch|
        @compile_state.add_instruction(:char, ch)
      end
    when :any
      @compile_state.add_instruction(:any, pattern.data)
    when :concat
      check_args(pattern)

      p1, p2 = pattern.data

      gen_first_pass_for(p1)
      gen_first_pass_for(p2)
    else
      raise "#{pattern.type} is not a simple construction"
    end
  end

  def link
    # First go through and generate line numbers for each label
    label_lines = {}

    phase1 = @compile_state.first_pass_code

    phase1.each_with_index do |command, line|
      label, = command
      next unless label

      raise "Label #{label} has already been seen" if label_lines[label]

      label_lines[label] = line
    end

    prog = []

    # Now pass through again and work out the offsets
    phase1.each_with_index do |command, line|
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

    prog << :end


    prog
  end
end

class ParsingMachine
  OP_CODES = %i[char charset any jump choice call return commit end fail].freeze
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
      return if done?

      if @current_instruction == :fail
        handle_fail_ptr
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
      when :choice
        # arg1 is the offset for the other side of the choice, which we push onto the stack
        @stack.push([@current_instruction + arg1, @current_subject_position, @capture_list.clone])
        @current_instruction += 1
      when :call
        # arg1 is an offset for the label to call.
        #
        # Call is like jump, but we push the return address onto the stack first
        @stack.push(@current_instruction + 1)
        @current_instruction += arg1
      when :return
        ret_address = @stack.pop
        raise 'Nothing on stack for return' unless ret_address
        raise 'Expected instruction pointer on stack for return' unless ret_address.is_a?(Integer)

        @current_instruction = ret_address
      when :commit
        # we pop and discard the to of the stack (which must be a triple) and then do the jump given by arg1
        _ = @stack.pop
        @current_instruction += arg1
      when :end
        @success = true
        @final_index = @current_subject_position
        done!
      else
        raise "Unsupported op code #{op_code}"
      end
    end
  end

  private

  def done!
    @done = true
  end

  def done?
    @done
  end

  # Not for the FAIL op_code, but for when the instruction pointer is :fail
  def handle_fail_ptr
    # special handling
    if @stack.empty?
      @success = false
      done!
    else
      top = @stack.pop

      if top.is_a?(Numeric)
      # nothing more to do
      else
        p, i, c = top
        @current_instruction = p
        @current_subject_position = i
        @capture_list = c
      end
    end
  end
end
