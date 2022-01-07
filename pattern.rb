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
  NODE_TYPES = %i[charset char string any concat ordered_choice repeat].freeze

  attr_reader :type, :left, :right, :program

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
      else
        raise "Pattern.P does not support argument #{arg}"
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
    Pattern.new(:concat, self, other)
  end

  # p1 + p2 is ordered choice: if p1 matches we match, otherwise try matching on p2
  def +(other)
    check_type(other)

    # TODO: if self and other are both :charset, combine the sets rather than form an :ordered_choice
    Pattern.new(:ordered_choice, self, other)
  end

  # pat ** n means "n or more occurrences of def"
  #
  # TODO:
  # - negative values: -n means at most n occurrences
  def **(num)
    raise "Power (repetition) currently supported only for non-negative integers" unless num.is_a?(Integer) && !num.negative?

    # So, we represent this by a sequence of num occurrences, followed by a zero-or-more

    patt = Pattern.new(:repeat, self) # this repeats 0 or more times
    while num > 0
      patt = self * patt
      num -= 1
    end
    patt
  end

  # If left is defined and right is nil - so we have a unary op - we can get child here
  def child
    raise 'Pattern is not unary' if right

    left
  end

  private def check_type(other)
    raise "Cannot coerce #{pattern} into a Pattern" unless other.is_a? Pattern
  end

  def initialize(type, left, right = nil)
    raise "Bad node type #{type}" unless NODE_TYPES.include?(type)

    @type = type
    @left = left
    @right = right

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
    # - If label is an actual label value, use it, adding it to any already there
    # - otherwise, if the new command already has one (from an earlier rule) we use it
    # - otherwise we generate a new one
    def add_instruction(op_code, arg1 = nil, arg2 = nil, label: false)
      next_instr = ensure_next_instr
      labels_for_instr = next_instr.first

      if label?(label)
        labels_for_instr << label
      elsif label
        labels_for_instr << next_label
      end

      next_instr[0...] = [labels_for_instr, op_code, arg1, arg2]
      @next_idx += 1
    end

    # Add the given label to the next instruction to be generated.
    #
    # If the argument is nil, create a new label.
    def add_label_to_next(label)
      next_instr = ensure_next_instr

      label ||= get_next_label
      next_instr[0] << label
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
      # the first element will contain the set (possibly empty) of labels for this line
      @first_pass_code[@next_idx] ||= [Set.new]
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

  def gen_first_pass_for(pattern)
    # brainless for now
    case pattern.type
    when :charset, :string, :any, :concat
      basic_construction(pattern)
    when :ordered_choice
      gen_ordered_choice(pattern)
    when :repeat
      # repeat 0 or more times
      gen_repeat(pattern)
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
    p1 = pattern.left.must_be
    p2 = pattern.right.must_be

    l1 = @compile_state.next_label
    l2 = @compile_state.next_label

    @compile_state.add_instruction(:choice, l1)
    gen_first_pass_for(p1)
    @compile_state.add_instruction(:commit, l2)
    @compile_state.add_label_to_next(l1)
    gen_first_pass_for(p2)
    @compile_state.add_label_to_next(l2)
  end

  # See Ierusalimschy, section 4.3
  def gen_repeat(pattern)
    l1 = @compile_state.next_label
    l2 = @compile_state.next_label

    @compile_state.add_instruction(:choice, l2)
    @compile_state.add_label_to_next(l1)
    gen_first_pass_for(pattern.child)
    @compile_state.add_instruction(:partial_commit, l1)
    @compile_state.add_label_to_next(l2)
  end

  def basic_construction(pattern)
    case pattern.type
    when :charset
      @compile_state.add_instruction(:charset, Set.new(pattern.child))
    when :string
      pattern.child.chars.each do |ch|
        @compile_state.add_instruction(:char, ch)
      end
    when :any
      @compile_state.add_instruction(:any, pattern.child)
    when :concat
      p1 = pattern.left.must_be
      p2 = pattern.right.must_be

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
      labels, = command
      next if labels.empty?

      labels.each do |label|
        raise "Label #{label} has already been used" if label_lines[label]
        label_lines[label] = line
      end
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
  OP_CODES = %i[char charset any jump choice call return commit partial_commit end fail].freeze
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
        push(:state, arg1)
        @current_instruction += 1
      when :call
        # arg1 is an offset for the label to call.
        #
        # Call is like jump, but we push the return address onto the stack first
        push(:offset, 1)
        @current_instruction += arg1
      when :return
        @current_instruction = pop(:instruction)
      when :commit
        # we pop and discard the to of the stack (which must be a triple) and then do the jump given by arg1. Even though we are
        # discarding it check that it was a full state as a sanity check.
        _ = pop(:state)
        @current_instruction += arg1
      when :partial_commit
        # Sort of a combination of commit (which pops) and choice (which pushes), but we just tweak the top of the stack. See
        # Ierusalimschy, sec 4.3
        stack_top = peek(:state)
        raise "Empty stack for partial commit!" unless stack_top

        stack_top[1] = @current_subject_position
        stack_top[2] = @capture_list.clone

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
      top = pop

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

  ########################################
  # Stack manipulation

  # We push either
  # - an instruction pointer, which may later be used to jump, etc, or
  # - the current state with an offset, which is the [instr ptr + offset, subject_pos, capture list] triple. Let's tag them for sanity's sake
  def push(type, offset)
    raise "must push something" unless offset

    case type
    when :instruction
      @stack.push([:instruction, @current_instruction + offset])
    when :state
      @stack.push([:state, [@current_instruction + offset, @current_subject_position, @capture_list.clone]])
    else
      raise "Bad push type #{type}"
    end
  end

  # Pop the top thing on the stack and return it (not including the tag). If expecting is non nil check that it equals the tag
  #
  # Raise if stack is empty
  def pop(expecting = nil)
    raise "Nothing in stack to pop" if @stack.empty?

    tag, val = @stack.pop

    raise "Top of stack is of type #{tag}, not of expected type #{expecting}" if expecting && expecting != tag

    val
  end

  # Peek and return the top of the stack, or nil if the stack is empty. The returned value, if an array, can be modified, thus
  # affecting the stack
  #
  # If expecting is given make sure that the top of the stack is of the given type
  def peek(expecting = nil)
    return nil if @stack.empty?

    tag, val = @stack.last

    raise "Top of stack is of type #{tag}, not of expected type #{expecting}" if expecting && expecting != tag

    val
  end
end
