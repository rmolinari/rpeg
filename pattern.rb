# See the README file for a little context. And also:
#
#   http://www.inf.puc-rio.br/~roberto/lpeg/#func
#

require 'set'
require 'must_be'
require 'ostruct'

# This class is intended to play the same role as LPEG's lpeg module. I don't yet have any real understanding of how that code works
# so this code is liable to change a lot.
#
# Top-level differences from LPEG:
#
# - and patterns in LPEG are &patt but +patt here
#   - unary & apparently can't be overloaded in Ruby
#   - this pattern matches when patt appears at the current location, but it doesn't consume any of the input
class Pattern
  NODE_TYPES = %i[charset char string any concat ordered_choice repeat not and literal grammar open_call].freeze

  attr_reader :type, :left, :right, :program

  class << self
    # Match any character in string (regarded as a set of characters)
    def S(string)
      new(:charset, Set.new(string.chars))
    end

    # Take argument and turn it into a pattern
    #
    # TODO (from the lpeg homepage)
    #  - If the argument is a table [roughly a HashTable in Ruby], it is interpreted as a grammar
    #  - If the argument is a function, returns a pattern equivalent to a match-time capture over the empty string.
    def P(arg)
      case arg
      when Pattern
        arg
      when String
        # match that string exactly
        new(:string, arg)
      when Integer
        # When n >= 0, match at least n chars.
        # When n < 0, there must not be n or more characters left
        if arg >= 0
          new(:any, arg)
        else
          # "Does not match n characters"
          -new(:any, -arg)
        end
      when FalseClass, TrueClass
        new(:literal, arg)
      when Hash
        # See the Compiler for how we interpret this, which is a bit different from LPEG
        new(:grammar, arg)
      else
        raise "Pattern.P does not support argument #{arg}"
      end
    end

    # Given a 2-char string xy, the ASCII range x..y. Each argument gives a range and we match on their union
    def R(*ranges)
      raise 'No data given!' if ranges.empty?

      check = lambda do |str|
        raise "Bad data #{str} for Pattern#R" unless str.is_a?(String) && str.size == 2
        (str[0])..(str[1])
      end

      result = ranges.map{ check.call(_1) }.reduce { |memo, operand| charset_union(memo, operand) }

      Pattern.new(:charset, result)
    end

    # An "open call" reference to a rule in a grammar. As we don't have the grammar yet - it is available in full only when we are
    # ready to cmopile - we remember it this way.
    #
    # ref should be either
    #  - a non-negative integer n, referring to the n-th rule in grammar (0-based) or
    #  - a value that will be the key in the final grammar - a Hash - of the rule being referenced
    def V(ref)
      Pattern.new(:open_call, ref)
    end

    private def charset_union(cs1, cs2)
      if cs1.is_a?(Set)
        cs1.merge(cs2)
      elsif cs2.is_a?(Range)
        #both ranges
        if cs1.max < cs2.min || cs2.max < cs1.min
          # disjoint
          Set.new(cs1) + Set.new(cs2)
        else
          ([cs1.min, cs2.min].min)..([cs1.max, cs2.max].max)
        end
      else
        Set.new(cs1).merge(cs2)
      end
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

    # TODO: if self and other are both :char or :charset, combine the sets rather than form an :ordered_choice
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

  # Unary negation represents "does not match". So -patt says that there is no match at the current position and we don't consume
  # any of the string
  def -@
    Pattern.new(:not, self)
  end

  # Unary "and": pattern matches here (without consuming any input)
  #
  # Ierusalimschy points out that &patt can be implemented as --patt, but there is an optimization for the VM, so we preserve it
  def +@
    Pattern.new(:and, self)
  end

  # Difference is "this but not that". So p1 - p2 matches if p1 does and p2 doesn't
  #
  # Special case: if both patterns are charsets we replace with a single charset
  def -(other)
    check_type(other)

    if type == :charset && other.type == :charset
      new_cs = charset_difference(child, other.child)
      if new_cs.is_a?(Set) && new_cs.empty?
        # always fails
        Pattern.P(false)
      else
        Pattern.new(:charset, new_cs)
      end
    end

    # Otherwise we use -p2 * p1: p2 doesn't match here followed by p1 does match here
    -other * self
  end

  # If left is defined and right is nil - so we have a unary op - we can get child here
  def child
    raise 'Pattern is not unary' if right

    left
  end

  private def check_type(other)
    raise "Cannot coerce #{pattern} into a Pattern" unless other.is_a? Pattern
  end

  # Each is either a Set or a Range
  private def charset_difference(cs1, cs2)
    if cs1.is_a?(Set)
      cs1.subtract(cs2)
    elsif cs2.is_a?(Range)
      # They are both ranges. We can keep it that way so long as cs2 isn't in the middle of cs1.
      if cs1.min < cs2.min && cs2.max < cs1.max
        # Oh, hamburgers!
        Set.new(cs1).subtract(cs2)
      elsif cs2.min <= cs1.min && cs2.max >= cs1.max
        # Empty set!
        Set.new
        elseif cs2.min <= cs1.min
        (cs2.max)..(cs1.max)
      else
        (cs1.min)..(cs2.min)
      end
    else
      Set.new(cs1).subtract(cs2)
    end
  end

  def initialize(type, left, right = nil)
    raise "Bad node type #{type}" unless NODE_TYPES.include?(type)

    @type = type
    @left = left
    @right = right

    @program = nil
  end

  private def sanity_check
    case type
    when :charset
      right.must_be nil
      left.must_be_a(Set, Range)
    when :literal
      right.must_be nil
      left.must_be_in(true, false)
    when :grammar
      right.must_be nil
      left.must_be_a(Hash)
      left.must_not.empty?
    when :open_call
      right.must_be nil
      if left.is_a?(Integer)
        left.must_not.negative?
      end
    end
  end
end

class Compiler
  class State
    attr_reader :first_pass_code

    def initialize
      @next_idx = 0
      # In the first pass instructions have the form [label, op, arg1, arg2] where label is nil or a symbolic label
      @first_pass_code = []
      @next_label = 1
      @nonterminal_indices = []

      @labels = Set.new
    end

    # Add an instruction to the (first-pass) program we are building
    #
    # It will have the given opcode and arguments.
    #
    # If label is given we attach it to the new instruction
    def add_instruction(op_code, arg1 = nil, arg2 = nil, label: nil)
      next_instr = ensure_next_instr
      labels_for_instr = next_instr.first

      if label
        labels_for_instr << label
        remember_label(label)
      end

      next_instr[0...] = [labels_for_instr, op_code, arg1, arg2]
      @next_idx += 1
    end

    # Add the given label to the next instruction to be generated.
    def add_label_to_next(label)
      next_instr = ensure_next_instr
      label.must_be
      next_instr[0] << label
      remember_label(label)
    end

    def next_label
      res = [:label, @next_label]
      @next_label += 1
      remember_label(res)
      res
    end

    def label?(arg)
      @labels.include?(arg)
    end

    def register_nonterminal(nonterminal, index)
      @nonterminal_indices[index] = nonterminal
    end

    def index_of_nonterminal(nonterminal)
      @nonterminal_indices.index(nonterminal)
    end

    def nonterminal_with_index(index)
      raise "Cannot look nonterminal with negative index" if index < 0
      @nonterminal_indices[index]
    end

    private def ensure_next_instr
      # the first element will contain the set (possibly empty) of labels for this line
      @first_pass_code[@next_idx] ||= [Set.new]
    end

    private def remember_label(label)
      # For technical reasons related to the linker a label can't have a non-negative integer value
      raise "Illegal label #{label}" if label.is_a?(Integer) && label >= 0

      @labels << label
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
    when :charset, :string, :any, :concat, :literal, :open_call
      basic_construction(pattern)
    when :ordered_choice
      gen_ordered_choice(pattern)
    when :repeat
      # repeat 0 or more times
      gen_repeat(pattern)
    when :not
      # the child pattern doesn't match here; consume no input
      gen_not(pattern)
    when :and
      # the child pattern doesn't match here, but consume no input
      gen_and(pattern)
    when :grammar
      gen_grammar(pattern)
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

    # Special, quicker handling when the thing we are repeated over is a charset. See Ierusalimschy 4.3
    if pattern.child.type == :charset
      @compile_state.add_instruction(:span, pattern.child.child)
    else
      gen_first_pass_for(pattern.child)
    end

    @compile_state.add_instruction(:partial_commit, l1)
    @compile_state.add_label_to_next(l2)
  end

  # See Ierusalimschy section 4.4
  #
  #     Choice L1
  #     <p>
  #     FailTwice
  # L1: ...
  def gen_not(pattern)
    l1 = @compile_state.next_label # get a label

    @compile_state.add_instruction(:choice, l1)
    gen_first_pass_for(pattern.child)
    @compile_state.add_instruction(:fail_twice)
    @compile_state.add_label_to_next(l1)
  end

  # See Ierusalimschy section 4.4
  #
  #     Choice L1
  #     <p>
  #     BackCommit L2
  # L1: Fail
  # L2: ...
  def gen_and(pattern)
    l1 = @compile_state.next_label
    l2 = @compile_state.next_label

    @compile_state.add_instruction(:choice, l1)
    gen_first_pass_for(pattern.child)
    @compile_state.add_instruction(:back_commit, l2)
    @compile_state.add_instruction(:fail, label: l1)
    @compile_state.add_label_to_next(l2)
  end

  # We have hash table, which we interpret as a grammar. Lua has a strong sense of the "first" element of a table (roughly like a
  # HashTable). It turns out we do, too (see the "Entry Order" documentation for the Hash class)
  def gen_grammar(pattern)
    grammar = pattern.child

    start_symbol, _ = grammar.first

    l2 = @compile_state.next_label

    @compile_state.add_instruction(:call, start_symbol)
    @compile_state.add_instruction(:jump, l2)

    grammar.each_with_index do |pair, idx|
      nonterminal, rule_pattern = pair
      raise "Grammar lists nonterminal #{nonterminal} twice" if @compile_state.index_of_nonterminal(nonterminal)

      @compile_state.register_nonterminal(nonterminal, idx)
      @compile_state.add_label_to_next(nonterminal)

      gen_first_pass_for(rule_pattern)
      @compile_state.add_instruction(:return)
    end

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
    when :literal
      if pattern.data
        # we always succeed, so nothing to do
      else
        # we always fail
        @compile_state.add_instruction(:fail)
      end
    when :open_call
      # This will be resolved in the second pass
      @compile_state.add_instruction(:open_call, pattern.child)
    else
      raise "#{pattern.type} is not a simple construction"
    end
  end

  def link
    # First go through and generate line numbers for each label.
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

      if op == :open_call
        # Special handling when we compile for a grammar
        #
        # The call we need to make might be represented by a non-negative integer n, indicating the n-th rule in the grammar, or
        # something else, which is the "name" of the nonterminal and which is (or should be) a symbolic label in our program.
          open_arg = args.first
        if open_arg.is_a?(Integer)
          symb_arg = @compile_state.terminal_with_index(open_arg)

          raise "Nonterminal reference #{open_arg} not found" unless symb_arg
          args[0] = symb_arg
        end

        # we also need to replace the op code with a regular :call
        op = :call
      end

      args = args.map do |arg|
        if @compile_state.label?(arg)
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
end

class ParsingMachine
  OP_CODES = %i[char charset any jump choice call return commit back_commit partial_commit span end fail fail_twice].freeze
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
        if @current_subject_position + arg1 <= @subject.size
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
        push(:instruction, 1)
        @current_instruction += arg1
      when :return
        @current_instruction = pop(:instruction)
      when :commit
        # we pop and discard the top of the stack (which must be a triple) and then do the jump given by arg1. Even though we are
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
      when :back_commit
        # A combination of a fail and a commit. We backtrack, but then jump to the specified instruction rather than using the
        # backtrack label. It's used for the :and pattern. See Ierusalimschy, 4.4
        _, subject_pos, captures = pop(:state)
        @current_subject_position = subject_pos
        @capture_list = captures
        @current_instruction += arg1
      when :span
        # Special instruction for when we are repeating over a charset, which is common
        if arg1.include?(@subject[@current_subject_position])
          @current_subject_position += 1
        else
          @current_instruction += 1
        end
      when :fail
        # We trigger the fail routine
        @current_instruction = :fail
      when :fail_twice
        # An optimization for the not(pattern) implementation. We pop the top of the stack and discard it, and then enter the fail
        # routine again. For sanity's sake we'll check that the thing we are popping is a :state entry. See Ierusalimschy, 4.4
        _ = pop(:state)
        @current_instruction = :fail
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
