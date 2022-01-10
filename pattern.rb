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
# - and patterns in LPEG are #patt (&patt in the first version) but +patt here
#   - unary & apparently can't be overloaded in Ruby
#   - this pattern matches when patt appears at the current location, but it doesn't consume any of the input
# - repeating patterns still use exponentiation, but it now looks like patt**n rather than patt^n because of Ruby's syntax
#   - so patt**n means
#     - "n or more occurrences of patt" when n is non-negative
#     - "fewer than -n occurrences of patt" when n is negative
class Pattern
  NODE_TYPES = %i[charset string any concat ordered_choice repeat not and literal grammar open_call].freeze

  attr_reader :type, :left, :right

  class << self
    # Match any character in string (regarded as a set of characters)
    def S(string)
      return P(false) if string.empty?

      new(:charset, Set.new(string.chars))
    end

    # Take argument and turn it into a pattern
    #
    # TODO (from the lpeg homepage)
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
      return P(false) if ranges.empty?

      # raise 'No data given!' if ranges.empty?

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

    def match(thing, string)
      P(thing).match(string)
    end

    private def charset_union(cs1, cs2)
      if cs1.is_a?(Set)
        cs1.merge(cs2)
      elsif cs2.is_a?(Range)
        # both ranges
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
    #@program ||= Compiler.new(self).compile

    machine = ParsingMachine.new(program + [:end], str)
    machine.run

    return machine.final_index if machine.success?

    # otherwise return nil
  end

  # If left is defined and right is nil - so we have a unary op - we can get child here
  def child
    raise 'Pattern is not unary' if right

    left
  end

  # sometimes it is more natural to call it the data
  alias data child

  ########################################
  # Operator overloading
  #
  # The LPEG library makes heavy use of operator overriding in Lua to combine patterns in a convenient way. We will follow.

  # This only happens if other is a Numeric type, which is annoying
  def coerce(other)
    [Pattern.P(other), self]
  end

  # p1 * p2 means p1 followed by p2
  def *(other)
    other = fix_type(other)

    # true is the identity for *
    if other.type == :literal && other.child == true
      return self
    elsif type == :literal && child == true
      return other
    end

    Pattern.new(:concat, self, other)
  end

  # p1 + p2 is ordered choice: if p1 matches we match, otherwise try matching on p2
  def +(other)
    other = fix_type(other)

    if type == :charset && other.type == :charset
      # Take the union of the charsets
      Pattern.new(:charset, charset_union(data, other.data))
    elsif type == :ordered_choice
      # rejigger to make this operation right-associative which makes for more efficient compiled code. See Ierusalimschy 4.2
      left + (right + other)
    elsif other.type == :literal && other.child == false
      # false is the right-identity for +
      self
    else
      Pattern.new(:ordered_choice, self, other)
    end
  end

  # pat ** n means "n or more occurrences of def"
  def **(other)
    n = other

    if n >= 0
      # So, we represent this by a sequence of num occurrences, followed by a zero-or-more
      patt = Pattern.new(:repeat, self) # this repeats 0 or more times
      while n.positive?
        patt = self * patt
        n -= 1
      end
    else
      n = -n
      patt = Pattern.P(true)
      while n.positive?
        patt = self * patt + true
        n -= 1
      end
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
    other = fix_type(other)

    if type == :charset && other.type == :charset
      new_cs = charset_difference(child, other.child)
      return Pattern.P(false) if new_cs.is_a?(Set) && new_cs.empty?

      return Pattern.new(:charset, new_cs)
    end

    # Otherwise we use -p2 * p1: p2 doesn't match here followed by p1 does match here
    -other * self
  end

  private def fix_type(other)
    return other if other.is_a?(Pattern)

    Pattern.P(other) # see what we can do
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
      elsif cs2.min <= cs1.min
        (cs2.max)...(cs1.max)
      else
        (cs1.min)...(cs2.min)
      end
    else
      Set.new(cs1).subtract(cs2)
    end
  end

  private def charset_union(cs1, cs2)
    self.class.send(:charset_union, cs1, cs2)
  end

  ########################################
  # Code generation

  def program
    return @program if @program

    prog = []
    case type
    when :charset
      prog << [:charset, Set.new(data)]
    when :string
      data.chars.each do |ch|
        prog << [:char, ch]
      end
    when :any
      prog << [:any, data]
    when :concat
      # Just concatenate the code
      prog = left.program + right.program
    when :literal
      # if data = true then we always succeed, which means we don't have to do anything at all
      prog << [:fail] unless data
    when :open_call
      # This occurs only in a grammar and will be closed in the link phase
      prog << [:open_call, child]
    when :ordered_choice
      p1 = left.program
      p2 = right.program

      prog << [:choice, 2 + p1.size]
      prog += p1
      prog << [:commit, 1 + p2.size]
      prog += p2
    when :repeat
      p = child.program

      if child.type == :charset
        # Special, quicker handling when the thing we are repeated over is a charset. See Ierusalimschy 4.3
        prog << [:span, child.data]
      else
        prog << [:choice, 2 + p.size]
        prog += p
        prog << [:partial_commit, -p.size]
      end
    when :not
      p = child.program

      prog << [:choice, 2 + p.size]
      prog += p
      prog << [:fail_twice]
    when :and
      p = child.program

      prog << [:choice, 2 + p.size]
      prog += p
      prog << [:back_commit, 2]
      prog << [:fail]
    when :grammar
      nonterminal_indices = {}
      nonterminal_by_index = []
      start_line_of_nonterminal = {}
      pattern_for_nonterminal = {}

      full_rule_code = []

      child.each_with_index do |rule, idx|
        nonterminal, rule_pattern = rule
        raise "Nonterminal #{nonterminal} appears twice in grammar" if nonterminal_indices[nonterminal]

        nonterminal_indices[nonterminal] = idx
        nonterminal_by_index[idx] = nonterminal
        start_line_of_nonterminal[nonterminal] = 2 + full_rule_code.size
        full_rule_code += rule_pattern.program + [[:return]]
        pattern_for_nonterminal[nonterminal] = rule_pattern
      end

      prog << [:call, 2] # call the first nonterminal
      prog << [:jump, 1 + full_rule_code.size] # we are done: jump to the line after the grammar's program
      prog += full_rule_code

      # Now close the :open_call instructions
      prog.each_with_index do |instr, idx|
        op, arg1, = instr
        next unless op == :open_call

        # arg1 is the nonterminal, or a numeric index giving the rule number
        if arg1.is_a?(Integer) && arg1 >= 0
          symbol = nonterminal_by_index[arg1]
          raise "Grammar does not have entry for index #{arg1}" unless symbol

          arg1 = symbol
        end
        start_line = start_line_of_nonterminal[arg1]
        raise "Nonterminal #{arg1} does not have a rule in grammar" unless start_line

        offset = start_line - idx

        # The usual action to replace the open call with a :call. But, if the following instruction is a :return this a tail call
        # and we can eliminate the stack push by using a :jump instead of the call. This leaves the following :return a dead
        # statement which we will never reach. We change it to a bogus op code as a sanity check: if the VM ever reaches it we have
        # made an error somewhere.
        if prog[idx + 1] && prog[idx + 1].first == :return
          prog[idx] = [:jump, offset]
          prog[idx + 1] = [:unreachable]
        else
          prog[idx] = [:call, offset]
        end
      end
    else
      raise "Unknown pattern type #{type}"
    end

    # @program = cleaned_up.call(prog).map(&:freeze).freeze
    @program = prog.map(&:freeze).freeze
  end

  ########################################
  # Misc

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
      left.must_not.negative? if left.is_a?(Integer)
    end
  end

  ########################################
  # Experimental monkeypatching
  #
  # Very annoyingly, Ruby's #coerce mechanism is only used by the Numeric types. This means things like "a" + Pattern.P(true) won't
  # work properly. The only way I can think to make it work - as it does in LPEG - is to monkeypatch String, TrueClass, FalseClass,
  # etc.

  # Trying the technique from https://stackoverflow.com/a/61438012/1299011
  module NonNumericOverloadExtension
    [:+, :*, :-].each do |sym|
      define_method sym do |other|
        return Pattern.P(self).send(sym, other) if other.is_a?(Pattern)

        super(other)
      end
    end
  end

  [::String, ::TrueClass, ::FalseClass, ::Hash].each do |klass|
    klass.class_eval do
      prepend NonNumericOverloadExtension
    end
  end
end

module Analysis
  extend self

  CHECK_PREDICATES = %i[nullable nofail].freeze

  # The is lpeg's checkaux from lpcode.c. Comment from that function (reformatted):
  #
  # /*
  # ** Checks how a pattern behaves regarding the empty string, in one of two different ways:
  #
  # ** - A pattern is *nullable* if it can match without consuming any character;
  # ** - A pattern is *nofail* if it never fails for any string (including the empty string).
  #
  # ** The difference is only for predicates and run-time captures; for other patterns, the two properties are equivalent.  (With
  # ** predicates, &'a' is nullable but not nofail. Of course, nofail => nullable.)
  #
  # ** These functions are all convervative in the following way:
  # **    p is nullable => nullable(p)
  # **    nofail(p) => p cannot fail
  #
  # ** The function assumes that TOpenCall is not nullable; this will be checked again when the grammar is fixed.  Run-time captures
  # ** can do whatever they want, so the result is conservative.
  # */
  #
  # TODO:
  #  - implement for our equivalent of TRep, TRunTime, TCaputre, etc. when we implement them
  def check_pred(pattern, pred)
    raise "Bad check predicate #{pred}" unless CHECK_PREDICATES.include?(pred)

    case pattern.type
    when :char, :charset, :any, :open_call
      # Not nullable; for open_call this is a blind assumption
      false
    when :literal
      # false is not nullable, true is nofail
      pattern.data
    when :not
      # can match empty, but can fail
      !(pred == :nofail)
    when :not
      # can match empty; can fail exactly when body can
      if pred == :nullable
        true
      else
        check_pred(pattern.child)
      end
    when :concat
      return false unless check_pred(pattern.left, pred)

      check_pred(pattern.right, pred)
    when :ordered_choice
      return true if check_pred(patter.left, pred)

      check_pred(pattern.right, pred)
    when :call
      check_pred(pattern.child, pred)
    else
      raise "Bad pattern type #{pattern.type}"
    end
  end

  def nullable?(pattern)
    check_pred(pattern, :nullable)
  end

  def nofail?(pattern)
    check_pred(pattern, :nofail)
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
        # Special instruction for when we are repeating over a charset, which is common. We just consume as many maching characters
        # as there are. This never fails as we might just match zero
        @current_subject_position += 1 while arg1.include?(@subject[@current_subject_position])

        @current_instruction += 1
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

  private def done!
    @done = true
  end

  private def done?
    @done
  end

  # Not for the FAIL op_code, but for when the instruction pointer is :fail
  private def handle_fail_ptr
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
  private def push(type, offset)
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
  private def pop(expecting = nil)
    raise "Nothing in stack to pop" if @stack.empty?

    tag, val = @stack.pop

    raise "Top of stack is of type #{tag}, not of expected type #{expecting}" if expecting && expecting != tag

    val
  end

  # Peek and return the top of the stack, or nil if the stack is empty. The returned value, if an array, can be modified, thus
  # affecting the stack
  #
  # If expecting is given make sure that the top of the stack is of the given type
  private def peek(expecting = nil)
    return nil if @stack.empty?

    tag, val = @stack.last

    raise "Top of stack is of type #{tag}, not of expected type #{expecting}" if expecting && expecting != tag

    val
  end
end
