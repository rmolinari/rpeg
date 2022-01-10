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
  NODE_TYPES = %i[charset string any seq ordered_choice repeat not and literal grammar open_call rule call].freeze

  attr_reader :type, :left, :right, :extra

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
        # match that string exactly. We always match the empty strin
        if arg.empty?
          P(true)
        else
          new(:string, arg)
        end
      when Integer
        # When n >= 0, match at least n chars.
        # When n < 0, there must not be n or more characters left
        if arg.zero?
          P(true)
        elsif arg.positive?
          new(:any, arg)
        else
          # "Does not match n characters"
          -new(:any, -arg)
        end
      when FalseClass, TrueClass
        new(:literal, arg)
      when Hash
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
    #    - strings are turned into symbox
    def V(ref)
      ref = ref.to_sym if ref.is_a?(String)
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
    machine = ParsingMachine.new(program + [Instruction.new(:end)], str)
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

    Pattern.new(:seq, self, other)
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
      raise "Pattern may match 0-length string so repetition may lead to an infinite loop" if nullable?

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

  def to_s
    return @to_s if @to_s

    result = []
    do_sub_pattern = lambda do |sub_patt|
      sub_patt.to_s.split("\n").each do |line|
        result << "  #{line}"
      end
    end

    type_s = type.to_s.capitalize

    # We don't use Pattern.visit because the order is wrong
    case type
    when :charset
      result << "#{type_s}: #{data.join}"
    when :string, :any
      result << "String: #{data}"
    when :literal
      result << "Literal: #{data.inspect}"
    when :open_call
      result << "OpenCall: #{data}"
    when :call
      result << "Call: #{extra}"
    when :seq, :ordered_choice
      result << (type == :seq ? "Seq:" : "Ordered Choice:")
      do_sub_pattern.call(left)
      do_sub_pattern.call(right)
    when :repeat, :not, :and
      result << type_s
      do_sub_pattern.call(child)
    when :grammar
      result << "Grammar:"
      first = true
      data.each do |nonterminal, rule_pattern|
        prefix = "  #{nonterminal}: "
        first = true
        rule_pattern.to_s.split("\n").each do |line|
          line_prefix = first ? prefix : (" " * prefix.len)
          result << "#{line_prefix}#{line}"
        end
      end
    else
      raise "Unhandled type for to_s: #{type}"
    end

    @to_s = result.join("\n")
  end

  ########################################
  # Pattern properties

  def nullable?
    return @nullable if defined? @nullable

    @nullable = Analysis.nullable?(self)
  end

  def nofail?
    return @nofail if defined? @nofail

    @nofail = Analysis.nofail?(self)
  end

  def num_children
    case type
    when :charset, :string, :any, :literal, :open_call
      0
    when :repeat, :and, :not, :call, :rule
      1
    when :seq, :ordered_choice
      2
    when :grammar
      raise "#num_children isn't meaningful for :grammar nodes"
    end
  end

  ########################################
  # Code generation

  def program
    return @program if @program

    prog = []
    case type
    when :charset
      prog << Instruction.new(:charset, Set.new(data))
    when :string
      data.chars.each do |ch|
        prog << Instruction.new(:char, ch)
      end
    when :any
      prog << Instruction.new(:any, data)
    when :seq

      # Just concatenate the code
      prog = left.program + right.program
    when :literal
      # if data = true then we always succeed, which means we don't have to do anything at all
      prog << Instruction.new(:fail) unless data
    when :open_call
      # we resolved these to :call when the grammar node was created. So if we see one now it is because it was not contained in a
      # grammar.
      raise ':open_call node appears outside of a grammar'
    when :call
      prog << Instruction.new(:call, extra)
    when :ordered_choice
      p1 = left.program
      p2 = right.program

      prog << Instruction.new(:choice, 2 + p1.size)
      prog += p1
      prog << Instruction.new(:commit, 1 + p2.size)
      prog += p2
    when :repeat
      p = child.program

      if child.type == :charset
        # Special, quicker handling when the thing we are repeated over is a charset. See Ierusalimschy 4.3
        prog << Instruction.new(:span, child.data)
      else
        prog << Instruction.new(:choice, 2 + p.size)
        prog += p
        prog << Instruction.new(:partial_commit, -p.size)
      end
    when :not
      p = child.program

      prog << Instruction.new(:choice, 2 + p.size)
      prog += p
      prog << Instruction.new(:fail_twice)
    when :and
      p = child.program

      prog << Instruction.new(:choice, 2 + p.size)
      prog += p
      prog << Instruction.new(:back_commit, 2)
      prog << Instruction.new(:fail)
    when :grammar
      start_line_of_nonterminal = {}
      full_rule_code = []

      data.each_with_index do |rule, idx|
        nonterminal = rule.extra
        rule_pattern = rule.left
        start_line_of_nonterminal[nonterminal] = 2 + full_rule_code.size
        full_rule_code += rule_pattern.program + [Instruction.new(:return)]
      end

      prog << Instruction.new(:call, @nonterminal_by_index[0]) # call the first nonterminal
      prog << Instruction.new(:jump, 1 + full_rule_code.size) # we are done: jump to the line after the grammar's program
      prog += full_rule_code

      # Now close the :call instructions. THe :open_call nodes were analyzed when the :grammar node was created but we still need to
      # calculate line offsets
      prog.each_with_index do |instr, idx|
        next unless instr.op_code == :call

        nonterminal = instr.arg1

        # arg1 is the nonterminal
        start_line = start_line_of_nonterminal[nonterminal]
        raise "Nonterminal #{nonterminal} does not have a rule in grammar" unless start_line

        offset = start_line - idx

        # We replaced :open_call with :call. But, if the following instruction is a :return this a tail call and we can eliminate
        # the stack push by using a :jump instead of the call. This leaves the following :return a dead statement which we will
        # never reach. We change it to a bogus op code as a sanity check: if the VM ever reaches it we have made an error somewhere.
        if prog[idx + 1] && prog[idx + 1].op_code == :return
          prog[idx] = Instruction.new(:jump, offset)
          prog[idx + 1] = Instruction.new(:unreachable)
        else
          prog[idx] = Instruction.new(:call, offset)
        end
      end
    else
      raise "Unknown pattern type #{type}"
    end

    @program = prog.map(&:freeze).freeze
  end

  ########################################
  # Misc

  def initialize(type, left, right = nil, extra: nil)
    raise "Bad node type #{type}" unless NODE_TYPES.include?(type)

    @type = type
    @left = left
    @right = right

    # :call uses this for the nonterminal
    @extra = extra

    sanity_check

    if type == :grammar
      fix_up_grammar

      Analysis.verify_grammar(self)
    end

    @program = nil
  end

  # Special operation when closing open calls
  def convert_open_call_to_call!(rule, ref)
    raise "Cannot convert pattern to :call" unless type == :open_call
    raise "Must give rule and nonterminal symbol to :call pattern" unless rule && ref
    raise "Rule for :call pattern must be a rule, got #{rule.type}" unless rule.type == :rule

    @type = :call
    @left = rule
    @extra = ref

    # We must check these again when needed rather than use the memoized values
    [:@nullable, :@nofail].each do |ivar|
      remove_instance_variable(ivar) if instance_variable_defined?(ivar)
    end
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
    when :call
      left_must_be
      right.must_be nil
      extra.must_be
    when :rule
      left.must_be_a Pattern
      right.must_be nil
      extra.must_be
    end
  end

  # We do several things
  # - make sure each rule pattern is actually a pattern.
  #   - since we can specify rules as strings, say, or subgrammars (as hash) we need to step in here
  # - the hash table of rules is replaced with a list of [nonterminal, pattern] pairs
  # - :opencall(v) patterns are replaced with :call(rule) patterns
  # - convert the hashtable of rules to a same-order list of :rule nodes.
  #
  # We set up
  #  @nonterminal_indices: map nonterminal symbols to their index (0, 1, ...)
  #  @nonterminal_by_index: may indices to the corresopnding nonterminal
  private def fix_up_grammar
    raise "Bad type for #fix_up_grammar" unless type == :grammar

    @nonterminal_indices = {}
    @nonterminal_by_index = []

    grammar_hash = child.transform_values!{ Pattern.P(_1) }
    grammar_hash.transform_keys! { |key| key.is_a?(String) ? key.to_sym : key }

    rule_hash = {}
    rule_list = []

    grammar_hash.each_with_index do |rule, idx|
      nonterminal, rule_pattern = rule
      raise "Nonterminal #{nonterminal} appears twice in grammar" if @nonterminal_indices[nonterminal]

      rule = Pattern.new(:rule, rule_pattern, extra: nonterminal)

      rule_list << rule
      rule_hash[nonterminal] = rule

      @nonterminal_indices[nonterminal] = idx
      @nonterminal_by_index[idx] = nonterminal
    end

    @left = rule_list

    # TODO: redo this. I don't like the idea of using a vistor to find the open_call nodes while we are modifiying in-place. It
    # works but feels fragile.
    Pattern.visit(self) do |node|
      next unless node.type == :open_call

      ref = node.data
      if ref.is_a?(Integer) && ref >= 0
        symb_ref = @nonterminal_by_index[ref]
        raise "bad grammar index for rule '#{ref}'" unless symb_ref

        ref = symb_ref
      end
      raise "bad grammar reference for rule '#{ref}'" unless @nonterminal_indices[ref]

      rule = rule_hash[ref].must_be
      node.convert_open_call_to_call!(rule, ref)
    end
  end

  ########################################
  # Visiting the nodes in a pattern tree

  # Yield each node in the tree to caller. We visit breadth-first
  def Pattern.visit(pattern)
    seen = Set.new
    to_do = [pattern]
    until to_do.empty?
      node = to_do.shift
      next if seen.include? node

      seen << node

      yield node

      case node.type
      when :charset, :string, :any, :literal, :call, :open_call
      # nothing more to do
      when :repeat, :not, :and, :rule
        to_do << node.child
      when :ordered_choice, :seq
        to_do << node.left
        to_do << node.right
      when :grammar
        node.data.each do |rule|
          rule.type.must_be :rule
          to_do << rule.child
        end
      else
        raise "Unhandled pattern type #{node.type}"
      end
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

  # These two are cached in pattern.nullable? and pattern.nofail?
  def nullable?(pattern)
    check_pred(Pattern.P(pattern), :nullable)
  end

  def nofail?(pattern)
    check_pred(Pattern.P(pattern), :nofail)
  end

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

    # loop to eliminate some tail calls, as in the LPEG code. I don't think it's really necessary - as my implementation is not
    # going to be fast overall - but let's try a new technique.
    loop do
      case pattern.type
      when :string, :charset, :any, :open_call
        # Not nullable; for open_call this is a blind assumption
        return false
      when :literal
        # false is not nullable, true is nofail
        return pattern.data
      when :repeat
        return true # we never fail, as we can match zero occurrences
      when :not
        # can match empty, but can fail
        return (pred != :nofail)
      when :and
        # can match empty; can fail exactly when body can
        return true if pred == :nullable

        pattern = pattern.child.must_be
      when :seq
        return false unless check_pred(pattern.left, pred)

        pattern = pattern.right.must_be
      when :ordered_choice
        return true if check_pred(pattern.left, pred)

        pattern = pattern.right.must_be
      when :grammar
        # Strings are matched by the initial nonterminal
        first_rule = pattern.child.first
        first_rule.type.must_be :rule
        pattern = first_rule.child.must_be
      when :call
        # The call's rule is in child
        pattern = pattern.child.must_be
      when :rule
        # Rule's pattern is in child
        pattern = pattern.child.must_be
      else
        raise "Bad pattern type #{pattern.type}"
      end
    end
  end

  def verify_grammar(grammar)
    raise "Not a grammar!" unless grammar.type == :grammar

    # /* check infinite loops inside rules */
    grammar.data.each do |rule|
      verify_rule(rule)
      raise "Grammar has potential infinite loop in rule '#{rule.extra}'" if loops?(rule)
    end
  end

  # Sanity checks from the LPEG sources. We check if a rule can be left-recursive, i.e., whether we can return to the rule without
  # consuming any input. The plan is to walk the tree into subtrees, possibily repetitively, whenever we see we can do so without
  # consuming any input.
  #
  # LPEG comment follows. Note that we check for nullability directly for sanity's sake.
  #
  # /*
  # ** Check whether a rule can be left recursive; raise an error in that
  # ** case; otherwise return 1 iff pattern is nullable.
  # ** The return value is used to check sequences, where the second pattern
  # ** is only relevant if the first is nullable.
  # ** Parameter 'nb' works as an accumulator, to allow tail calls in
  # ** choices. ('nb' true makes function returns true.)
  # ** Parameter 'passed' is a list of already visited rules, 'npassed'
  # ** counts the elements in 'passed'.
  # ** Assume ktable at the top of the stack.
  # */
  def verify_rule(rule)
    rules_seen = []

    local_rec = lambda do |pattern, num_rules_seen|
      case pattern.type
      when :string, :charset, :any, :literal
        # no op
      when :not, :and, :repeat
        # nullable, so keep going
        local_rec.call(pattern.child, num_rules_seen)
      when :call
        local_rec.call(pattern.child, num_rules_seen)
      when :seq
        local_rec.call(pattern.left, num_rules_seen)
        # only check 2nd child if first is nullable
        local_rec.call(pattern.right, num_rules_seen) if pattern.left.nullable?
      when :ordered_choice
        # must check both children
        local_rec.call(pattern.left, num_rules_seen)
        local_rec.call(pattern.right, num_rules_seen)
      when :rule
        raise "rule '#{pattern.extra}' may be left-recursive" if rules_seen[0...num_rules_seen].include?(pattern)

        num_rules_seen += 1
        rules_seen[num_rules_seen] = pattern
        local_rec.call(pattern.child, num_rules_seen)
      when :grammar
        # LPEG says: /* sub-grammar cannot be left recursive */
        # But why?
      else
        raise "Unhandled case #{pattern.type} in verify_rule"
      end
    end

    local_rec.call(rule, 0)
  end

  # From lptree.c
  #
  # /*
  # ** Check whether a tree has potential infinite loops
  # */
  def loops?(pattern)
    return true if pattern.type == :repeat && pattern.child.nullable?

    # /* sub-grammars already checked */
    #
    # The comment refers to verify_grammar
    return false if pattern.type == :grammar

    # left-recursive grammar loops are handled in verifygrammar
    return false if pattern.type == :call

    case pattern.num_children
    when 1
      loops?(pattern.child)
    when 2
      fst = loops?(pattern.left)
      return true if fst

      loops?(pattern.right)
    end
  end
end

# Instances are generated during program generation in Pattern and consumed in the ParsingMachine
class Instruction
  OP_CODES = %i[char charset any jump choice call return commit back_commit partial_commit span end fail fail_twice unreachable].freeze
  OP_WIDTH = OP_CODES.map(&:length).max

  attr_reader :op_code, :arg1, :arg2

  def initialize(op_code, arg1 = nil, arg2 = nil)
    raise "Bad instruction op_code #{op_code}" unless OP_CODES.include?(op_code)
    raise 'Cannot specify arg2 for Instruction without arg1' if arg2 && !arg1

    @op_code = op_code
    @arg1 = arg1
    @arg2 = arg2
  end

  def to_s
    str = "#{op_code.to_s.capitalize.rjust(OP_WIDTH + 1)}"
    str << " #{arg1}" if arg1
    str << ", #{arg2}" if arg2
    str
  end
end

class ParsingMachine
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

      instr = @program[@current_instruction].must_be_a(Instruction)
      # op_code, arg1, _arg2 = @program[@current_instruction]

      match_char_p = lambda do |success|
        if success
          @current_instruction += 1
          @current_subject_position += 1
        else
          @current_instruction = :fail
        end
      end

      case instr.op_code
      when :char
        # arg1 is a single character
        match_char_p.call(instr.arg1 == @subject[@current_subject_position])
      when :charset
        match_char_p.call(instr.arg1.include?(@subject[@current_subject_position]))
      when :any
        # arg1 is the number of chars we are looking for
        if @current_subject_position + instr.arg1 <= @subject.size
          @current_instruction += 1
          @current_subject_position += instr.arg1
        else
          @current_instruction = :fail
        end
      when :jump
        # arg1 is an offset for the label to jump to
        @current_instruction += instr.arg1
      when :choice
        # arg1 is the offset for the other side of the choice, which we push onto the stack
        push(:state, instr.arg1)
        @current_instruction += 1
      when :call
        # arg1 is an offset for the label to call.
        #
        # Call is like jump, but we push the return address onto the stack first
        push(:instruction, 1)
        @current_instruction += instr.arg1
      when :return
        @current_instruction = pop(:instruction)
      when :commit
        # we pop and discard the top of the stack (which must be a triple) and then do the jump given by arg1. Even though we are
        # discarding it check that it was a full state as a sanity check.
        _ = pop(:state)
        @current_instruction += instr.arg1
      when :partial_commit
        # Sort of a combination of commit (which pops) and choice (which pushes), but we just tweak the top of the stack. See
        # Ierusalimschy, sec 4.3
        stack_top = peek(:state)
        raise "Empty stack for partial commit!" unless stack_top

        stack_top[1] = @current_subject_position
        stack_top[2] = @capture_list.clone

        @current_instruction += instr.arg1
      when :back_commit
        # A combination of a fail and a commit. We backtrack, but then jump to the specified instruction rather than using the
        # backtrack label. It's used for the :and pattern. See Ierusalimschy, 4.4
        _, subject_pos, captures = pop(:state)
        @current_subject_position = subject_pos
        @capture_list = captures
        @current_instruction += instr.arg1
      when :span
        # Special instruction for when we are repeating over a charset, which is common. We just consume as many maching characters
        # as there are. This never fails as we might just match zero
        @current_subject_position += 1 while instr.arg1.include?(@subject[@current_subject_position])

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
      when :unreachable
        raise "VM reached :unreachable instruction at line #{@current_instruction}"
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
