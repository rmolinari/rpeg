# frozen_string_literal: true

# See the README file for a little context. And also:
#
#   http://www.inf.puc-rio.br/~roberto/lpeg/#func

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
  NODE_TYPES = %i[
    charset string any seq ordered_choice repeated not and
    ntrue nfalse grammar open_call rule call capture
  ].each do |op|
    const_set op.upcase, op
  end

  attr_reader :type, :left, :right, :data, :capture

  class << self
    # Match any character in string (regarded as a set of characters), range, or Set
    def S(charset)
      case charset
      when Range, Set
        Pattern.new(CHARSET, data: charset)
      when String
        Pattern.new(CHARSET, data: Set.new(charset.chars))
      else
        raise "Cannot create a character set pattern from #{chars}"
      end
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
          new(STRING, data: arg)
        end
      when Integer
        # When n >= 0, match at least n chars.
        # When n < 0, there must not be n or more characters left
        if arg.zero?
          P(true)
        elsif arg.positive?
          new(ANY, data: arg)
        else
          # "Does not match n characters"
          -new(ANY, data: -arg)
        end
      when FalseClass
        new(NFALSE)
      when TrueClass
        new(NTRUE)
      when Hash
        new(GRAMMAR, data: arg)
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

      Pattern.new(CHARSET, data: result)
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
      Pattern.new(OPEN_CALL, data: ref)
    end

    # LPEG: Creates a simple capture, which captures the substring of the subject that matches patt. The captured value is a
    # string. If patt has other captures, their values are returned after this one.
    def C(pattern)
      Pattern.new(CAPTURE, pattern, capture: Capture::SIMPLE)
    end

    # LPEG: Creates a constant capture. This pattern matches the empty string and produces all given values as its captured values.
    #
    # No value at all - Cc() - adds nothing to the result, which is different from a value of nil.
    #
    # We capture several values with individual captures.
    def Cc(*values)
      pattern = P(true)
      until values.empty?
        val = values.pop

        pattern = Pattern.new(CAPTURE, pattern, data: val, capture: Capture::CONST)
      end

      pattern
    end

    # LPEG: Creates a position capture. It matches the empty string and captures the position in the subject where the match
    # occurs. The captured value is a number.
    def Cp
      Pattern.new(CAPTURE, P(true), capture: Capture::POSITION)
    end

    # Capture the n-th extra argument provided to #match. The first extra argument is n=1, etc.
    #
    # We accept missing argument to match some LPEG test cases. An error is raised
    def Carg(num = nil)
      raise "Invalid argument for Carg: #{num || 'nil'}" unless num.is_a?(Integer) && num&.positive?

      Pattern.new(CAPTURE, P(true), data: num, capture: Capture::ARGUMENT)
    end

    # See the instance method #match for the arguments
    def match(thing, string, init = 0, *extra_args)
      P(thing).match(string, init, *extra_args)
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
  #
  # str: the string the match against
  # init: the string index to start at, defaulting to 0
  # extra_args: used by Argument Captures
  def match(str, init = 0, *extra_args)
    machine = ParsingMachine.new(program + [Instruction.new(Instruction::OP_END)], str, init, extra_args)
    machine.run

    return machine.captures if machine.success?

    # otherwise return nil
  end

  # If left is defined and right is nil - so we have a unary op - we can get child here
  def child
    raise 'Pattern is not unary' if right

    left
  end

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
    return self if other.type == NTRUE
    return other if type == NTRUE

    Pattern.new(SEQ, self, other)
  end

  # p1 + p2 is ordered choice: if p1 matches we match, otherwise try matching on p2
  def +(other)
    other = fix_type(other)

    if type == CHARSET && other.type == CHARSET
      # Take the union of the charsets
      Pattern.S(charset_union(data, other.data))
    elsif type == ORDERED_CHOICE
      # rejigger to make this operation right-associative which makes for more efficient compiled code. See Ierusalimschy 4.2
      left + (right + other)
    elsif other.type == NFALSE
      # false is the right-identity for +
      self
    else
      Pattern.new(ORDERED_CHOICE, self, other)
    end
  end

  # pat ** n means "n or more occurrences of def"
  def **(other)
    n = other

    if n >= 0
      # So, we represent this by a sequence of num occurrences, followed by a zero-or-more
      raise "Pattern may match 0-length string so repetition may lead to an infinite loop" if nullable?

      patt = Pattern.new(REPEATED, self) # this repeats 0 or more times
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
    Pattern.new(NOT, self)
  end

  # Unary "and": pattern matches here (without consuming any input)
  #
  # Ierusalimschy points out that &patt can be implemented as --patt, but there is an optimization for the VM, so we preserve it
  def +@
    Pattern.new(AND, self)
  end

  # Difference is "this but not that". So p1 - p2 matches if p1 does and p2 doesn't
  #
  # Special case: if both patterns are charsets we replace with a single charset
  def -(other)
    other = fix_type(other)

    if type == CHARSET && other.type == CHARSET
      new_cs = charset_difference(data, other.data)
      return Pattern.P(false) if new_cs.is_a?(Set) && new_cs.empty?

      return Pattern.S(new_cs)
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
    when CHARSET
      result << "#{type_s}: #{data.join}"
    when STRING, ANY
      result << "String: #{data}"
    when NTRUE
      result << "TRUE"
    when NFALSE
      reasult << "FALSE"
    when OPEN_CALL
      result << "OpenCall: #{data}"
    when CALL
      result << "Call: #{data}"
    when SEQ, ORDERED_CHOICE
      result << (type == SEQ ? "Seq:" : "Ordered Choice:")
      do_sub_pattern.call(left)
      do_sub_pattern.call(right)
    when REPEATED, NOT, AND
      result << type_s
      do_sub_pattern.call(child)
    when CAPTURE
      result << "#{capture} #{data.inspect}"
      do_sub_pattern.call(child)
    when GRAMMAR
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
    when CHARSET, STRING, ANY, NTRUE, NFALSE, OPEN_CALL
      0
    when REPEATED, AND, NOT, CALL, RULE, CAPTURE
      1
    when SEQ, ORDERED_CHOICE
      2
    when GRAMMAR
      raise "#num_children isn't meaningful for GRAMMAR nodes"
    else
      raise "Unhandled pattern type #{type}"
    end
  end

  ########################################
  # Code generation

  def program
    return @program if @program

    # shorthand
    i = Instruction

    prog = []
    case type
    when CHARSET
      prog << Instruction.new(i::CHARSET, data: Set.new(data))
    when STRING
      data.chars.each do |ch|
        prog << Instruction.new(i::CHAR, data: ch)
      end
    when ANY
      prog << Instruction.new(i::ANY, data: data)
    when SEQ

      # Just concatenate the code
      prog = left.program + right.program
    when NTRUE
      # we always succeed, which means we don't have to do anything at all
    when NFALSE
      prog << Instruction.new(i::FAIL) unless data
    when OPEN_CALL
      # we resolved these to CALL when the grammar node was created. So if we see one now it is because it was not contained in a
      # grammar.
      raise 'OPEN_CALL node appears outside of a grammar'
    when CALL
      # This is symbolic target for now. It will be converted to a numeric offset during GRAMMAR analysis
      prog << Instruction.new(i::CALL, offset: data)
    when ORDERED_CHOICE
      p1 = left.program
      p2 = right.program

      prog << Instruction.new(i::CHOICE, offset: 2 + p1.size)
      prog += p1
      prog << Instruction.new(i::COMMIT, offset: 1 + p2.size)
      prog += p2
    when REPEATED
      p = child.program

      if child.type == CHARSET
        # Special, quicker handling when the thing we are repeated over is a charset. See Ierusalimschy 4.3
        prog << Instruction.new(i::SPAN, data: child.data)
      else
        prog << Instruction.new(i::CHOICE, offset: 2 + p.size)
        prog += p
        prog << Instruction.new(i::PARTIAL_COMMIT, offset: -p.size)
      end
    when NOT
      p = child.program

      prog << Instruction.new(i::CHOICE, offset: 2 + p.size)
      prog += p
      prog << Instruction.new(i::FAIL_TWICE)
    when AND
      p = child.program

      prog << Instruction.new(i::CHOICE, offset: 2 + p.size)
      prog += p
      prog << Instruction.new(i::BACK_COMMIT, offset: 2)
      prog << Instruction.new(i::FAIL)
    when CAPTURE
      case capture
      when Capture::CONST, Capture::POSITION, Capture::ARGUMENT
        prog << Instruction.new(i::FULL_CAPTURE, data: data, aux: { capture_length: 0, kind: capture })
        prog += child.program
      when Capture::SIMPLE
        prog << Instruction.new(i::OPEN_CAPTURE, aux: { capture_length: 0, kind: capture })
        prog += child.program
        prog << Instruction.new(i::CLOSE_CAPTURE, aux: { capture_length: 0, kind: Capture::CLOSE })
      else
        raise "Unhandled capture kind #{capture}"
      end
    when GRAMMAR
      start_line_of_nonterminal = {}
      full_rule_code = []

      child.each do |rule|
        nonterminal = rule.data
        rule_pattern = rule.left
        start_line_of_nonterminal[nonterminal] = 2 + full_rule_code.size
        full_rule_code += rule_pattern.program + [Instruction.new(i::RETURN)]
      end

      prog << Instruction.new(i::CALL, offset: @nonterminal_by_index[0]) # call the first nonterminal
      prog << Instruction.new(i::JUMP, offset: 1 + full_rule_code.size) # we are done: jump to the line after the grammar's program
      prog += full_rule_code

      # Now close the CALL instructions. THe OPEN_CALL nodes were analyzed when the GRAMMAR node was created but we still need to
      # calculate line offsets
      prog.each_with_index do |instr, idx|
        next unless instr.op_code == CALL

        nonterminal = instr.offset

        start_line = start_line_of_nonterminal[nonterminal]
        raise "Nonterminal #{nonterminal} does not have a rule in grammar" unless start_line

        offset = start_line - idx

        # We replaced OPEN_CALL with CALL earlier in #fix_up_grammar. But, if the following instruction is a :return this a tail
        # call and we can eliminate the stack push by using a :jump instead of the call. This leaves the following :return a dead
        # statement which we will never reach. We change it to a bogus op code as a sanity check: if the VM ever reaches it we have
        # made an error somewhere.
        if prog[idx + 1] && prog[idx + 1].op_code == :return
          prog[idx] = Instruction.new(i::JUMP, offset: offset)
          prog[idx + 1] = Instruction.new(i::UNREACHABLE)
        else
          prog[idx] = Instruction.new(i::CALL, offset: offset)
        end
      end
    else
      raise "Unknown pattern type #{type}"
    end

    @program = prog.map(&:freeze).freeze
  end

  ########################################
  # Misc

  # Left and right are subtrees/patterns.
  # data is other relevant data
  # capture is used for Capture patterns
  #
  def initialize(type, left = nil, right = nil, data: nil, capture: nil)
    raise "Bad node type #{type}" unless NODE_TYPES.include?(type)

    @type = type
    @left = left
    @right = right

    # When we have information that isn't a pattern
    @data = data

    @capture = capture

    sanity_check

    if type == GRAMMAR
      fix_up_grammar

      Analysis.verify_grammar(self)
    end

    @program = nil
  end

  # Special operation when closing open calls
  def convert_open_call_to_call!(rule, ref)
    raise "Cannot convert pattern to CALL" unless type == OPEN_CALL
    raise "Must give rule and nonterminal symbol to CALL pattern" unless rule && ref
    raise "Rule for CALL pattern must be a rule, got #{rule.type}" unless rule.type == RULE

    @type = CALL
    @left = rule
    @data = ref

    # We must check these again when needed rather than use the memoized values
    %i[@nullable @nofail].each do |ivar|
      remove_instance_variable(ivar) if instance_variable_defined?(ivar)
    end
  end

  private def sanity_check
    capture.must_be nil unless type == CAPTURE

    case type
    when CHARSET
      right.must_be nil
      left.must_be nil
      data.must_be_a(Set, Range)
    when NTRUE, NFALSE
      right.must_be nil
      left.must_be nil
      data.must_be nil
    when GRAMMAR
      right.must_be nil
      left.must_be nil
      data.must_be_a(Hash)
      data.must_not.empty?
    when OPEN_CALL
      right.must_be nil
      left.must_be nil
      data.must_not.negative? if left.is_a?(Integer)
    when CALL
      left_must_be_a(Rule)
      right.must_be nil
      data.must_be
    when RULE
      left.must_be_a Pattern
      right.must_be nil
      data.must_be
    when CAPTURE
      left.must_be_a Pattern
      right.must_be nil
      capture.must_be
    end
  end

  # We do several things
  # - make sure each rule pattern is actually a pattern.
  #   - since we can specify rules as strings, say, or subgrammars (as hash) we need to step in here
  # - the hash table of rules is replaced with a list of [nonterminal, pattern] pairs
  # - :opencall(v) patterns are replaced with CALL(rule) patterns
  # - convert the hashtable of rules to a same-order list of RULE nodes.
  #
  # We set up
  #  @nonterminal_indices: map nonterminal symbols to their index (0, 1, ...)
  #  @nonterminal_by_index: may indices to the corresopnding nonterminal
  private def fix_up_grammar
    raise "Bad type for #fix_up_grammar" unless type == GRAMMAR

    @nonterminal_indices = {}
    @nonterminal_by_index = []

    grammar_hash = data.transform_values!{ Pattern.P(_1) }
    grammar_hash.transform_keys! { |key| key.is_a?(String) ? key.to_sym : key }

    rule_hash = {}
    rule_list = []

    grammar_hash.each_with_index do |rule, idx|
      nonterminal, rule_pattern = rule
      raise "Nonterminal #{nonterminal} appears twice in grammar" if @nonterminal_indices[nonterminal]

      rule = Pattern.new(RULE, rule_pattern, data: nonterminal)

      rule_list << rule
      rule_hash[nonterminal] = rule

      @nonterminal_indices[nonterminal] = idx
      @nonterminal_by_index[idx] = nonterminal
    end

    @left = rule_list
    @data = nil # we don't need the Hash any more

    # Traverse the grammar rules and fix open calls. Do it in-line so we don't risk traversing the tree(s) via a generic visitor
    # while modifying the tree
    fix_it = lambda do |node|
      return if node.type == GRAMMAR # subgrammars already fixed
      return if node.type == CALL # already done

      if node.type == OPEN_CALL
        ref = node.data
        if ref.is_a?(Integer) && ref >= 0
          symb_ref = @nonterminal_by_index[ref]
          raise "bad grammar index for rule '#{ref}'" unless symb_ref

          ref = symb_ref
        end
        raise "bad grammar reference for rule '#{ref}'" unless @nonterminal_indices[ref]

        rule = rule_hash[ref].must_be
        node.convert_open_call_to_call!(rule, ref)
        return
      end

      return if node.num_children.zero?

      fix_it.call(node.left)
      fix_it.call(node.right) if node.num_children == 2
    end

    rule_list.each { |rule| fix_it.call(rule) }
  end

  # Namespace for some analysis methods
  module Analysis
    CHECK_PREDICATES = %i[nullable nofail].freeze

    # These two are cached in pattern.nullable? and pattern.nofail?
    module_function def nullable?(pattern)
      check_pred(Pattern.P(pattern), :nullable)
    end

    module_function def nofail?(pattern)
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
    # ** The function assumes that TOpenCall is not nullable; this will be checked again when the grammar is fixed.  Run-time
    # ** captures

    # ** can do whatever they want, so the result is conservative.
    # */
    #
    # TODO:
    #  - implement for our equivalent of TRep, TRunTime, TCaputre, etc. when we implement them
    module_function def check_pred(pattern, pred)
      raise "Bad check predicate #{pred}" unless CHECK_PREDICATES.include?(pred)

      # loop to eliminate some tail calls, as in the LPEG code. I don't think it's really necessary - as my implementation is not
      # going to be fast overall - but let's try a new technique.
      loop do
        case pattern.type
        when STRING, CHARSET, ANY, OPEN_CALL
          # Not nullable; for open_call this is a blind assumption
          return false
        when NTRUE
          return true
        when NFALSE
          return false
        when REPEATED
          return true # we never fail, as we can match zero occurrences
        when NOT
          # can match empty, but can fail
          return (pred != :nofail)
        when AND
          # can match empty; can fail exactly when body can
          return true if pred == :nullable

          pattern = pattern.child.must_be
        when SEQ
          return false unless check_pred(pattern.left, pred)

          pattern = pattern.right.must_be
        when ORDERED_CHOICE
          return true if check_pred(pattern.left, pred)

          pattern = pattern.right.must_be
        when GRAMMAR
          # Strings are matched by the initial nonterminal
          first_rule = pattern.child.first
          first_rule.type.must_be RULE
          pattern = first_rule.child.must_be
        when CALL, RULE, CAPTURE
          # The call's rule, rule's pattern, and capture's pattern are in child
          pattern = pattern.child.must_be
        else
          raise "Bad pattern type #{pattern.type}"
        end
      end
    end

    module_function def verify_grammar(grammar)
      raise "Not a grammar!" unless grammar.type == GRAMMAR

      # /* check infinite loops inside rules */
      grammar.child.each do |rule|
        verify_rule(rule)
        raise "Grammar has potential infinite loop in rule '#{rule.data}'" if loops?(rule)
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
    module_function def verify_rule(rule)
      rules_seen = []

      local_rec = lambda do |pattern, num_rules_seen|
        case pattern.type
        when STRING, CHARSET, ANY, NTRUE, NFALSE
          # no op
        when NOT, AND, REPEATED, CAPTURE
          # nullable, so keep going
          local_rec.call(pattern.child, num_rules_seen)
        when CALL
          local_rec.call(pattern.child, num_rules_seen)
        when SEQ
          local_rec.call(pattern.left, num_rules_seen)
          # only check 2nd child if first is nullable
          local_rec.call(pattern.right, num_rules_seen) if pattern.left.nullable?
        when ORDERED_CHOICE
          # must check both children
          local_rec.call(pattern.left, num_rules_seen)
          local_rec.call(pattern.right, num_rules_seen)
        when RULE
          raise "rule '#{pattern.data}' may be left-recursive" if rules_seen[0...num_rules_seen].include?(pattern)

          num_rules_seen += 1
          rules_seen[num_rules_seen] = pattern
          local_rec.call(pattern.child, num_rules_seen)
        when GRAMMAR
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
    module_function def loops?(pattern)
      return true if pattern.type == REPEATED && pattern.child.nullable?

      # /* sub-grammars already checked */
      #
      # The comment refers to verify_grammar
      return false if pattern.type == GRAMMAR

      # left-recursive grammar loops are handled in verifygrammar
      return false if pattern.type == CALL

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

  ########################################
  # Experimental monkeypatching
  #
  # Very annoyingly, Ruby's #coerce mechanism is only used by the Numeric types. This means things like "a" + Pattern.P(true) won't
  # work properly. The only way I can think to make it work - as it does in LPEG - is to monkeypatch String, TrueClass, FalseClass,
  # etc.

  # Trying the technique from https://stackoverflow.com/a/61438012/1299011
  module NonNumericOverloadExtension
    %i[+ * -].each do |sym|
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

# Instances are generated during program generation in Pattern and consumed in the ParsingMachine
#
# We don't need to slavish follow the LPEG structure - which has constraints like constant-size for C memory management and pointer
# arithmetic - but with the need to support captures cleanly we shouldn't ignore how LPEG does it.
#
# - op_code: the instruction op
# - offset: the address offset used in jumps, calls, etc.
# - aux: extra information used by instruction like capture
#   - in LPEG this is used to carefully pack data by bit-twiddling, etc., but we can use anything, such as structs, OpenStructs,
#     etc., as needed
# - data: this is called "key" in LPEG and is (I think) used to store pointers to Lua-based objects, etc.
#   - we will just store Ruby objects here.
#   - it contains things like the Set/Range of characters for Charset instructions, the character count for Any instructions, etc.
class Instruction
  OP_CODES = %i[
    char charset any jump choice call return commit back_commit
    partial_commit span op_end fail fail_twice unreachable
    full_capture
  ].each do |op|
    const_set op.upcase, op
  end

  OP_WIDTH = OP_CODES.map(&:length).max

  attr_reader :op_code, :offset, :data, :aux

  def initialize(op_code, offset: nil, data: nil, aux: nil)
    raise "Bad instruction op_code #{op_code}" unless OP_CODES.include?(op_code)

    @op_code = op_code
    @offset = offset
    @data = data
    @aux = aux
  end

  def to_s
    return @to_s if @to_s

    str = op_code.to_s.rjust(OP_WIDTH + 1)

    case op_code
    when CHAR, ANY
      str << " #{data}"
    when CHARSET, SPAN
      str << " #{data.join}"
    when JUMP, CHOICE, CALL, COMMIT, BACK_COMMIT, PARTIAL_COMMIT
      str << " #{offset}"
    when RETURN, OP_END, FAIL, FAIL_TWICE, UNREACHABLE
      # no-op
    else
      raise "Unhandled op_code #{op_code} in Instruction#to_s"
    end
    @to_s = str
  end
end

module Capture
  KINDS = %i[const position argument simple close].each do |kind|
    const_set kind.upcase, kind
  end

  # Used inside the VM when recording Captures
  class Breadcrumb
    attr_reader :size, :subject_index, :value, :kind

    # From LPEG:
    #
    # typedef struct Capture {
    #   const char *s;  /* subject position */
    #   unsigned short idx;  /* extra info (group name, arg index, etc.) */
    #   byte kind;  /* kind of capture */
    #   byte siz;  /* size of full capture + 1 (0 = not a full capture) */
    # } Capture;
    #
    # We use #value instead of #idx
    def initialize(size, subject_index, value, kind)
      @size = size
      @subject_index = subject_index
      @value = value
      @kind = kind

      raise "Bad Capture kind #{@kind}" unless KINDS.include?(@kind)
    end

    # An "open" capture is a "full capture" if it has non-zero size. See isfullcap in lpcap.c
    #
    # This feels janky, but for now I'm following the LPEG capture code as closely as I can
    def full_capture?
      @size.positive?
    end

    def close?
      @kind == CLOSE
    end
  end
end

# The VM used to run the programs generated from the patterns.
#
# See lpvm.c in the LPEG code.
class ParsingMachine
  # program: the program to run
  # subject: the string to match against
  # initial_pos: the position in subject to start the search at
  # extra_args may have been supplied in the initial #match call. These are consumed by Argument Captures.
  def initialize(program, subject, initial_pos, extra_args)
    @program = program.clone.freeze
    @prog_len = @program_size
    @subject = subject.clone.freeze

    @i_ptr = 0 # index in @program of the next instruction
    @subject_index = initial_pos
    @stack = []
    @breadcrumbs = [] # the records of the captures we make during parsing

    @extra_args = extra_args.clone
  end

  def success?
    @success
  end

  # TODO
  #
  # Instead of pushing clones of the breadcrumbs onto the stack and then restoring the entire data structure we could do what LPEG
  # does and use a fixed array for the breadcrumbs and push/pop the top-of-stack index. This is safe because, during a successful
  # match, breadcrumbs are never removed.
  #
  # LPEG does it that way - at least in part - because of C's manual memory management requirements, but it is efficient. This would
  # save object clones during a run. We would just need to clean things up at the end of the run so we don't have junk entries at
  # the end of the array when we analyse the breadcrumbs for capture returns.
  def run
    i = Instruction # shorthand
    loop do
      return if done?

      if @i_ptr == :fail
        handle_fail_ptr
        next
      end

      raise "current instruction pointer #{@i_ptr} is negative" if @i_ptr.negative?

      instr = @program[@i_ptr].must_be_a(Instruction)

      case instr.op_code
      when i::CHAR
        # arg1 is a single character
        check_char(instr.data == @subject[@subject_index])
      when i::CHARSET
        check_char(instr.data.include?(@subject[@subject_index]))
      when i::ANY
        # arg1 is the number of chars we are looking for
        if @subject_index + instr.data <= @subject.size
          @i_ptr += 1
          @subject_index += instr.data
        else
          @i_ptr = :fail
        end
      when i::JUMP
        @i_ptr += instr.offset
      when i::CHOICE
        # We push the offset for the other side of the choice
        push(:state, instr.offset)
        @i_ptr += 1
      when i::CALL
        # Call is like jump, but we push the return address onto the stack first
        push(:instruction, 1)
        @i_ptr += instr.offset
      when i::RETURN
        @i_ptr = pop(:instruction).i_ptr
      when i::COMMIT
        # we pop and discard the top of the stack (which must be a full state) and then do the jump given by arg1. Even though we
        # are discarding it check that it was a full state for sanity.
        _ = pop(:state)
        @i_ptr += instr.offset
      when i::PARTIAL_COMMIT
        # Sort of a combination of commit (which pops) and choice (which pushes), but we just tweak the top of the stack. See
        # Ierusalimschy, sec 4.3
        stack_top = peek(:state)
        raise "Empty stack for partial commit!" unless stack_top

        stack_top.subject_index = @subject_index
        stack_top.breadcrumbs = @breadcrumbs.clone
        @i_ptr += instr.offset
      when i::BACK_COMMIT
        # A combination of a fail and a commit. We backtrack, but then jump to the specified instruction rather than using the
        # backtrack label. It's used for the AND pattern. See Ierusalimschy, 4.4
        stack_top = pop(:state)
        @subject_index = stack_top.subject_index
        @breadcrumbs = stack_top.breadcrumbs
        @i_ptr += instr.offset
      when i::SPAN
        # Special instruction for when we are repeating over a charset, which is common. We just consume as many maching characters
        # as there are. This never fails as we might just match zero
        @subject_index += 1 while instr.data.include?(@subject[@subject_index])

        @i_ptr += 1
      when i::FAIL
        # We trigger the fail routine
        @i_ptr = :fail
      when i::FAIL_TWICE
        # An optimization for the not(pattern) implementation. We pop the top of the stack and discard it, and then enter the fail
        # routine again. For sanity's sake we'll check that the thing we are popping is a :state entry. See Ierusalimschy, 4.4
        _ = pop(:state)
        @i_ptr = :fail
      when i::OP_END
        @success = true
        done!
      when i::UNREACHABLE
        raise "VM reached :unreachable instruction at line #{@i_ptr}"
      when i::FULL_CAPTURE
        handle_capture(instr)
      else
        raise "Unhandled op code #{instr.op_code}"
      end
    end
  end

  private def handle_capture(instr)
    len = instr.aux[:capture_length].must_be
    # any value(s) for the match that need to be populated now
    kind = instr.aux[:kind].must_be

    match_value = case kind
                  when Capture::CONST, Capture::ARGUMENT
                    instr.data
                  when Capture::POSITION
                    @subject_index
                  else
                    "Unhandled capture kind #{kind}"
                  end

    add_capture(Capture::Breadcrumb.new(
                  1 + len,
                  @subject_index - len,
                  match_value,
                  instr.aux[:kind]
                ))
    @i_ptr += 1
  end

  private def done!
    @done = true
  end

  private def done?
    @done
  end

  # React to a character match or failure
  private def check_char(success)
    if success
      @i_ptr += 1
      @subject_index += 1
    else
      @i_ptr = :fail
    end
  end

  # Not for the FAIL op_code, but for when the instruction pointer is :fail
  private def handle_fail_ptr
    # special handling
    if @stack.empty?
      @success = false
      done!
    else
      top = pop

      if top.type == :instruction
      # nothing more to do
      else
        @i_ptr = top.i_ptr
        @subject_index = top.subject_index
        @breadcrumbs = top.breadcrumbs
      end
    end
  end

  ########################################
  # Stack manipulation

  # We push either
  # - an instruction pointer, which may later be used to jump, etc, or
  # - the current state with an offset, which is the [instr ptr + offset, subject_index, capture list] triple.
  private def push(type, offset)
    raise "Must push something onto stack" unless offset
    raise "Bad stack frame type" unless %i[instruction state].include?(type)

    frame = OpenStruct.new(type: type, i_ptr: @i_ptr + offset)
    if type == :state
      frame.subject_index = @subject_index
      frame.breadcrumbs = @breadcrumbs.clone
    end

    @stack.push frame
  end

  # Pop and return the top stack frame. If expected_type is non nil check that the frame has that type
  #
  # Raise if stack is empty
  private def pop(expected_type = nil)
    raise "Nothing in stack to pop" if @stack.empty?

    frame = @stack.pop
    check_frame(frame, expected_type)
    frame
  end

  # Peek and return the top of the stack, or nil if the stack is empty. The returned value, if an array, can be modified, thus
  # affecting the stack
  #
  # If expecting is given make sure that the top of the stack is of the given type
  private def peek(expected_type = nil)
    return nil if @stack.empty?

    frame = @stack.last
    check_frame(frame, expected_type)
    frame
  end

  private def check_frame(frame, expected_type)
    frame.must_be
    return unless expected_type

    raise "Top of stack is of type #{frame.type}, not of expected type #{expected_type}" unless frame.type == expected_type
  end

  private def add_capture(capture)
    capture.must_be_a Capture::Breadcrumb

    @breadcrumbs.push(capture)
  end

  ########################################
  # Capture extraction code

  # Returns the captures obtained when we ran the machine.
  #
  # If there are no captures we return the final index into the subject string. This is typically one past the matched section.
  # If there is exactly one capture we return it
  # If there are multiple captures we return them in an array (or perhaps other structures for more complex captures TBD)
  #
  # The capture code in LPEG is complicated and I don't understand very much of it. I think part of the complexity comes from the
  # manual memory management required in C and the need to interact with Lua values. All I can think to do is a) implement things in
  # a way that seems natural to me and that respects the LPEG documentation and tests and b) puzzle through the LPEG code when
  # necessary.
  #
  # The post-run capture retrieval is actually straightforward, at least at the top level: see getcaptures and pushcapture in
  # lpcap.c. But the code in the VM for ICloseRuntime is terrifying.
  #
  # Basic model:
  #
  # - We push Capture objects onto the stack as we run the VM based on the instructions generated from the patterns. We never pop
  #   anything from the stack: the Captures are breadcrumbs that let us work out after the fact what happend. Things do get removed
  #   from the Capture stack but only at backtrack points because a match has failed.
  # - The End instruction tacks on an unbalanced CloseCapture. This appears to be simply an end-marker like the null string
  #   terminator.
  # - After the VM runs we read through the Captures from the _oldest_ first until we reach the end-marker. So isn't not a stack,
  #   but a queue.
  #
  # Some properties derived from the LPEG docs and tests.
  # 1. an array capture gets flattened into the result.
  #    - So if we have two constant captures of 1 and [2,3,4] in sequence, say, the result will be [1, 2, 3, 4], and not
  #      [1, [2, 3, 4]]. Multiple match values are equivalent to sequential single match values.
  #    - LPEG actually implements a multivalue constant capture as several single capture. This is cleaner overall and I will do the
  #      same.
  #
  # - extra_args is the list of extra arguments provided to #match. These are used for argument captures
  def captures
    raise "Cannot call #captures unless machine ran sucessfully" unless done? && success?

    return @subject_index if @breadcrumbs.empty?

    # set it aside in case we need it later. The capture methods will shift @breadcrumbs as they go
    @final_captures = @breadcrumbs.clone

    @captures_to_return = [] # accumulate them here

    extract_next_capture until @breadcrumbs.empty?

    @captures_to_return = @captures_to_return.first if @captures_to_return.size == 1
    @captures_to_return
  end

  # Extract the next capture, returning the number of values obtained.
  private def extract_next_capture
    capture = @breadcrumbs.first.must_be_a Capture::Breadcrumb

    case capture.kind
    when Capture::CONST, Capture::POSITION
      @captures_to_return << capture.value
      @breadcrumbs.shift # consume it
      1
    when Capture::ARGUMENT
      index = capture.value
      raise "Reference to absent extra argument ##{index}" if index > @extra_args.size

      # with an Argument Capture the extra arguments are indexed from 1
      @captures_to_return << @extra_args[index - 1]
      @breadcrumbs.shift # consume it
      1
    when Capture::SIMPLE
      count = extract_nested_captures(add_extra: true).must_be.positive?

      # We need to make the whole match appear first in the list we just generated
      if count > 1
        back_shift = count - 1
        whole_match = @captures_to_return.pop
        @captures_to_return[-back_shift...-back_shift] = whole_match
      end
      count
    else
      raise "Unhandled capture kind #{capture.kind}"
    end
  end

  # See pushnestedcaptures in lpcap.c
  #
  # /*
  # ** Push on the Lua stack all values generated by nested captures inside
  # ** the current capture. Returns number of values pushed. 'addextra'
  # ** makes it push the entire match after all captured values. The
  # ** entire match is pushed also if there are no other nested values,
  # ** so the function never returns zero.
  # */
  #
  # The "current capture" is the first one in the given list.
  #
  # We append what we find to @captures_to_return and return their number.
  #
  # Code is closely based on the LPEG code.
  def extract_nested_captures(add_extra:)
    open_capture = @breadcrumbs.shift.must_be a Capture::Breadcrumb

    if open_capture.full_capture?
      # This is presumably a nontrivial capture that was converted to a "full" capture in code optimization or at runtime.
      # We don't do such optimziations yet but might.
      cpos = capture.subject_index
      match_range = (cpos - (capture.size - 1))..cpos
      @captures_to_return << @subject[match_range]
      return 1
    end

    count = 0
    count += extract_next_capture until @breadcrumbs.first.close? # Nested captures

    # We have reached our matching close
    close_capture = @breadcrumbs.shift.must_be_a Capture::Breadcrumb
    if add_extra || count.zero?
      match_range = (open_capture.subject_index)..(close_capture.subject_index)
      @captures_to_return << @subject[match_range]
      count += 1
    end

    count
  end
end
