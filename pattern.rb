# frozen_string_literal: true

# See the README file for a little context. And also:
#
#   http://www.inf.puc-rio.br/~roberto/lpeg/#func

require 'set'
require 'must_be'
# MustBe.disable

# This class is intended to play the same role as LPEG's lpeg module.
#
# Top-level differences from LPEG:
#
# - and patterns in LPEG are #patt (&patt in the first version) but +patt here
#   - unary & apparently can't be overloaded in Ruby
#   - this pattern matches when patt appears at the current location, but it doesn't consume any of the input
#   - +patt doesnt' read well, even though -patt does. I think this is because binary plus is so much more common when buliding
#     patterns than binary minus is.
#     - I tried using the "&" operator by overriding #to_proc but the Ruby parser rejects the &patt expression.
#     - There doesn't seem to be another workable option. According to https://stackoverflow.com/a/21060235/1299011 the unary
#       operators are !, ~, +, and -. We use - already and ! needs to be avoided because of idiomiatic Ruby checks like !foo for
#       existence. Operator ~ works, but that character is easy to mistake for a - when reading, which is bad. Ruby uses & as a
#       unary (for to_proc) but the Ruby parser doesn't allow its use in general. So I think we're stuck with +.
#
# - repeating patterns still use exponentiation, but it now looks like patt**n rather than patt^n because of Ruby's syntax
#   - so patt**n means
#
# - grammars are represented by hashes or arrays. LPEG uses Lua tables (which are a mashup of hashtables and arrays)
#
#   If an array then the nonterminals aren't named and all open calls must use numeric indices. The first element of the array is
#   either
#   - a non-negative integer 0, 1, 2, ... and specifies the (rule of the) initial nonterminal among the remaining elements with
#     indices reckoned _without_ that initial integer
#   - something else, which is interpreted as the pattern for the initial nonterminal
#
#   Otherwise the grammar is defined with a Hash. The keys are the nonterminal symbols and the values are the rule patterns.
#   - the keys must be symbols or strings (which are converted to symbols). No rule can use :initial or "initial" as
#     nonterminal.
#   - the open calls can refer either to the nonterminals (as strings or symbols) or to rule indices as they appear in the hash,
#     ignoring the :initial key (if present)
#   - :initial/"initial" can appear as a key in the hash to specify the initial nonterminal.
#     - if it is a non-zero integer it gives the index of the initial terminal's rule, reckoned without the presence of the :initial
#       key itself.
#     - if it is a symbol or a string it specifies the initial nonterminal directly
#
# - "Table" captures return a Hash if there are named captures to represent, and an array otherwise (though this may change). But we
#   continue to call them "table" captures even though there is no Table class in Ruby.
#   - Changing the name to "Hash" captures implies an implementation that may not be accurate.
#
# TODO:
# - Match-time captures, Cmt
# - Substitution captures, Cs
# - /-based captures
# - program generation optimations
#   - LPEG's "keyhole" optimizations
#   - other pattern-based optimizations: need to scan through the LPEG code again
#   - profiling
# - break this file into smaller parts
# - port LPEG's re module
class Pattern
  NODE_TYPES = %i[
    charset string any seq ordered_choice repeated not and
    ntrue nfalse grammar open_call rule call capture behind
  ].each do |op|
    const_set op.upcase, op
  end

  attr_reader :type, :left, :right, :capture
  attr_accessor :data # sometimes we need to tweak this

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
      when Hash, Array
        new(GRAMMAR, data: arg)
      else
        raise "Pattern.P does not support argument #{arg}"
      end
    end

    # Given a 2-char string xy, the ASCII range x..y. Each argument gives a range and we match on their union
    def R(*ranges)
      return P(false) if ranges.empty?

      check = lambda do |str|
        raise "Bad data #{str} for Pattern#R" unless str.is_a?(String) && str.size == 2

        (str[0])..(str[1])
      end

      result = ranges.map{ check.call(_1) }.reduce { |memo, operand| charset_union(memo, operand) }
      Pattern.new(CHARSET, data: result)
    end

    # An "open call" reference to a rule in a grammar. As we don't have the grammar yet - it is available in full only when we are
    # ready to compile - we remember it this way.
    #
    # ref should be either
    #  - a non-negative integer n, referring to the n-th rule in the grammar (0-based) or
    #  - a value that will is the key in the final grammar - a Hash - of the rule being referenced
    #    - strings are turned into symbols
    def V(ref)
      ref = ref.to_sym if ref.is_a?(String)
      Pattern.new(OPEN_CALL, data: ref)
    end

    # LPEG: Creates a simple capture, which captures the substring of the subject that matches patt. The captured value is a
    # string. If patt has other captures, their values are returned after this one.
    #
    # Note: it appears that when a simple capture is over another simple capture - like C(C(patt)) - we squeeze out the
    # duplication. See this test at l.216 of test.lua:
    #
    #     assert(#m.match(m.C(m.C(i)), string.rep('a', i)) == i)
    #
    # (In lua the # operator gives the size of a string or table)
    def C(pattern)
      pattern = P(pattern)
      return pattern if pattern.type == CAPTURE && pattern.capture == Capture::SIMPLE

      Pattern.new(CAPTURE, pattern, capture: Capture::SIMPLE)
    end

    # Capture the n-th extra argument provided to #match. The first extra argument is n=1, etc.
    #
    # We accept a missing argument to match some LPEG test cases but an error is raised
    def Carg(num = nil)
      raise "Invalid argument for Carg: #{num || 'nil'}" unless num.is_a?(Integer) && num&.positive?

      Pattern.new(CAPTURE, P(true), data: num, capture: Capture::ARGUMENT)
    end

    # From LPEG docs:
    #
    #   Creates a back capture. This pattern matches the empty string and produces the values produced by the most recent group
    #   capture named name (where name can be any Lua value).
    #
    #   Most recent means the last complete outermost group capture with the given name. A Complete capture means that the entire
    #   pattern corresponding to the capture has matched. An Outermost capture means that the capture is not inside another complete
    #   capture.
    #
    # nil is not allowed as a name
    def Cb(name)
      raise "Back capture must specify name of group" unless name

      Pattern.new(CAPTURE, P(true), data: name, capture: Capture::BACKREF)
    end

    # LPEG: Creates a constant capture. This pattern matches the empty string and produces all given values as its captured values.
    #
    # No value at all - Cc() - adds nothing to the result, which is different from a value of nil.
    #
    # We capture several values with individual captures.
    def Cc(*values)
      return P(true) if values.empty?

      patt = Pattern.new(CAPTURE, P(true), data: values.first, capture: Capture::CONST)
      return patt if values.size == 1

      # Otherwise, follow LPEG and make an anonymous Group capture over a sequence of single-val const captures
      values[1...].each do |val|
        patt *= Cc(val)
      end

      Cg(patt)
    end

    # From LPEG docs:
    #
    #  Creates a fold capture. If patt produces a list of captures C1 C2 ... Cn, this capture will produce the value
    #  func(...func(func(C1, C2), C3)..., Cn), that is, it will fold (or accumulate, or reduce) the captures from patt using
    #  function func.
    #
    # The second argument should be a lambda taking two arguments and returning one in the standard way.
    #
    # The first nested capture is examined. If there isn't one or it captures no values there is an error. If this capture contains
    # more than one value all but the first are discarded. This is the initial value for the fold. Then we extract the remaining
    # nested captures and use their values C2, ..., Cn in the fold as described.
    #
    # If Ci (i >= 2) produces k values then the lambda will receive k+1 arguments: the accumulator and the k captured values.
    # an array.
    def Cf(pattern, lambda)
      raise "Fold capture must have an accumulation lambda" unless lambda

      Pattern.new(CAPTURE, P(pattern), data: lambda, capture: Capture::FOLD)
    end

    # From LPEG docs:
    #
    #   Creates a group capture. It groups all values returned by patt into a single capture. The group may be anonymous (if no name
    #   is given) or named with the given name (which can be any non-nil Lua value).
    #
    #   An anonymous group serves to join values from several captures into a single capture. A named group has a different
    #   behavior. In most situations, a named group returns no values at all. Its values are only relevant for a following back
    #   capture or when used inside a table capture.
    #
    # The name doesn't have to be string. It can be anything other than nil, because nil means it's an anonymous group.
    def Cg(pattern, name = nil)
      Pattern.new(CAPTURE, P(pattern), data: name, capture: Capture::GROUP)
    end

    # LPEG: Creates a position capture. It matches the empty string and captures the position in the subject where the match
    # occurs. The captured value is a number.
    def Cp
      Pattern.new(CAPTURE, P(true), capture: Capture::POSITION)
    end

    # From LPEG:
    #
    #   Creates a table capture. This capture returns a table with all values from all anonymous captures made by patt inside this
    #   table in successive integer keys, starting at 1. Moreover, for each named capture group created by patt, the first value of
    #   the group is put into the table with the group name as its key. The captured value is only the table.
    #
    # For us, if there are no named group captures we return an array with the anonymous captures. Otherwise we return a hash with
    # integer keys 0, 1, 2, ... for the anonymous captures and the group names as keys for the others.
    #
    # So, the anonymous captures are available in the result, r, as r[0], r[1], ..., and the named captures as r[name].
    #
    # "Table" is a Lua term and is a like a hashtable crossed with an array.
    def Ct(patt)
      Pattern.new(CAPTURE, P(patt), capture: Capture::TABLE)
    end

    # Returns a pattern that matches only if the input string at the current position is preceded by patt. Pattern patt must match
    # only strings with some fixed length, and it cannot contain captures.
    def B(patt)
      patt = P(patt)
      len = patt.fixed_len
      raise "Behind match: pattern may not have fixed length" unless len >= 0
      raise "Behind match: pattern has captures" if patt.has_captures?

      # LPEG puts an upper bound of MAXBEHIND = 255 on how large the match can be here. I think it is because the value is packed
      # into a byte of memory. We don't care about that
      Pattern.new(BEHIND, patt, data: len)
    end

    # See the instance method #match for the arguments
    def match(thing, string, init = 0, *extra_args)
      P(thing).match(string, init, *extra_args)
    end

    # Try to keep a Range if possible
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

  # Return the index just after the matching prefix of str or nil if there is no match
  #
  # str: the string the match against
  # init: the string index to start at, defaulting to 0
  # extra_args: used by Argument Captures
  def match(str, init = 0, *extra_args)
    machine = ParsingMachine.new(program + [Instruction.new(Instruction::OP_END)], str, init, extra_args)
    machine.run

    return machine.captures if machine.success?
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

  # This only happens if other is a Numeric type, which is annoying. See the monkeypatching below for other cases.
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
    elsif type == NFALSE
      other
    elsif other.type == NFALSE
      self
    elsif type == ORDERED_CHOICE
      # rejigger to make this operation right-associative, giving more efficient compiled code. See Ierusalimschy 4.2
      left + (right + other)
    else
      Pattern.new(ORDERED_CHOICE, self, other)
    end
  end

  # pat ** n means "n or more occurrences of pat" when n is non-negative, and "up to -n occurrences of pat" if n is negative.
  def **(other)
    n = other.must_be_a(Integer)

    if n >= 0
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

  # Difference is "this but not that". So p1 - p2 matches if p1 does and p2 doesn't. Here, p2 doesn't consume any input but p1
  # does. The pattern is equivalent to -p2 * p1.
  #
  # Special case: if both patterns are charsets we replace with a single charset
  def -(other)
    other = fix_type(other)

    if type == CHARSET && other.type == CHARSET
      new_cs = charset_difference(data, other.data)
      return Pattern.P(false) if new_cs.is_a?(Set) && new_cs.empty?

      return Pattern.S(new_cs)
    end

    # Otherwise we use -p2 * p1, i.e., "p2 doesn't match here" followed by "try to match and consume p1"
    -other * self
  end

  # Replacement captures of various kinds
  #
  # From the LPEG docs:
  #
  #   patt / string
  #
  #     Creates a string capture. It creates a capture string based on string. The captured value is a copy of string, except that
  #     the character % works as an escape character: any sequence in string of the form %n, with n between 1 and 9, stands for the
  #     match of the n-th capture in patt. The sequence %0 stands for the whole match. The sequence %% stands for a single %.
  #
  #   patt / number
  #
  #     Creates a numbered capture. For a non-zero number, the captured value is the n-th value captured by patt. When number is
  #     zero, there are no captured values.
  #
  #   patt / table
  #
  #     Creates a query capture. It indexes the given table using as key the first value captured by patt, or the whole match if
  #     patt produced no value. The value at that index is the final value of the capture. If the table does not have that key,
  #     there is no captured value.
  #
  #   patt / function
  #
  #     Creates a function capture. It calls the given function passing all captures made by patt as arguments, or the whole match
  #     if patt made no capture. The values returned by the function are the final values of the capture. In particular, if function
  #     returns no value, there is no captured value.
  #
  # We create a query capture when a Hash is passed
  def /(other)
    case other
    when String
      Pattern.new(CAPTURE, self, data:other, capture: Capture::STRING)
    when Integer
      raise "Cannot use negative number for numbered capture" if other.negative?

      Pattern.new(CAPTURE, self, data: other, capture: Capture::NUM)
    when Hash
      Pattern.new(CAPTURE, self, data: other, capture: Capture::QUERY)
    when Proc
      Pattern.new(CAPTURE, self, data: other, capture: Capture::FUNCTION)
    else
      raise "Replacement capture is not supported for #{other}"
    end
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

    case type
    when CHARSET
      result << "CHARSET: #{data.join}"
    when STRING, ANY
      result << "#{type_s}: #{data}"
    when NTRUE
      result << "TRUE"
    when NFALSE
      result << "FALSE"
    when OPEN_CALL
      result << "OpenCall: #{data}"
    when CALL
      result << "Call: #{data}"
    when SEQ, ORDERED_CHOICE
      result << (type == SEQ ? "Seq:" : "Ordered Choice:")
      do_sub_pattern.call(left)
      do_sub_pattern.call(right)
    when REPEATED, NOT, AND, BEHIND
      result << type_s
      do_sub_pattern.call(child)
    when CAPTURE
      result << "Capture: #{capture} #{data.inspect}"
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

  def fixed_len
    Analysis.fixed_len(self)
  end

  def has_captures?
    Analysis.has_captures?(self)
  end

  # TODO: consider recurising via a #children method. Then we can handle the necessary rules in a GRAMMAR just once.
  def num_children
    case type
    when CHARSET, STRING, ANY, NTRUE, NFALSE, OPEN_CALL
      0
    when REPEATED, AND, NOT, CALL, RULE, CAPTURE, BEHIND
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
      prog = left.program + right.program
    when NTRUE
      # we always succeed, which means we don't have to do anything at all
    when NFALSE
      prog << Instruction.new(i::FAIL)
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
      # LPEG:
      # /*
      # ** And predicate
      # ** optimization: fixedlen(p) = n ==> <&p> == <p>; behind n
      # ** (valid only when 'p' has no captures)
      # */
      p = child.program
      len = child.fixed_len
      if len >= 0 && !child.has_captures?
        prog += p
        prog << Instruction.new(i::BEHIND, aux: len) if len.positive?
      else
        prog << Instruction.new(i::CHOICE, offset: 2 + p.size)
        prog += p
        prog << Instruction.new(i::BACK_COMMIT, offset: 2)
        prog << Instruction.new(i::FAIL)
      end
    when BEHIND
      prog << Instruction.new(i::BEHIND, aux: data) if data.positive?
      prog += child.program
    when CAPTURE
      len = fixed_len
      if len >= 0 && !child.has_captures?
        prog += child.program
        prog << Instruction.new(i::FULL_CAPTURE, data: data, aux: { capture_length: len, kind: capture })
      else
        prog << Instruction.new(i::OPEN_CAPTURE, data: data, aux: { capture_length: 0, kind: capture })
        prog += child.program
        prog << Instruction.new(i::CLOSE_CAPTURE, aux: { capture_length: 0, kind: Capture::CLOSE })
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

      prog << Instruction.new(i::CALL, offset: data) # call the nonterminal, in @data by fix_up_grammar
      prog << Instruction.new(i::JUMP, offset: 1 + full_rule_code.size) # we are done: jump to the line after the grammar's program
      prog += full_rule_code

      # Now close the CALL instructions.
      prog.each_with_index do |instr, idx|
        next unless instr.op_code == CALL

        nonterminal = instr.offset # still symbolic
        start_line = start_line_of_nonterminal[nonterminal]
        raise "Nonterminal #{nonterminal} does not have a rule in grammar" unless start_line

        # We replaced OPEN_CALL with CALL earlier in #fix_up_grammar. But, if the following instruction is a :return this a tail
        # call and we can eliminate the stack push by using a :jump instead of the call. This leaves the following :return a dead
        # statement which we will never reach. We change it to a bogus op code as a sanity check: if the VM ever reaches it we have
        # made an error somewhere.
        offset = start_line - idx
        if prog[idx + 1] && prog[idx + 1].op_code == :return
          prog[idx] = Instruction.new(i::JUMP, offset: offset)
          prog[idx + 1] = Instruction.new(i::UNREACHABLE)
        else
          prog[idx] = Instruction.new(i::CALL, offset: offset)
        end
      end
    else
      raise "Unhandled pattern type #{type}"
    end

    @program = prog.freeze
  end

  ########################################
  # Misc

  # Left and right are subpatterns.
  # data is other relevant data
  # capture is used for Capture patterns
  def initialize(type, left = nil, right = nil, data: nil, capture: nil)
    raise "Bad node type #{type}" unless NODE_TYPES.include?(type)

    @type = type
    @left = left
    @right = right
    @data = data
    @capture = capture
    sanity_check
    return unless type == GRAMMAR

    fix_up_grammar
    Analysis.verify_grammar(self)
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
      data.must_be_a(Set, Range)
    when NTRUE, NFALSE
      data.must_be nil
    when GRAMMAR
      data.must_be_a(Hash, Array)
      data.must_not.empty?
    when OPEN_CALL
      data.must_not.negative? if left.is_a?(Integer)
    when CALL
      data.must_be
    when RULE
      data.must_be
    when CAPTURE
      capture.must_be
    when BEHIND
      data.must_be
    end

    return if type == GRAMMAR

    case num_children
    when 0
      left.must_be nil
      right.must_be nil
    when 1
      left.must_be_a(Pattern)
      right.must_be nil
    when 2
      left.must_be_a(Pattern)
      right.must_be_a(Pattern)
    end
  end

  # The grammar is currently in @data. It can be either a Hash or an Array
  #
  # We do several things
  # - make sure each rule pattern is actually a pattern.
  #   - since we can specify rules as strings, say, or subgrammars (as hash) we need to step in here
  # - the hash/array in @data is replaced with an array of RULE patterns in @left
  # - the initial nonterminal (a symbol) is put into @data
  # - :opencall(v) nodes are replaced with CALL(rule) nodes
  #
  # We set up
  #  @nonterminal_indices: map nonterminal symbols to their index (0, 1, ...)
  #  @nonterminal_by_index: map indices to the corresopnding nonterminal
  private def fix_up_grammar
    raise "Bad type for #fix_up_grammar" unless type == GRAMMAR

    @nonterminal_indices = {}
    @nonterminal_by_index = []

    if data.is_a?(Array)
      # We replace it with an equivalent Hash
      initial_nonterminal = nil
      if data.first.is_a?(Integer)
        initial_rule_idx = data.shift # discard the Integer
        raise "Bad index for initial nonterminal in grammar" if initial_rule_idx.negative?
      else
        initial_rule_idx = 0
      end
      # Convert to a hash with sythentic keys
      as_hash = {}
      data.each_with_index do |p, i|
        key = "__#{i}".to_sym
        as_hash[key] = p
        initial_nonterminal = key if i == initial_rule_idx
      end
      raise "Bad grammar: no rule correspnds to an index of #{initial_rule_dix} for initial nonterminal" unless initial_nonterminal

      as_hash[:initial] = initial_nonterminal
      @data = as_hash
    end

    # Canonical representations of keys (symbols) and values (patterns)
    grammar_hash = {}
    data.each do |nonterminal, pattern|
      nonterminal = nonterminal.to_sym if nonterminal.is_a?(String)
      raise "Nonterminal symbol can be only a string or a symbol" unless nonterminal.is_a?(Symbol)

      next if nonterminal == :initial # the only case in which we don't specify a rule pattern

      grammar_hash[nonterminal] = Pattern.P(pattern)
    end

    initial_symbol = grammar_hash.delete(:initial)
    initial_symbol ||= grammar_hash.keys.first

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
    @data = initial_symbol.must_be # we don't need the Hash any more

    # Traverse a rule rules and fix open calls. Do it in-line so we don't risk traversing the tree(s) via a generic visitor while
    # modifying the tree
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
    # ** The function assumes that TOpenCall is not nullable; this will be checked again when the grammar is fixed.  Run-time
    # ** captures

    # ** can do whatever they want, so the result is conservative.
    # */
    #
    # TODO:
    #  - implement for our equivalent of TRep, TRunTime, TCaputre, etc. when we implement them
    def check_pred(pattern, pred)
      raise "Bad check predicate #{pred}" unless CHECK_PREDICATES.include?(pred)

      # loop to eliminate some tail calls, as in the LPEG code. I don't think it's really necessary - my implementation is not going
      # to be fast overall - but let's try a new technique.
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
        when NOT, BEHIND
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

    def verify_grammar(grammar)
      raise "Not a grammar!" unless grammar.type == GRAMMAR

      # /* check for infinite loops inside rules */
      grammar.child.each do |rule|
        verify_rule(rule)
        raise "Grammar has potential infinite loop in rule '#{rule.data}'" if loops?(rule)
      end
    end

    # We check if a rule can be left-recursive, i.e., whether we can return to the rule without consuming any input. The plan is to
    # walk the tree into subtrees whenever we see we can do so without consuming any input.
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
        when STRING, CHARSET, ANY, NTRUE, NFALSE, BEHIND
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
        # But why? I guess because we would have rejected it at creation.
        else
          raise "Unhandled case #{pattern.type} in verify_rule"
        end
      end

      local_rec.call(rule, 0)
    end

    # From checkloops in lptree.c
    #
    # /*
    # ** Check whether a tree has potential infinite loops
    # */
    def loops?(pattern)
      return true if pattern.type == REPEATED && pattern.child.nullable?

      # /* sub-grammars already checked */, i.e., by verify_grammar
      return false if pattern.type == GRAMMAR
      return false if pattern.type == CALL

      case pattern.num_children
      when 1
        loops?(pattern.child)
      when 2
        loops?(pattern.left) || loops?(pattern.right)
      end
    end

    # From callrecursive in LPEG's lpcode.c
    #
    # /*
    # ** Visit a TCall node taking care to stop recursion. If node not yet
    # ** visited, return 'f(sib2(tree))', otherwise return 'def' (default
    # ** value)
    # */
    #
    # This method acts as a circuit breaker for structural recursion that might otherwise get in a loop among mutually
    # recursive grammar rules.
    #
    # It's janky, but we follow LPEG's approach of hijacking the key field (which we call data) to keep track of the recursion
    def call_recursive(call_node, func, default)
      call_node.must_be
      call_node.type.must_be CALL
      call_node.child.type.must_be RULE

      already_visited = :already_visited

      data = call_node.data

      if data == already_visited
        default
      else
        # first time we've been here
        call_node.data = already_visited
        result = func.call(call_node)
        call_node.data = data
        result
      end
    end

    # From hascaptures in LPEG's lpcode.c
    # /*
    # ** Check whether a pattern tree has captures
    # */
    def has_captures?(node)
      case node.type
      when CAPTURE
        true
      when CALL
        call_recursive(node, ->(n) { has_captures?(n) }, false)
      when GRAMMAR
        node.child.any? { |rule| has_captures?(rule) }
      else
        case node.num_children
        when 0
          false
        when 1
          has_captures?(node.child)
        when 2
          has_captures?(node.left) || has_captures?(node.right)
        end
      end
    end

    # fixedlen from LPEG's lpcode.h
    #
    # /*
    # ** number of characters to match a pattern (or -1 if variable)
    # */
    #
    # We return -Infinity if the node's matches are not all of the same length
    def fixed_len(node)
      minus_infty = -Float::INFINITY
      case node.type
      when CHARSET
        1
      when ANY
        node.data
      when STRING
        node.data.length
      when NOT, AND, NTRUE, NFALSE, BEHIND
        0
      when REPEATED, OPEN_CALL
        minus_infty
      when CAPTURE, RULE
        fixed_len(node.child)
      when GRAMMAR
        fixed_len(node.child.first) # the first rule is the initial nonterminal
      when CALL
        call_recursive(node, ->(n) { fixed_len(n) }, minus_infty)
      when SEQ
        left_len = fixed_len(node.left)
        return left_len if left_len == minus_infty

        left_len + fixed_len(node.right)
      when ORDERED_CHOICE
        left_len = fixed_len(node.left)
        return left_len if left_len == minus_infty

        right_len = fixed_len(node.right)
        right_len == left_len ? right_len : minus_infty
      else
        raise "Unhandled node type #{node.type}"
      end
    end
  end

  ########################################
  # Experimental monkeypatching
  #
  # Very annoyingly, Ruby's #coerce mechanism is only used by the Numeric types. This means it doesn't help with things like "a" +
  # Pattern.P(true) even though we want the convenience.  The only way I can think to make it work is to monkeypatch String,
  # TrueClass, FalseClass, etc.

  # Technique from https://stackoverflow.com/a/61438012/1299011
  module NonNumericOverloadExtension
    %i[+ * -].each do |sym|
      define_method sym do |other|
        return Pattern.P(self).send(sym, other) if other.is_a?(Pattern)

        super(other)
      end
    end
  end

  [::String, ::TrueClass, ::FalseClass, ::Hash, ::Array].each do |klass|
    klass.class_eval do
      prepend NonNumericOverloadExtension
    end
  end
end

# Instances are generated during program generation in Pattern and consumed in the ParsingMachine
#
# - op_code: the instruction op
# - offset: the address offset used in jumps, calls, etc.
# - aux: extra information used by instruction like capture
#   - in LPEG this is used to carefully pack data by bit-twiddling, etc., but we can use anything, such as structs, etc., as needed
# - data: this is called "key" in LPEG and is (I think) used to store pointers to Lua-based objects, etc.
#   - we will just store Ruby objects here.
#   - it contains things like the Set/Range of characters for Charset instructions, the character count for Any instructions, etc.
class Instruction
  OP_CODES = %i[
    char charset any jump choice call return commit back_commit
    partial_commit span op_end fail fail_twice unreachable
    open_capture close_capture full_capture behind
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
    when BEHIND
      str << " #{aux}"
    when CHARSET, SPAN
      str << " #{data.join}"
    when JUMP, CHOICE, CALL, COMMIT, BACK_COMMIT, PARTIAL_COMMIT
      str << " #{offset}"
    when RETURN, OP_END, FAIL, FAIL_TWICE, UNREACHABLE
    # no-op
    when OPEN_CAPTURE, CLOSE_CAPTURE, FULL_CAPTURE
      str << " data:#{data} aux:#{aux}"
    else
      raise "Unhandled op_code #{op_code} in Instruction#to_s"
    end
    @to_s = str
  end
end

module Capture
  KINDS = %i[const position argument simple group backref table fold string num query function close].each do |kind|
    const_set kind.upcase, kind
  end

  # Used inside the VM when recording capture information.
  class Breadcrumb
    attr_reader :subject_index, :data, :kind
    # We update this when converting an open capture into a full capture
    attr_accessor :size

    # From LPEG:
    #
    # typedef struct Capture {
    #   const char *s;  /* subject position */
    #   unsigned short idx;  /* extra info (group name, arg index, etc.) */
    #   byte kind;  /* kind of capture */
    #   byte siz;  /* size of full capture + 1 (0 = not a full capture) */
    # } Capture;
    #
    # We use
    # - size instead of siz
    # - subject_index instead of s
    # - data instead of idx.
    def initialize(size, subject_index, data, kind)
      raise "Bad Capture kind #{kind}" unless KINDS.include?(kind)

      @size = size
      @subject_index = subject_index
      @data = data
      @kind = kind
    end

    # An "open" capture is a "full capture" if it has non-zero size. See isfullcap in lpcap.c
    #
    # This feels goofy, but for now I'm following the LPEG capture code as closely as I can
    def full?
      @size.positive?
    end

    def close?
      @kind == CLOSE
    end

    # The index of the end of the match
    # q.v. LPEG's closeaddr (lpcap.h)
    def end_index
      @subject_index + size - 1
    end

    def to_s
      @to_s ||= "Breadcrumb size:#{size} sub_idx:#{subject_index} data:#{data.inspect} kind:#{kind}"
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
    @program = program.clone.freeze.must_only_contain(Instruction)
    @prog_len = @program_size
    @subject = subject.clone.freeze

    @i_ptr = 0 # index in @program of the next instruction
    @subject_index = initial_pos
    @stack = []
    @breadcrumbs = [].must_only_ever_contain(Capture::Breadcrumb) # the records of the captures we make during parsing

    @extra_args = extra_args.clone
  end

  def success?
    @success
  end

  private def done!
    @done = true
  end

  private def done?
    @done
  end

  # TODO
  #
  # Instead of pushing clones of the breadcrumbs onto the stack and then restoring the entire data structure we could do what LPEG
  # does and use a fixed array for the breadcrumbs and push/pop the top-of-stack index. This is safe because, during a successful
  # match, breadcrumbs are never removed.
  #
  # LPEG does it that way - at least in part - because of C's manual memory management requirements, but it is also efficient. This
  # would save object clones during a run. We would just need to clean things up at the end of the run so we don't have junk entries
  # at the end of the array when we analyse the breadcrumbs for capture returns.
  def run
    i = Instruction # shorthand
    loop do
      return if done?

      if @i_ptr == :fail
        handle_fail_ptr
        next
      end

      raise "current instruction pointer #{@i_ptr} is negative" if @i_ptr.negative?

      instr = @program[@i_ptr]

      case instr.op_code
      when i::CHAR
        check_char(instr.data == @subject[@subject_index])
      when i::CHARSET
        check_char(instr.data.include?(@subject[@subject_index]))
      when i::ANY
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
        # as there are. This never fails as we can always match at least zero.
        @subject_index += 1 while instr.data.include?(@subject[@subject_index])
        @i_ptr += 1
      when i::BEHIND
        n = instr.aux # the (fixed) length of the pattern we want to match.
        if n > @subject_index
          # We can't jump back in the index so far
          @i_ptr = :fail
        else
          @subject_index -= n
          @i_ptr += 1
        end
      when i::FAIL
        @i_ptr = :fail
      when i::FAIL_TWICE
        # An optimization for the NOT implementation. We pop the top of the stack and discard it, and then enter the fail routine
        # again. For sanity's sake we'll check that the thing we are popping is a :state entry. See Ierusalimschy, 4.4
        _ = pop(:state)
        @i_ptr = :fail
      when i::OPEN_CAPTURE
        record_capture(instr, size: 0, subject_index: @subject_index)
      when i::CLOSE_CAPTURE
        # As in LPEG: "if possible, turn capture into a full capture"
        lc = @breadcrumbs.last.must_be # still on the breadcrumb list
        if lc.size.zero? && (@subject_index - lc.subject_index) < 255 # TODO: should we care about an upper bound here?
          # The previous breadcrumb was an OPEN, and we are closing it
          lc.size = @subject_index - lc.subject_index + 1
          @i_ptr += 1
        else
          record_capture(instr, size: 1, subject_index: @subject_index)
        end
      when i::FULL_CAPTURE
        # We have an all-in-one match, and the "capture length" tells us how far back in the subject the match started.
        len = instr.aux[:capture_length].must_be(Integer)
        record_capture(instr, size: 1 + len, subject_index: @subject_index - len)
      when i::OP_END
        @success = true
        done!
      when i::UNREACHABLE
        raise "VM reached :unreachable instruction at line #{@i_ptr}"
      else
        raise "Unhandled op code #{instr.op_code}"
      end
    end
  end

  # For this and the handling of the foo_CAPTURE op codes above, see the corresponding LPEG code in lpvm.c
  #
  # In that code, captop points to the "next" or "new" capture info, so captop - 1 is the current top.
  # Fields:
  #  - s is the subject index/pos
  #  - siz is 1 plus the "size" of the capture. A value of 0 indicates an OpenCapture (start of capture)
  #    - but when we are expecting an OpenCapture and see one with a non-zero size it means we actually have a "full" capture, which
  #      is a combined open/close. These have a "subject offset" (getoff(p)) that says how long the match is. Westick this value,
  #      when appropriate, the :capture_length field of the instr.aux hash.
  #  - idx is the "extra" data provided with the capture instruction, such as a const value represended as a pointer to Lua data.
  #    - it is often nil/absent
  #    - for us this comes from the #data field of the instruction
  #  - kind is the sort of capture it is: const, arg, etc.
  #
  # The first two vary with the capture type and are passed in here. The other two come cleanly out of the Instruction.
  private def record_capture(instr, size:, subject_index:)
    @breadcrumbs.push Capture::Breadcrumb.new(size, subject_index, instr.data, instr.aux[:kind].must_be)
    @i_ptr += 1
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
    if @stack.empty?
      @success = false
      done!
    else
      top = pop
      return if top.type == :instruction

      @i_ptr = top.i_ptr
      @subject_index = top.subject_index
      @breadcrumbs = top.breadcrumbs
    end
  end

  ########################################
  # Stack manipulation

  Frame = Struct.new :type, :i_ptr, :subject_index, :breadcrumbs

  # We push either
  # - an instruction pointer, which may later be used to jump, etc, or
  # - the current state with an offset, which is the [instr ptr + offset, subject_index, breadcrumb list] triple.
  private def push(type, offset)
    raise "Must push something onto stack" unless offset
    raise "Bad stack frame type" unless %i[instruction state].include?(type)

    frame = if type == :state
              Frame.new(type, @i_ptr + offset, @subject_index, @breadcrumbs.clone)
            else
              Frame.new(type, @i_ptr + offset)
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

  # Peek and return the top of the stack without popping it. Return nil if the stack is empty.
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
  # - We push Breadcrumb objects onto the stack as we run the VM based on the instructions generated from the patterns. We never pop
  #   anything from the stack: the Captures are breadcrumbs that let us work out after the fact what happend. Things do get removed
  #   from the Capture stack but only at backtrack points because a match has failed.
  # - The End instruction tacks on an unbalanced CloseCapture. This appears to be simply an end-marker like the null string
  #   terminator.
  # - After the VM runs we analyze the Breadcrumbs to calculate the captures. We go back and forth through the data, . So isn't not
  #   a stack, but an array..
  #
  # This method plays the same role as LPEG's getcaptures (lpcap.c)
  def captures
    raise "Cannot call #captures unless machine ran sucessfully" unless done? && success?

    @capture_state = CaptureState.new(@breadcrumbs)

    push_capture until @capture_state.done?

    result = @capture_state.captures
    if result.size == 1
      result = result.first
    elsif result.empty?
      result = @subject_index
    end
    result
  end

  # Extract the next capture, returning the number of values obtained.
  private def push_capture
    breadcrumb = @capture_state.current_breadcrumb

    case breadcrumb.kind
    when Capture::CONST
      @capture_state.push breadcrumb.data
      @capture_state.advance
      1
    when Capture::POSITION
      @capture_state.push breadcrumb.subject_index
      @capture_state.advance
      1
    when Capture::ARGUMENT
      index = breadcrumb.data
      raise "Reference to absent extra argument ##{index}" if index > @extra_args.size

      # with an Argument Capture the extra arguments are indexed from 1
      @capture_state.push @extra_args[index - 1]
      @capture_state.advance
      1
    when Capture::SIMPLE
      count = push_nested_captures(add_extra: true)
      # We need to make the whole match appear first in the list we just generated
      @capture_state.munge_last!(count) if count > 1
      count
    when Capture::GROUP
      if breadcrumb.data
        # Named group. We don't extract anything but just move forward. A Backref capture might find us later
        @capture_state.seek_next!
        0
      else
        push_nested_captures
      end
    when Capture::BACKREF
      group_name = breadcrumb.data
      breadcrumb_idx = @capture_state.index

      @capture_state.seek_back_ref!(group_name) # move to the named group capture
      count = push_nested_captures
      # restore our location and step to the next one
      @capture_state.index = breadcrumb_idx
      @capture_state.advance

      count
    when Capture::TABLE
      push_table_capture
    when Capture::FOLD
      push_fold_capture
    when Capture::STRING
      @capture_state.push extract_string_capture
      1
    when Capture::NUM
      push_num_capture
    when Capture::FUNCTION
      push_function_capture
    when Capture::QUERY
      push_query_capture
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
  # We append what we find to the capture state and return their number.
  #
  # Code is closely based on the LPEG code.
  def push_nested_captures(add_extra: false)
    open_capture = @capture_state.current_breadcrumb
    @capture_state.advance

    if open_capture.full?
      cpos = open_capture.subject_index
      match_len = open_capture.size - 1
      match_range = cpos...(cpos + match_len)
      @capture_state.push @subject[match_range]
      return 1
    end

    count = 0
    count += push_capture until @capture_state.current_breadcrumb.close? # Nested captures

    # We have reached our matching close
    close_capture = @capture_state.current_breadcrumb
    @capture_state.advance
    if add_extra || count.zero?
      match_range = (open_capture.subject_index)...(close_capture.subject_index)
      @capture_state.push @subject[match_range]
      count += 1
    end
    count.must_be.positive?
    count
  end

  # This is LPEG's tablecap (lpcap.h)
  #
  # Instead of always producing a Hash, potentially with integer keys 0, 1, 2, ..., we produce an array when there are no named
  # groups to worry about.
  # - TODO: reconsider this. Arrays are nicer than Hashes in this case but client code might not know which class to expect,
  #   especially when getting captures from a complicated pattern.
  #   - At first I always returned a Hash, with numeric keys 0, 1, 2, ... for the anonymous captures and name keys for the
  #     others. But this felt clunky, especially when we want to, say, join the anonymous arguments into a string.
  #   - Maybe we should return a hash with an :anonymous key giving the array of anonymous captures, or something like that.
  def push_table_capture
    if @capture_state.current_breadcrumb.full?
      # Empty table
      @capture_state.push []
      @capture_state.advance
      return 1
    end

    @capture_state.advance # move past the open capture
    named_results = {}
    indexed_results = []
    next_index = 0
    until @capture_state.current_breadcrumb.close?
      breadcrumb = @capture_state.current_breadcrumb
      if breadcrumb.kind == Capture::GROUP && breadcrumb.data
        # named group. We only keep track of the *first* value in the group
        push_one_nested_value
        value = @capture_state.pop
        named_results[breadcrumb.data] = value
      else
        # not a named group
        # k is the number we just got. We pop them back and put them in our result object
        k = push_capture
        (0..(k - 1)).to_a.reverse.each do |i|
          indexed_results[next_index + i] = @capture_state.pop
        end
        next_index += k
      end
    end
    @capture_state.advance # skip the close entry

    if named_results.empty?
      @capture_state.push indexed_results
    else
      indexed_results.each_with_index do |v, i|
        named_results[i] = v
      end
      @capture_state.push named_results.merge
    end
    1 # we pushed just a single entry, the hash or array
  end

  # This is LPEG's foldcap (lpcap.c)
  def push_fold_capture
    fn = @capture_state.current_breadcrumb.data.must_be

    if @capture_state.current_breadcrumb.full? ||
       (@capture_state.advance; @capture_state.current_breadcrumb.close?) ||
       (n = push_capture).zero?
      raise "no initial value for fold capture"
    end

    # discard all but one capture. This is the first value for the fold accumulator
    @capture_state.pop(n - 1)
    acc = @capture_state.pop
    until @capture_state.current_breadcrumb.close?
      n = push_capture
      acc = fn.call(acc, *@capture_state.pop(n))
    end
    @capture_state.advance # skip close
    @capture_state.push acc
    1
  end

  # Push nested values and then pop off all but one
  def push_one_nested_value
    n = push_nested_captures
    @capture_state.pop(n - 1)
  end

  # This is LPEG's numcap (lpcap.c)
  def push_num_capture
    idx = @capture_state.current_breadcrumb.data
    if idx.zero?
      # skip them all
      @capture_state.seek_next!
      return 0
    end

    n = push_nested_captures
    raise "no capture '#{idx}" if n < idx

    vals = @capture_state.pop(n) # pop them off
    @capture_state.push vals[idx - 1] # push back the one we want
    1
  end

  # This is LPEG's functioncap (lpcap.c)
  def push_function_capture
    proc = @capture_state.current_breadcrumb.data.must_be_a(Proc) # get the proc to call
    n = push_nested_captures # get the nested captures...
    args = @capture_state.pop(n) # ...pop them
    result = Array(proc.call(*args)) # ... and pass them to the proc
    num = result.size
    result.each { |cap| @capture_state.push cap } # the results, if any, are the capture values
    num
  end

  # This is LPEG's querycap (lpcap.c)
  def push_query_capture
    hash = @capture_state.current_breadcrumb.data.must_be_a(Hash)
    push_one_nested_value # /* get nested capture */
    query_key = @capture_state.pop # pop it
    result = hash[query_key]
    if result
      @capture_state.push(result)
      1
    else
      0 # no result
    end
  end

  # This is LPEG's stringcap (lpcap.c)
  #
  # We just return the result
  def extract_string_capture
    fmt = @capture_state.current_breadcrumb.data.must_be_a(String)
    str_caps = get_str_caps # /* collect nested captures */
    result = +""
    idx = -1
    loop do
      idx += 1
      break if idx >= fmt.length

      if fmt[idx] != "%"
        result << fmt[idx]
        next
      end

      idx += 1
      unless ('0'..'9').include?(fmt[idx])
        result << fmt[idx]
        next
      end

      capture_index = fmt[idx].to_i
      raise "invalid capture index (#{capture_index})" if capture_index > str_caps.size - 1

      str_cap = str_caps[capture_index]
      if str_cap.isstring
        result << @subject[(str_cap.subject_start)...(str_cap.subject_end)]
        next
      end

      cs_index = @capture_state.index
      @capture_state.index = str_caps[capture_index].breadcrumb_idx
      val = extract_one_string("capture") # lpeg's addonestring, but return instead of appending to b
      raise "no values in capture index #{capture_index}" unless val

      result << val
      @capture_state.index = cs_index
    end
    result
  end

  # This is LPEG's getstrcaps (lpcap.c)
  # /*
  # ** Collect values from current capture into array 'cps'. Current
  # ** capture must be Cstring (first call) or Csimple (recursive calls).
  # ** (In first call, fills %0 with whole match for Cstring.)
  # ** Returns number of elements in the array that were filled.
  # */
  #
  # We simply return the array of StrAux elements
  def get_str_caps
    result = []
    first_aux = StrAux.new
    first_aux.isstring = true
    first_aux.subject_start = @capture_state.current_breadcrumb.subject_index
    result << first_aux

    first_is_full = @capture_state.current_breadcrumb.full?
    if first_is_full
      result[0].subject_end = @capture_state.current_breadcrumb.end_index
      @capture_state.advance
    else
      @capture_state.advance
      until @capture_state.current_breadcrumb.close?
        if result.size > MAX_STR_CAPS
          @capture_state.seek_next! # just skip it
        elsif @capture_state.current_breadcrumb.kind == Capture::STRING
          result += get_str_caps # get the matches recursively
        else
          # Not a string
          aux = StrAux.new
          aux.isstring = false
          aux.breadcrumb_idx = @capture_state.index
          @capture_state.seek_next!
          result << aux
        end
      end
      result[0].subject_end = @capture_state.current_breadcrumb.end_index
      @capture_state.advance # skip capture close
    end
    result
  end

  # This is LPEG's addonestring (lpcap.c)
  #
  # /*
  # ** Evaluates a capture and adds its first value to buffer 'b'; returns
  # ** whether there was a value
  # */
  #
  # We just return the value or nil if there isn't one
  def extract_one_string(what)
    case @capture_state.current_breadcrumb.kind
    when Capture::STRING
      extract_string_capture
    # when Capture::SUBST
      #   extract_subst_cap
    else
      n = push_capture
      return nil if n.zero?

      @capture_state.pop(n-1) # just leave one
      res = @capture_state.pop
      # LPEG tests the type of this value with lua_isstring, which returns 1 if the value is a string or a number.
      raise "invalid #{what} value (a #{res.class})" unless res.is_a?(String) || res.is_a?(Numeric)

      res.to_s
    end
  end

  # q.v. struct StrAux in lpcap.c
  #
  # value is a Breadcrumb or subject index pair (start stop)
  StrAux = Struct.new :isstring, :breadcrumb_idx, :subject_start, :subject_end
  MAX_STR_CAPS = 10

  # q.v. LPEG's CaptureState, lpcap.h
  #
  # We'll also use this class for seeking operations like findnext findopen, etc.
  class CaptureState
    attr_reader :captures

    def initialize(breadcrumbs)
      @breadcrumbs = breadcrumbs
      @breadcrumb_idx = 0
      @captures = []
    end

    # push a captured value
    def push(cap)
      @captures << cap
    end

    # Pop the top capture off and return it, erroring if there isn't one.
    #
    # If the argument is given we pop off the last n captures as a chunk (not one by one) and return them in an array.
    #
    # nil might be in the stack, so we need to count rather than simply check pop for truthiness
    def pop(num = nil)
      if num
        raise "There are not #{num} captures to pop" if num > @captures.size

        @captures.pop(num)
      else
        raise "There is not a capture to pop" unless @captures.size > 0

        @captures.pop
      end
    end

    def done?
      @breadcrumb_idx == @breadcrumbs.size
    end

    def current_breadcrumb
      raise "No available breadcrumb" if done?

      @breadcrumbs[@breadcrumb_idx]
    end

    def advance
      @breadcrumb_idx += 1
    end

    def index
      @breadcrumb_idx
    end

    def index=(val)
      @breadcrumb_idx = val
    end

    # Search backwards from the current breadcrumb for the start of the group capture with the given name.
    #
    # If we find it the state index is updated appropriately.
    # If we don't find it we raise an exception.
    #
    # This is LPEG's findback() (lpcap.c)
    def seek_back_ref!(group_name)
      group_name.must_be
      while @breadcrumb_idx > 0
        @breadcrumb_idx -= 1
        # Skip nested captures
        if current_breadcrumb.close?
          seek_matching_open!
        else
          # The opening of a capture that encloses the BACKREF. Skip it and keep going backwards
          next unless current_breadcrumb.full?
        end
        # We are at an open capture that was closed before our BACKREF
        next unless current_breadcrumb.kind == Capture::GROUP # is it a group?
        next unless current_breadcrumb.data == group_name # does it have the right name?

        # We found it!
        return
      end
      raise "back reference '#{group_name}' not found"
    end

    # This is LPEG's findopen (lpcap.c)
    #
    # Assume we are starting from a close capture. We go back to the matching open capture.
    def seek_matching_open!
      n = 0 # number of nested closes waiting for an open
      loop do
        @breadcrumb_idx -= 1
        raise "subject index underflow in seek_open!" if @breadcrumb_idx < 0

        if current_breadcrumb.close?
          n += 1
        elsif current_breadcrumb.full?
          next
        end

        # It's a close
        return if n.zero?

        n -= 1
      end
    end

    # This is LPEG's nextcap (lpcap.c)
    #
    # Move to the next capture
    def seek_next!
      unless current_breadcrumb.full?
        n = 0 # number of nested opens waiting for a close
        loop do
          @breadcrumb_idx += 1
          if current_breadcrumb.close?
            break if n.zero?

            n -= 1
          elsif !current_breadcrumb.full?
            n += 1
          end
        end
      end

      @breadcrumb_idx += 1
    end

    # partially rotate the captures to make what is currently the final value the n-th from last value. For example, if @captures is
    # currently [0, 1, 2, 3, 4], then calling munge_last(3) makes it [0, 1, 4, 2, 3]. Now 4 (previously the last value) is third
    # from last. When n == 1 this is a no-op
    def munge_last!(n)
      return if n == 1
      raise "Bad munge argument" unless n.positive?

      @captures[-n...] = @captures[-n...].rotate(-1)
    end
  end
end
