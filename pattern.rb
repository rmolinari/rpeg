# frozen_string_literal: true

# See the README file for a little context. And also:
#
#   http://www.inf.puc-rio.br/~roberto/lpeg/#func

require 'set'
require 'must_be'
# MustBe.disable

require_relative 'captures'
require_relative 'parsing_machine'

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
# - program generation optimations
#   - other pattern-based optimizations: need to scan through the LPEG code again
#     - headfail(), getFirst(); need to understand them
#   - profiling
# - port LPEG's re module
class Pattern
  NODE_TYPES = %i[
    charset char any seq ordered_choice repeated not and
    ntrue nfalse grammar open_call rule call capture runtime behind
  ].each do |op|
    const_set op.upcase, op
  end

  FULL_CHAR_SET = Set.new (0..255).map(&:chr)

  attr_reader :type, :left, :right, :capture
  attr_accessor :data # sometimes we need to tweak this

  class << self
    # Match any character in string (regarded as a set of characters), range, or Set
    #
    # If the set is empty we have NFALSE, which always fails
    # If the set has a single element we have CHAR pattern, which is a little faster in the VM
    # Otherwise we have a CHARSET pattern
    def S(charset)
      case charset
      when Set
        size = charset.size
        return new(NFALSE) if size.zero?
        return new(CHAR, data: charset.first) if size == 1
        return new(ANY, data: 1) if charset == FULL_CHAR_SET

        Pattern.new(CHARSET, data: charset)
      when String
        S(Set.new(charset.chars))
      else
        raise "Cannot create a character set pattern from #{chars}"
      end
    end

    # Take argument and turn it into a pattern
    def P(arg)
      case arg
      when Pattern
        arg
      when String
        # a sequence of CHAR patterns
        patt = P(true)
        arg.chars.each do |ch|
          patt *= new(CHAR, data: ch)
        end
        patt
      when Integer
        # When n >= 0, match at least n chars.
        # When n < 0, there must not be n or more characters left
        return -P(-arg) if arg.negative?
        return P(true) if arg.zero?

        # LPEG represents this with a sequence of arg single-char ANY statements, i.e., not as described in the paper. I think it
        # makes certain code optimizations simpler to analyze. But doing that here slows down my unit tests to a suprising degree
        # (which itself shows that I'm doing something very inefficiently somewhere).
        new(ANY, data: arg)
      when FalseClass
        @false_tree ||= new(NFALSE)
      when TrueClass
        @true_tree ||= new(NTRUE)
      when Hash, Array
        new(GRAMMAR, data: arg)
      when Proc
        # a pattern equivalent to a match-time capture over the empty string.
        new(RUNTIME, P(true), data: arg)
      else
        raise "Pattern.P does not support argument #{arg}"
      end
    end

    # Given a 2-char string xy, the ASCII range x..y. Each argument gives a range and we match on their union.
    #
    # Always represent with a Set.
    def R(*ranges)
      return P(false) if ranges.empty?

      check = lambda do |str|
        raise "Bad data #{str} for Pattern#R" unless str.is_a?(String) && str.size == 2

        Set.new ((str[0])..(str[1])).to_a
      end

      result = ranges.map{ check.call(_1) }.reduce(:|)
      S(result)
    end

    # An "open call" reference to a rule in a grammar. As we don't have the grammar yet - it is available in full only when we are
    # ready to compile - we remember it this way.
    #
    # ref should be either
    #  - a non-negative integer n, referring to the n-th rule in the grammar (0-based) or
    #  - a value that will be the key in the final grammar - a Hash or Array- of the rule being referenced
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
    #   Creates a substitution capture, which captures the substring of the subject that matches patt, with substitutions. For any
    #   capture inside patt with a value, the substring that matched the capture is replaced by the capture value (which should be a
    #   string). The final captured value is the string resulting from all replacements.
    def Cs(patt)
      Pattern.new(CAPTURE, P(patt), capture: Capture::SUBST)
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

    # From LPEG:
    #
    #   Creates a match-time capture. Unlike all other captures, this one is evaluated immediately when a match occurs (even if it
    #   is part of a larger pattern that fails later). It forces the immediate evaluation of all its nested captures and then calls
    #   function.

    #   The given function gets as arguments the entire subject, the current position (after the match of patt), plus any capture
    #   values produced by patt.

    #   The first value returned by function defines how the match happens. If the call returns a number, the match succeeds and the
    #   returned number becomes the new current position. (Assuming a subject s and current position i, the returned number must be
    #   in the range [i, len(s) + 1].) If the call returns true, the match succeeds without consuming any input. (So, to return true
    #   is equivalent to return i.) If the call returns false, nil, or no value, the match fails.

    #   Any extra values returned by the function become the values produced by the capture.
    def Cmt(patt, function)
      # LPEG uses a separate RUNTIME node type instead of CAPTURE because certain functions, like hascaptures and fixedlen, need
      # special behavior here. Note
      #
      # LPEG also uses "runtime" interally instead of "matchtime". We follow
      Pattern.new(RUNTIME, P(patt), data: function)
    end

    # Returns a pattern that matches only if the input string at the current position is preceded by patt. Pattern patt must match
    # only strings with some fixed length, and it cannot contain captures.
    def B(patt)
      patt = P(patt)
      len = patt.fixed_len
      raise "Behind match: pattern may not have fixed length" unless len
      raise "Behind match: pattern has captures" if patt.has_captures?

      # LPEG puts an upper bound of MAXBEHIND = 255 on how large the match can be here. I think it is because the value is packed
      # into a byte of memory. We don't care about that
      Pattern.new(BEHIND, patt, data: len)
    end

    # See the instance method #match for the arguments
    def match(thing, string, init = 0, *extra_args)
      P(thing).match(string, init, *extra_args)
    end

    private def charset_union(cs1, cs2)
      cs1 = Set.new([cs1]) if cs1.is_a?(String) # single char
      cs2 = Set.new([cs2]) if cs2.is_a?(String) # single char

      cs1 + cs2
    end
  end

  # Return the index just after the matching prefix of str or nil if there is no match
  #
  # str: the string the match against
  # init: the string index to start at, defaulting to 0
  # extra_args: used by Argument Captures
  def match(str, init = 0, *extra_args)
    @program ||= optimize_jumps(code + [Instruction.new(Instruction::OP_END)])

    machine = ParsingMachine.new(@program, str, init, extra_args)
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

    # rejigger to make SEQ right-associative. I don't know that it makes a difference, but LPEG does it.
    return left * (right * other) if type == SEQ

    Pattern.new(SEQ, self, other)
  end

  # p1 + p2 is ordered choice: if p1 matches we match and never consider p2, otherwise try matching on p2
  def +(other)
    other = fix_type(other)

    if charsettish?(self) && charsettish?(other)
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

    return Pattern.S(charset_difference(data, other.data)) if charsettish?(self) && charsettish?(other)

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
  #   patt / table  [for this we accept a Hash]
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
  def /(other)
    case other
    when String
      Pattern.new(CAPTURE, self, data: other, capture: Capture::STRING)
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

  # Each is either a Set or a single character (from a CHAR node)
  private def charset_difference(cs1, cs2)
    cs1 = Set.new([cs1]) if cs1.is_a?(String) # single char
    cs2 = Set.new([cs2]) if cs2.is_a?(String) # single char
    cs1 - cs2
  end

  private def charset_union(cs1, cs2)
    self.class.send(:charset_union, cs1, cs2)
  end

  private def charsettish?(pattern)
    pattern.type == CHARSET || pattern.type == CHAR
  end

  def to_s
    return @to_s if @to_s

    result = []
    do_sub_pattern = lambda do |sub_patt|
      sub_patt.to_s.split("\n").each do |line|
        result << "|  #{line}"
      end
    end

    type_s = type.to_s.capitalize

    case type
    when CHARSET
      result << "Charset: #{data.join.dump}"
    when ANY
      result << "#{type_s}: #{data}"
    when CHAR
      result << "#{type_s}: #{data.dump}"
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
      result << "#{type_s}: "
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
    when CHARSET, CHAR, ANY, NTRUE, NFALSE, OPEN_CALL
      0
    when REPEATED, AND, NOT, CALL, RULE, CAPTURE, RUNTIME, BEHIND
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

  # Duplicate the code elements to avoid incorrect decoration showing up due to subprogram reuse
  private def code_for(pattern)
    pattern.code.map(&:dup)
  end

  def code
    return @code if @code

    # shorthand
    i = Instruction

    code = []
    case type
    when CHARSET
      code << Instruction.new(i::CHARSET, data: Set.new(data))
    when CHAR
      code << Instruction.new(i::CHAR, data:)
    when ANY
      code << Instruction.new(i::ANY, data:)
    when SEQ
      code = code_for(left) + code_for(right)
    when NTRUE
      # we always succeed, which means we don't have to do anything at all
    when NFALSE
      code << Instruction.new(i::FAIL)
    when OPEN_CALL
      # we resolved these to CALL when the grammar node was created. So if we see one now it is because it was not contained in a
      # grammar.
      raise 'OPEN_CALL node appears outside of a grammar'
    when CALL
      # This is symbolic target for now. It will be converted to a numeric offset during GRAMMAR analysis
      code << Instruction.new(i::CALL, offset: data)
    when ORDERED_CHOICE
      p1 = code_for(left)
      p2 = code_for(right)

      code << Instruction.new(i::CHOICE, offset: 2 + p1.size)
      code += p1
      code << Instruction.new(i::COMMIT, offset: 1 + p2.size)
      code += p2
    when REPEATED
      p = code_for(child)

      if child.type == CHARSET
        # Special, quicker handling when the thing we are repeated over is a charset. See Ierusalimschy 4.3
        code << Instruction.new(i::SPAN, data: child.data)
      else
        code << Instruction.new(i::CHOICE, offset: 2 + p.size)
        code += p
        code << Instruction.new(i::PARTIAL_COMMIT, offset: -p.size)
      end
    when NOT
      p = code_for(child)

      code << Instruction.new(i::CHOICE, offset: 2 + p.size)
      code += p
      code << Instruction.new(i::FAIL_TWICE)
    when AND
      # LPEG:
      # /*
      # ** And predicate
      # ** optimization: fixedlen(p) = n ==> <&p> == <p>; behind n
      # ** (valid only when 'p' has no captures)
      # */
      p = code_for(child)
      len = child.fixed_len
      if len && !child.has_captures?
        code += p
        code << Instruction.new(i::BEHIND, aux: len, dec: :and) if len.positive?
      else
        code << Instruction.new(i::CHOICE, offset: 2 + p.size)
        code += p
        code << Instruction.new(i::BACK_COMMIT, offset: 2)
        code << Instruction.new(i::FAIL)
      end
    when BEHIND
      code << Instruction.new(i::BEHIND, aux: data) if data.positive?
      code += code_for(child)
    when CAPTURE
      c = code_for(child)
      len = fixed_len
      if len && !child.has_captures?
        code += c
        code << Instruction.new(i::FULL_CAPTURE, data:, aux: { capture_length: len, kind: capture })
      else
        code << Instruction.new(i::OPEN_CAPTURE, data:, aux: { kind: capture })
        code += c
        code << Instruction.new(i::CLOSE_CAPTURE, aux: { kind: Capture::CLOSE })
      end
    when RUNTIME
      code << Instruction.new(i::OPEN_CAPTURE, data:, aux: { kind: Capture::GROUP })
      code += code_for(child)
      code << Instruction.new(i::CLOSE_RUN_TIME, aux: { kind: Capture::CLOSE })
    when RULE
      code = code_for(child)
      code.first.dec = data # decorate with the nonterminal
    when GRAMMAR
      start_line_of_nonterminal = {}
      full_rule_code = []

      child.each do |rule|
        # byebug if $do_it
        nonterminal = rule.data
        start_line_of_nonterminal[nonterminal] = 2 + full_rule_code.size
        full_rule_code += code_for(rule) + [Instruction.new(i::RETURN)]
      end


      code << Instruction.new(i::CALL, offset: data) # call the nonterminal, in @data by fix_up_grammar
      code << Instruction.new(i::JUMP, offset: 1 + full_rule_code.size) # we are done: jump to the line after the grammar's coderam
      code += full_rule_code

      # Now close the CALL instructions.
      code.each_with_index do |instr, idx|
        next unless instr.op_code == CALL

        nonterminal = instr.offset # still symbolic
        start_line = start_line_of_nonterminal[nonterminal]
        raise "Nonterminal #{nonterminal} does not have a rule in grammar" unless start_line

        # We replaced OPEN_CALL with CALL earlier in #fix_up_grammar. But, if the following instruction is a :return this a tail
        # call and we can eliminate the stack push by using a :jump instead of the call. The following :return must remain, as we
        # may reach there via another jump/commit/etc
        offset = start_line - idx
        dec = "->#{nonterminal}"
        code[idx] = if code[idx + 1] && code[idx + 1].op_code == :return
                      Instruction.new(i::JUMP, offset:, dec:)
                    else
                      Instruction.new(i::CALL, offset:, dec:)
                    end
      end
    else
      raise "Unhandled pattern type #{type}"
    end

    @code = code.freeze
  end

  # LPEG's peephole (lpcode.c)
  #
  # /*
  # ** Optimize jumps and other jump-like instructions.
  # ** * Update labels of instructions with labels to their final
  # ** destinations (e.g., choice L1; ... L1: jmp L2: becomes
  # ** choice L2)
  # ** * Jumps to other instructions that do jumps become those
  # ** instructions (e.g., jump to return becomes a return; jump
  # ** to commit becomes a commit)
  # */
  def optimize_jumps(program)
    i = Instruction # shorthand

    program.each_with_index do |instr, idx|
      case instr.op_code
      when i::CHOICE, i::CALL, i::COMMIT, i::PARTIAL_COMMIT, i::BACK_COMMIT
        n_off = finallabel(program, idx) - idx
        instr.offset = n_off
      when i::JUMP
        final_t = finaltarget(program, idx)
        case program[final_t].op_code
        when i::RETURN, i::FAIL, i::FAIL_TWICE, i::OP_END
          # instructions with unconditional implicit jumps. The jump just becomes that instruction
          program[idx] = program[final_t]
        when i::COMMIT, i::PARTIAL_COMMIT, i::BACK_COMMIT
          # instruction with unconditional explicit jumps
          final_final_t = finallabel(program, final_t)
          instr = program[final_t].clone # The jump becomes that instruction...
          instr.offset = final_final_t - idx # ... but we must correct the offset
          program[idx] = instr
          redo # "reoptimize the label"
        else
          # just optimize the label
          program[idx].offset = final_t - idx
        end
      end
    end
  end

  # LPEG's target (lpcode.c)
  #
  # The absolute target of the instruction at index idx
  def target(program, idx)
    idx + program[idx].offset
  end

  # LPEG's finaltarget (lpcode.c)
  #
  # Find the final [absolute] destination of a sequence of jumps
  def finaltarget(program, idx)
    idx = target(program, idx) while program[idx].op_code == Instruction::JUMP
    idx
  end

  # LPEG's finallabel (lpcode.c)
  #
  # final label (after traversing any jumps)
  def finallabel(program, idx)
    finaltarget(program, target(program, idx))
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
      data.must_be_a Set

      # assume we only worry about 8-bit ascii characters. Note that empty set should have been converted to NFALSE and singletons
      # to CHAR.
      data.size.must_be_in(2..255)
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
    when RUNTIME
      data.must_be_a(Proc)
    when CHAR
      data.must_be_a(String)
      data.length.must_be 1
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
    def check_pred(pattern, pred)
      raise "Bad check predicate #{pred}" unless CHECK_PREDICATES.include?(pred)

      # loop to eliminate some tail calls, as in the LPEG code. I don't think it's really necessary - my implementation is not going
      # to be fast overall - but let's try a new technique.
      loop do
        case pattern.type
        when CHAR, CHARSET, ANY, OPEN_CALL, NFALSE
          # Not nullable; for open_call this is a blind assumption
          return false
        when NTRUE, REPEATED
          return true
        when NOT, BEHIND
          # can match empty, but can fail
          return (pred != :nofail)
        when AND
          # can match empty; can fail exactly when body can
          return true if pred == :nullable

          pattern = pattern.child
        when RUNTIME
          # can fail; match empty iff body does
          return false if pred == :nofail

          pattern = pattern.child
        when SEQ
          return false unless check_pred(pattern.left, pred)

          pattern = pattern.right
        when ORDERED_CHOICE
          return true if check_pred(pattern.left, pred)

          pattern = pattern.right
        when GRAMMAR
          # Strings are matched by the initial nonterminal
          first_rule = pattern.child.first
          pattern = first_rule.child
        when CALL, RULE, CAPTURE
          # The call's rule, rule's pattern, and capture's pattern are in child
          pattern = pattern.child
        else
          raise "Unhandled pattern type #{pattern.type}"
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
        when CHAR, CHARSET, ANY, NTRUE, NFALSE, BEHIND
          # no op
        when NOT, AND, REPEATED, CAPTURE, RUNTIME
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
      when CAPTURE, RUNTIME
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
    # We return nil if the node's matches are not all of the same length
    def fixed_len(node)
      case node.type
      when CHARSET, CHAR
        1
      when ANY
        node.data
      when NOT, AND, NTRUE, NFALSE, BEHIND
        0
      when REPEATED, OPEN_CALL, RUNTIME
        nil
      when CAPTURE, RULE
        fixed_len(node.child)
      when GRAMMAR
        fixed_len(node.child.first) # the first rule is the initial nonterminal
      when CALL
        call_recursive(node, ->(n) { fixed_len(n) }, nil)
      when SEQ
        left_len = fixed_len(node.left)
        return nil unless left_len

        right_len = fixed_len(node.right)
        return nil unless right_len

        left_len + right_len
      when ORDERED_CHOICE
        left_len = fixed_len(node.left)
        return nil unless left_len

        right_len = fixed_len(node.right)
        right_len == left_len ? right_len : nil
      else
        raise "Unhandled node type #{node.type}"
      end
    end
  end

  ########################################
  # Experimental monkeypatching
  #
  # Very annoyingly, Ruby's #coerce mechanism is only used by the Numeric types. This means it doesn't help with things like the
  # convenient "a" + Pattern.P(true). The only way I can think to make it work is to monkeypatch.

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
