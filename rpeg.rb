# frozen_string_literal: true

# See the README file for a little context. And also:
#
#   http://www.inf.puc-rio.br/~roberto/lpeg/#func

require 'set'
require 'must_be'

MustBe.disable

require_relative 'captures'
require_relative 'parsing_machine'

# This is intended to play the same role as LPEG's lpeg module.
#
# Top-level differences from LPEG:
#
# - AND patterns in LPEG are written as #patt (&patt in the first version) but +patt here
#   - unary & apparently can't be overloaded in Ruby
#     - I tried using the "&" operator by overriding #to_proc but the Ruby parser rejects the &patt expression.
#   - +patt doesn't read well, even though -patt does. I think this is because binary plus is so much more common when buliding
#     patterns than binary minus is.
#     - There doesn't seem to be another workable option. According to https://stackoverflow.com/a/21060235/1299011 the unary
#       operators are !, ~, +, and -. We use - already and ! needs to be avoided because of idiomiatic Ruby checks like !foo for
#       existence. Operator ~ works, but that character is easy to mistake for a - when reading, which is bad. Ruby uses & as a
#       unary (for to_proc) but the Ruby parser doesn't allow its use in general. So I think we're stuck with +.
#
# - repeating patterns still use exponentiation, but it now looks like patt**n rather than patt^n because of Ruby's syntax
#
# - grammars are represented by hashes or arrays. LPEG uses Lua tables (which are mashups of hashtables and arrays)
#
#   If an array is given then the nonterminals aren't named and all open calls must use numeric indices. The first element of the
#   array is either
#   - a non-negative integer 0, 1, 2, ... and specifies the (rule of the) initial nonterminal among the remaining elements with
#     indices reckoned _without_ that initial integer
#   - something else, which is interpreted as the pattern for the initial nonterminal
#
#   Otherwise the grammar is defined with a Hash. The keys are the nonterminal symbols and the values are the rule patterns.
#   - the keys must be symbols or strings (which are converted to symbols). No rule can use :initial or "initial" as
#     nonterminal.
#   - the open calls can refer either to the nonterminals (as strings or symbols) or to rule indices as they appear in the hash,
#     ignoring the :initial key (if present)
#   - :initial/"initial" can appear as a key in the hash and its value specifies the initial nonterminal.
#     - if it is a non-zero integer it gives the index of the initial terminal's rule, reckoned without the presence of the :initial
#       key itself.
#     - if it is a symbol or a string it specifies the initial nonterminal directly
#
# - "Table" captures return an instace of TableCapture, which impelements a little bit of a Lua's table functionality
#   - other formats haven't worked out well
#
# Function captures
#
# Various kinds of captures involve calling a function (proc) provided by client code. For example, the construction (patt / fn)
# takes the captures made by patt and passes them as arguments to fn. Then the values returned by fn become the captures of the
# expression. Lua is better than Ruby at distinguishing between a function that returns multiple values and one that returns a
# single value that is an array. In RPEG, returns from function in contexts like this are treated as follows:
#
#   - [1, 2, 3]: multiple captures, 1, 2, 3.
#     - this is the natural interpretation as it's the standard way that a Ruby function returns multiple values
#   - [[1, 2, 3]]: a single capture that is an array
#   - nil: no captures
#     - even if the function says something like "return nil", the capture code has no way to distinguish between that and a
#       function that returns nothing
#   - [nil]: a single capture with value nil
#     - the weirdest case, but I don't see an alternative
#
# TODO:
# - program generation optimations
#   - other pattern-based optimizations: need to scan through the LPEG code again
#     - I think I've done them all now
#   - profiling
# - LPEG's locale support
#   - what would this look like in Ruby?
module RPEG
  extend self

  # Match any character in string (regarded as a set of characters), range, or Set
  #
  # If the set is empty we have NFALSE, which always fails
  # If the set has a single element we have CHAR pattern, which is a little faster in the VM
  # Otherwise we have a CHARSET pattern
  def S(charset)
    case charset
    when Set
      size = charset.size
      return P(false) if size.zero?
      return P(charset.first) if size == 1
      return P(1) if charset == Pattern::FULL_CHAR_SET

      Pattern.new(Pattern::CHARSET, data: charset)
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
      arg.chars.reverse.each do |ch|
        patt = Pattern.new(Pattern::CHAR, data: ch) * patt
      end
      patt
    when Integer
      # When n >= 0, match at least n chars.
      # When n < 0, there must not be n or more characters left
      return -P(-arg) if arg.negative?

      # In LPEG the ANY VM instruction takes no arg and matches a single char, unlike the description in the paper. I think it
      # makes certain code optimizations simpler to analyze. We do the same.
      patt = P(true)
      arg.times do
        patt = Pattern.new(Pattern::ANY) * patt
      end
      patt
    when FalseClass
      @false_tree ||= Pattern.new(Pattern::NFALSE)
    when TrueClass
      @true_tree ||= Pattern.new(Pattern::NTRUE)
    when Hash, Array
      Pattern.new(Pattern::GRAMMAR, data: arg)
    when Proc
      # a pattern equivalent to a match-time capture over the empty string.
      Pattern.new(Pattern::RUNTIME, P(true), data: arg)
    else
      raise "RPEG.P does not support argument #{arg}"
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
  #  - a value that will be the key in the final grammar - a Hash or Array - of the rule being referenced
  #    - strings are turned into symbols
  def V(ref)
    ref = ref.to_sym if ref.is_a?(String)
    Pattern.new(Pattern::OPEN_CALL, data: ref)
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
    return pattern if pattern.type == Pattern::CAPTURE && pattern.capture == Capture::SIMPLE

    Pattern.new(Pattern::CAPTURE, pattern, capture: Capture::SIMPLE)
  end

  # Capture the n-th extra argument provided to #match. The first extra argument is n=1, etc.
  #
  # We accept a missing argument to match some LPEG test cases but an error is raised
  def Carg(num = nil)
    raise "Invalid argument for Carg: #{num || 'nil'}" unless num.is_a?(Integer) && num&.positive?

    Pattern.new(Pattern::CAPTURE, P(true), data: num, capture: Capture::ARGUMENT)
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

    Pattern.new(Pattern::CAPTURE, P(true), data: name, capture: Capture::BACKREF)
  end

  # LPEG: Creates a constant capture. This pattern matches the empty string and produces all given values as its captured values.
  #
  # No value at all - Cc() - adds nothing to the result, which is different from a value of nil.
  #
  # We capture several values with individual captures.
  def Cc(*values)
    return P(true) if values.empty?

    patt = Pattern.new(Pattern::CAPTURE, P(true), data: values.first, capture: Capture::CONST)
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

    Pattern.new(Pattern::CAPTURE, P(pattern), data: lambda, capture: Capture::FOLD)
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
    Pattern.new(Pattern::CAPTURE, P(pattern), data: name, capture: Capture::GROUP)
  end

  # LPEG: Creates a position capture. It matches the empty string and captures the position in the subject where the match
  # occurs. The captured value is a number.
  def Cp
    Pattern.new(Pattern::CAPTURE, P(true), capture: Capture::POSITION)
  end

  # From LPEG:
  #
  #   Creates a substitution capture, which captures the substring of the subject that matches patt, with substitutions. For any
  #   capture inside patt with a value, the substring that matched the capture is replaced by the capture value (which should be a
  #   string). The final captured value is the string resulting from all replacements.
  def Cs(patt)
    Pattern.new(Pattern::CAPTURE, P(patt), capture: Capture::SUBST)
  end

  # From LPEG:
  #
  #   Creates a table capture. This capture returns a table with all values from all anonymous captures made by patt inside this
  #   table in successive integer keys, starting at 1. Moreover, for each named capture group created by patt, the first value of
  #   the group is put into the table with the group name as its key. The captured value is only the table.
  #
  # For us the capture takes the form of a custom class, TableCapture. It is intended to mimic a little bit of the functionality
  # of Lua's tables, which are a combination Array/Hashtable
  # - indexing is by 0, 1, 2, ... for the anonmyous catpures
  # - indexing by group names otherwise
  # - #unpack gives an arry of the anonymous captures.
  #
  # See the class definition (captures.rb) for more details.
  #
  # Other things tried:
  # - returning a hash when there are named captures and an array when there are not
  #   - in the hash, anonmyous captures are at keys 0, 1, 2, ...
  #   - this turned out to be somewhat frustrating in unit tests.
  # - returning a hash with group names as keys and a special key of :anon for the array of anonmyous captures
  #   - I thought this would work better, but actually turned out to be much worse to work with
  def Ct(patt)
    Pattern.new(Pattern::CAPTURE, P(patt), capture: Capture::TABLE)
  end

  # From LPEG:
  #
  #   Creates a match-time capture. Unlike all other captures, this one is evaluated immediately when a match occurs (even if it
  #   is part of a larger pattern that fails later). It forces the immediate evaluation of all its nested captures and then calls
  #   function.
  #
  #   The given function gets as arguments the entire subject, the current position (after the match of patt), plus any capture
  #   values produced by patt.
  #
  #   The first value returned by function defines how the match happens. If the call returns a number, the match succeeds and the
  #   returned number becomes the new current position. (Assuming a subject s and current position i, the returned number must be
  #   in the range [i, len(s) + 1].) If the call returns true, the match succeeds without consuming any input. (So, to return true
  #   is equivalent to return i.) If the call returns false, nil, or no value, the match fails.
  #
  #   Any extra values returned by the function become the values produced by the capture.
  def Cmt(patt, function)
    # LPEG uses a separate RUNTIME node type instead of CAPTURE because certain functions, like hascaptures and fixedlen, need
    # special behavior here. Note
    #
    # LPEG also uses "runtime" interally instead of "matchtime". We follow
    Pattern.new(Pattern::RUNTIME, P(patt), data: function)
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
    Pattern.new(Pattern::BEHIND, patt, data: len)
  end

  # See the instance method #match for the arguments
  def match(thing, string, init = 0, *extra_args)
    P(thing).match(string, init, *extra_args)
  end

  # The class representing "patterns" and containing the logic to turn them into programs for the virtual machine.
  #
  # Very roughly, this is where the LPEG code in lptree.c and lpcode.c lives
  class Pattern
    NODE_TYPES = %i[
      charset char any seq ordered_choice repeated not and
      ntrue nfalse grammar open_call rule call capture runtime behind
    ].each do |op|
      const_set op.upcase, op
    end

    # We assume we have UTF-8 input with no multibyte characters.
    FULL_CHAR_SET = Set.new((0..255).map{ _1.chr(Encoding::UTF_8) })

    attr_reader :type, :left, :right, :capture
    attr_accessor :data # sometimes we need to tweak this

    # Return the index just after the matching prefix of str or nil if there is no match
    #
    # str: the string the match against
    # init: the string index to start at, defaulting to 0
    # extra_args: used by Argument Captures
    def match(str, init = 0, *extra_args)
      # Note that the program doesn't depend on the arguments so we can cache it
      @program ||= optimize_jumps(code(follow_set: FULL_CHAR_SET) + [Instruction.new(i::OP_END)])

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
      [RPEG.P(other), self]
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

      if charsetlike? && other.charsetlike?
        # Take the union of the charsets
        RPEG.S(charset + other.charset)
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
        patt = RPEG.P(true)
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

      return RPEG.S(charset - other.charset) if charsetlike? && other.charsetlike?

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
    #     the character % works as an escape character: any sequence in string of the form %n, with n between 1 and 9, stands for
    #     the match of the n-th capture in patt. The sequence %0 stands for the whole match. The sequence %% stands for a single %.
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
    #     if patt made no capture. The values returned by the function are the final values of the capture. In particular, if
    #     function returns no value, there is no captured value.
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

    def fix_type(other)
      return other if other.is_a?(Pattern)

      RPEG.P(other) # see what we can do
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
      when RULE
        result << "nonterminal: #{data}"
      when REPEATED, NOT, AND, BEHIND
        result << "#{type_s}: "
        do_sub_pattern.call(child)
      when CAPTURE
        result << "Capture: #{capture} #{data.inspect}"
        do_sub_pattern.call(child)
      when RUNTIME
        result << "Runtime: #{capture} #{data.inspect}"
        do_sub_pattern.call(child)
      when GRAMMAR
        result << "Grammar:"
        first = true
        child.each do |nonterminal, rule_pattern|
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

    def charsetlike?
      type == CHARSET || type == CHAR || type == ANY
    end

    def charset
      raise "Pattern #{type} isn't charset-like" unless charsetlike?

      case type
      when CHARSET
        data
      when CHAR
        Set.new([data])
      when ANY
        FULL_CHAR_SET
      end
    end

    def nullable?
      return @nullable if defined? @nullable

      @nullable = check_pred(:nullable)
    end

    def nofail?
      return @nofail if defined? @nofail

      @nofail = check_pred(:nofail)
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
    # ** The function assumes that TOpenCall is not nullable; this will be checked again when the grammar is fixed.
    #
    # ** Run-time captures can do whatever they want, so the result is conservative.
    # */
    def check_pred(pred)
      raise "Bad check predicate #{pred}" unless %i[nullable nofail].include?(pred)

      case type
      when CHAR, CHARSET, ANY, OPEN_CALL, NFALSE
        # Not nullable; for open_call this is a blind assumption
        false
      when NTRUE, REPEATED
        true
      when NOT, BEHIND
        # can match empty, but can fail
        pred != :nofail
      when AND
        # can match empty; can fail exactly when body can
        return true if pred == :nullable

        child.check_pred(pred)
      when RUNTIME
        # can fail; match empty iff body does
        return false if pred == :nofail

        child.check_pred(pred)
      when SEQ
        left.check_pred(pred) && right.check_pred(pred)
      when ORDERED_CHOICE
        left.check_pred(pred) || right.check_pred(pred)
      when GRAMMAR
        # Strings are matched by the initial nonterminal
        child.first.check_pred(pred)
      when CALL, RULE, CAPTURE
        child.check_pred(pred)
      else
        raise "Unhandled pattern type #{type}"
      end
    end

    # fixedlen from LPEG's lpcode.h
    #
    # /*
    # ** number of characters to match a pattern (or -1 if variable)
    # */
    #
    # We return nil if the node's matches are not all of the same length
    def fixed_len
      case type
      when CHARSET, CHAR, ANY
        1
      when NOT, AND, NTRUE, NFALSE, BEHIND
        0
      when REPEATED, OPEN_CALL, RUNTIME
        nil
      when CAPTURE, RULE
        child.fixed_len
      when GRAMMAR
        child.first.fixed_len # the first rule is the initial nonterminal
      when CALL
        call_recursive(:fixed_len, nil)
      when SEQ
        left_len = left.fixed_len
        return nil unless left_len

        right_len = right.fixed_len
        return nil unless right_len

        left_len + right_len
      when ORDERED_CHOICE
        left_len = left.fixed_len
        return nil unless left_len

        right_len = right.fixed_len
        right_len == left_len ? right_len : nil
      else
        raise "Unhandled node type #{type}"
      end
    end

    # From checkloops in lptree.c
    #
    # /*
    # ** Check whether a tree has potential infinite loops
    # */
    def loops?
      return true if type == REPEATED && child.nullable?

      # /* sub-grammars already checked */, i.e., by verify_grammar
      return false if type == GRAMMAR || type == CALL

      case num_children
      when 1
        child.loops?
      when 2
        left.loops? || right.loops?
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
    # This method acts as a circuit breaker for structural recursion that might otherwise get in a loop among mutually recursive
    # grammar rules.
    #
    # It's janky, but we follow LPEG's approach of hijacking the key field (which we call data) to keep track of the recursion
    def call_recursive(func, default)
      type.must_be CALL
      child.type.must_be RULE

      already_visited = :already_visited

      saved_data = @data

      if saved_data == already_visited
        default
      else
        # first time we've been here
        @data = already_visited
        result = send(func)
        @data = saved_data
        result
      end
    end

    # From hascaptures in LPEG's lpcode.c
    # /*
    # ** Check whether a pattern tree has captures
    # */
    def has_captures?
      case type
      when CAPTURE, RUNTIME
        true
      when CALL
        call_recursive(:has_captures?, false)
      when GRAMMAR
        child.any?(&:has_captures?)
      else
        case num_children
        when 0
          false
        when 1
          child.has_captures?
        when 2
          left.has_captures? || right.has_captures?
        end
      end
    end

    # LPEG's getfirst
    #
    # static int getfirst (TTree *tree, const Charset *follow, Charset *firstset) {
    #
    # /*
    # ** Computes the 'first set' of a pattern.
    # ** The result is a conservative aproximation:
    # **   match p ax -> x (for some x) ==> a belongs to first(p)
    # ** or
    # **   a not in first(p) ==> match p ax -> fail (for all x)
    # [So we want to know the set of characters that can make the pattern succeed, at least on the first characters]
    # **
    # ** The set 'follow' is the first set of what follows the
    # ** pattern (full set if nothing follows it).
    # **
    # ** The function returns 0 when this resulting set can be used for
    # ** test instructions that avoid the pattern altogether.
    # ** A non-zero return can happen for two reasons:
    # ** 1) match p '' -> ''            ==> return has bit 1 set
    # ** (tests cannot be used because they would always fail for an empty input);
    # ** 2) there is a match-time capture ==> return has bit 2 set
    # ** (optimizations should not bypass match-time captures).
    # */
    #
    # I don't really understand what is going on here. I'm hoping it will make more sense as I port it. I think we pass in follow
    # and return the int and firstset.
    def first_set(follow_set = FULL_CHAR_SET)
      case type
      when CHAR, CHARSET, ANY
        [0, charset]
      when NTRUE
        [1, follow_set.clone] # /* accepts the empty string */
      when NFALSE
        [0, Set.new]
      when ORDERED_CHOICE
        e1, first1 = left.first_set(follow_set)
        e2, first2 = right.first_set(follow_set)
        [e1 | e2, first1 | first2]
      when SEQ
        if !left.nullable?
          # /* when p1 is not nullable, p2 has nothing to contribute;
          #  return getfirst(sib1(tree), fullset, firstset); */
          left.first_set(FULL_CHAR_SET)
        else
          e2, first2 = right.first_set(follow_set)
          e1, first1 = left.first_set(first2)
          return [0, first1] if e1.zero? # /* 'e1' ensures that first can be used */
          return [2, first1] if (e1 | e2) & 2 == 2 # /* one of the children has a matchtime? */

          [e2, first1] # /* else depends on 'e2' */
        end
      when REPEATED
        _, first_cs = child.first_set(follow_set)
        [1, first_cs | follow_set] # /* accept the empty string */
      when CAPTURE, RULE
        child.first_set(follow_set)
      when GRAMMAR
        child.first.first_set(follow_set)
      when RUNTIME
        # NOTE: I don't understand this
        #
        # /* function invalidates any follow info. */
        e, first_set = child.first_set(FULL_CHAR_SET)
        if e.positive?
          [2, first_set] # /* function is not "protected"? */
        else
          [0, first_set] # /* pattern inside capture ensures first can be used */
        end
      when CALL
        child.first_set(follow_set)
      when AND
        e, first_set = child.first_set(follow_set)
        [e, first_set & follow_set]
      when NOT, BEHIND
        if type == NOT && child.charsetlike?
          [1, FULL_CHAR_SET - child.charset]
        else
          # /* instruction gives no new information */
          # /* call 'getfirst' only to check for math-time captures */
          e, = child.first_set(follow_set)
          [e | 1, follow_set] # /* always can accept the empty string */
        end
      else
        raise "Unhandled node type #{type}"
      end
    end

    # LPEG's headfail
    #
    # /*
    # ** If 'headfail(tree)' true, then 'tree' can fail only depending on the
    # ** next character of the subject.
    # */
    def head_fail?
      case type
      when CHAR, CHARSET, ANY, NFALSE
        true
      when NTRUE, REPEATED, RUNTIME, NOT
        false
      when CAPTURE, RULE, AND, CALL
        child.head_fail?
      when GRAMMAR
        child.first.head_fail?
      when SEQ
        return false unless right.nofail?

        left.head_fail?
      when ORDERED_CHOICE
        left.head_fail? && right.head_fail?
      else
        raise "Unhandled node type #{type}"
      end
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

    # shorthand
    def i
      Instruction
    end

    # follow_set
    # - the set of first characters accepted by whatever comes after us, or the full set of characters if nothing follows us
    #
    # dominating_test
    # - a TEST_CHAR, TEST_CHARSET, or TEST_ANY instruction that we can assume has succeeded and which might save us a little time
    # - see the tt argument sprinkled through the functions in LPEG's lpcode.c
    #
    # active_option
    # - there is a CHOICE instruction still "active" in the code already generated
    # - certain pattern types can take advantange of this to avoid another CHOICE instruction
    # - this is called 'opt' in lpcode.c
    #
    # NOTE: don't cache the results as we did before, because the code depends on the arguments
    def code(follow_set: FULL_CHAR_SET, dominating_test: nil, active_choice: false)
      code = []
      case type
      when CHARSET
        code << charset_code(data, dominating_test)
      when CHAR
        code << char_code(data, dominating_test)
      when ANY
        code << Instruction.new(i::ANY)
      when SEQ
        code.concat seq_code(follow_set, dominating_test)
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
        code.concat choice_code(follow_set, active_choice)
      when REPEATED
        code.concat repeated_code(follow_set, active_choice)
      when NOT
        code.concat not_code
      when AND
        code.concat and_code(dominating_test)
      when BEHIND
        code << Instruction.new(i::BEHIND, aux: data) if data.positive?
        code.concat child.code
      when CAPTURE
        c = child.code(follow_set:, dominating_test:)
        len = fixed_len
        if len && !child.has_captures?
          code.concat c
          code << Instruction.new(i::FULL_CAPTURE, data:, aux: { capture_length: len, kind: capture })
        else
          code << Instruction.new(i::OPEN_CAPTURE, data:, aux: { kind: capture })
          code.concat c
          code << Instruction.new(i::CLOSE_CAPTURE, aux: { kind: Capture::CLOSE })
        end
      when RUNTIME
        code << Instruction.new(i::OPEN_CAPTURE, data:, aux: { kind: Capture::GROUP })
        code.concat child.code(follow_set: FULL_CHAR_SET, dominating_test:)
        code << Instruction.new(i::CLOSE_RUN_TIME, aux: { kind: Capture::CLOSE })
      when RULE
        code = child.code(follow_set:)
        code[0] = code.first.clone
        code.first.dec = data # decorate with the nonterminal, but clone first to avoid unexpected mutations
      when GRAMMAR
        code.concat grammar_code
      else
        raise "Unhandled pattern type #{type}"
      end

      code
    end

    # LPEG's codeseq1
    #
    # /*
    # ** Code first child of a sequence
    # ** (second child is called in-place to allow tail call)
    # ** Return 'tt' for second child
    # */
    #
    # We do both parts of the sequence as we don't do TCO
    private def seq_code(follow_set, dominating_test)
      code = []
      if left.need_follow?
        _, follow_set1 = right.first_set(follow_set)
        code = left.code(follow_set: follow_set1, dominating_test:)
      else
        code = left.code(dominating_test:)
      end
      if left.fixed_len != 0
        # /* can 'p1' consume anything? */
        dominating_test = nil # /* invalidate test */
        # /* else 'tt' still protects sib2 */
      end
      code.concat right.code(follow_set:, dominating_test:)
      code
    end

    # LPEG's codechoice (lpcode.c)
    #
    # /*
    # ** Choice; optimizations:
    # ** - when p1 is headfail or
    # ** when first(p1) and first(p2) are disjoint, than
    # ** a character not in first(p1) cannot go to p1, and a character
    # ** in first(p1) cannot go to p2 (at it is not in first(p2)).
    # ** (The optimization is not valid if p1 accepts the empty string,
    # ** as then there is no character at all...)
    # ** - when p2 is empty and opt is true; a IPartialCommit can reuse
    # ** the Choice already active in the stack.
    # */
    private def choice_code(follow_set, active_choice)
      raise "Not an ORDERED_CHOICE pattern" unless type == ORDERED_CHOICE

      code = []
      right_empty = (right.type == NTRUE)
      e1, left_first_set = left.first_set
      if left.head_fail? ||
         (e1.zero? && ((_, right_first_set = right.first_set(follow_set)) && left_first_set.disjoint?(right_first_set)))

        # We can optimize here. See the comment at ParsingMachine#test_char about how the LPEG approach (which we copy) differs from
        # what is described in Ierusalimschy's paper.
        test = testset_code(left_first_set)

        left_code = left.code(follow_set:, dominating_test: test)
        offset = 1 + left_code.size
        offset += 1 unless right_empty

        test.offset = offset

        code << test
        code.concat left_code
        unless right_empty
          right_code = right.code(follow_set:, active_choice:)
          code << Instruction.new(i::JUMP, offset: 1 + right_code.size)
          code.concat right_code
        end
      elsif active_choice && right_empty
        code << Instruction.new(i::PARTIAL_COMMIT, 1)
        code.concat child.code(active_choice: true)
      else
        test = testset_code(left_first_set) if e1.zero?

        p1 = left.code(dominating_test: test, active_choice: right_empty)
        p2 = right.code(follow_set:, active_choice:)

        if test
          test.offset = 3 + p1.size
          code << test
        end

        code << Instruction.new(i::CHOICE, offset: 2 + p1.size)
        code.concat p1
        code << Instruction.new(i::COMMIT, offset: 1 + p2.size)
        code.concat p2
      end
      code
    end

    # LPEG's coderep (lpcode.c)
    # /*
    # ** Repetion; optimizations:
    # ** When pattern is a charset, can use special instruction ISpan.
    # ** When pattern is head fail, or if it starts with characters that
    # ** are disjoint from what follows the repetions, a simple test
    # ** is enough (a fail inside the repetition would backtrack to fail
    # ** again in the following pattern, so there is no need for a choice).
    # ** When 'opt' is true, the repetion can reuse the Choice already
    # ** active in the stack.
    # */
    private def repeated_code(follow_set, active_choice)
      raise "Not a REPEATED pattern" unless type == REPEATED

      # Special, quicker handling when the thing we are repeated over is a charset. See Ierusalimschy 4.3
      return [Instruction.new(i::SPAN, data: child.charset)] if child.charsetlike?

      code = []
      e1, first_set = child.first_set(follow_set)
      if child.head_fail? || (e1.zero? && first_set.disjoint?(follow_set))
        test = testset_code(first_set)
        p = child.code(dominating_test: test)
        test.offset = 2 + p.size
        code << test
        code.concat p
        code << Instruction.new(i::JUMP, offset: -(1 + p.size))
      else
        p = child.code
        code << testset_code(first_set, 3 + p.size) if e1.zero?
        if active_choice
          code << Instruction.new(i::PARTIAL_COMMIT, 1)
        else
          code << Instruction.new(i::CHOICE, offset: 2 + p.size)
        end
        code.concat p
        code << Instruction.new(i::PARTIAL_COMMIT, offset: -p.size)
      end
      code
    end

    private def grammar_code
      raise "Not a GRAMMAR pattern" unless type == GRAMMAR

      code = []
      start_line_of_nonterminal = {}
      full_rule_code = []

      # we need to put the initial nonterminal's rules first
      initial_rule = child.find { |rule| rule.data == data }
      raise "Cannot find initial rule, for #{data}" unless initial_rule

      the_rules = [initial_rule] + child.find.reject { |r| r == initial_rule }

      the_rules.each do |rule|
        nonterminal = rule.data
        start_line_of_nonterminal[nonterminal] = 2 + full_rule_code.size
        full_rule_code.concat rule.code(follow_set: FULL_CHAR_SET) + [Instruction.new(i::RETURN)]
      end

      code << Instruction.new(i::CALL, offset: data) # call the nonterminal, in @data by fix_up_grammar
      code << Instruction.new(i::JUMP, offset: 1 + full_rule_code.size) # we are done: jump to the line after the grammar's code
      code.concat full_rule_code

      # Now close the CALL instructions.
      code.each_with_index do |instr, idx|
        next unless instr.op_code == CALL

        nonterminal = instr.offset # still symbolic
        next if nonterminal.is_a?(Integer) # ... expect when we're in a subgrammar, since it has already been fixed up.

        start_line = start_line_of_nonterminal[nonterminal]
        raise "Nonterminal #{nonterminal} does not have a rule in grammar" unless start_line

        # We replaced OPEN_CALL with CALL earlier in #fix_up_grammar. But, if the following instruction is a :return this a tail
        # call and we can eliminate the stack push by using a :jump instead of the call. The following :return must remain, as we
        # may reach there via another jump/commit/etc
        offset = start_line - idx
        dec = "->#{nonterminal}"
        code[idx] = if code[finaltarget(code, idx + 1)]&.op_code == :return
                      Instruction.new(i::JUMP, offset:, dec:)
                    else
                      Instruction.new(i::CALL, offset:, dec:)
                    end
      end
    end

    # LPEG's codeand (lpcode.c)
    # /*
    # ** And predicate
    # ** optimization: fixedlen(p) = n ==> <&p> == <p>; behind n
    # ** (valid only when 'p' has no captures)
    # */
    private def and_code(dominating_test)
      code = []
      p = child.code(dominating_test:)
      len = child.fixed_len
      if len && !child.has_captures?
        code.concat p
        code << Instruction.new(i::BEHIND, aux: len, dec: :and) if len.positive?
      else
        code << Instruction.new(i::CHOICE, offset: 2 + p.size)
        code.concat p
        code << Instruction.new(i::BACK_COMMIT, offset: 2)
        code << Instruction.new(i::FAIL)
      end
      code
    end

    # LPEG's codenot (lpcode.c)
    #
    # /*
    # ** Not predicate; optimizations:
    # ** In any case, if first test fails, 'not' succeeds, so it can jump to
    # ** the end. If pattern is headfail, that is all (it cannot fail
    # ** in other parts); this case includes 'not' of simple sets. Otherwise,
    # ** use the default code (a choice plus a failtwice).
    # */
    private def not_code
      code = []
      e, first_set = child.first_set
      if e.zero? && child.head_fail?
        code << testset_code(first_set, 2)
        code << Instruction.new(i::FAIL)
      else
        p = child.code

        code << Instruction.new(i::CHOICE, offset: 2 + p.size)
        code.concat p
        code << Instruction.new(i::FAIL_TWICE)
      end
      code
    end

    # LPEG's codetestset (lpcode.c)
    #
    # /*
    # ** code a test set, optimizing unit sets for ITestChar, "complete"
    # ** sets for ITestAny, and empty sets for IJmp (always fails).
    # ** 'e' is true iff test should accept the empty string. (Test
    # ** instructions in the current VM never accept the empty string.)
    # */
    #
    # first_set is the set of first-chars that we want to match on.
    # Offset is where to jump to if we don't match one of them.
    #
    # If offset is not given we don't mind: client code is responsible for that.
    private def testset_code(first_set, offset = nil)
      case first_set.size
      when 0
        Instruction.new(i::JUMP, offset:) # we will always fail, so just jump
      when 1
        Instruction.new(i::TEST_CHAR, offset:, data: first_set.first)
      when FULL_CHAR_SET.size
        Instruction.new(i::TEST_ANY, offset:)
      else
        Instruction.new(i::TEST_CHARSET, offset:, data: first_set)
      end
    end

    private def charset_code(charset, dominating_test)
      if charset.size == 1
        char_code(charset.first, dominating_test)
      elsif dominating_test&.op_code == i::TEST_CHARSET && dominating_test&.data == charset
        # the "dominating test" has already checked for us so we can use ANY, which is quicker
        Instruction.new(i::ANY)
      else
        Instruction.new(i::CHARSET, data: charset)
      end
    end

    private def char_code(char, dominating_test)
      if dominating_test&.op_code == i::TEST_CHAR && dominating_test&.data == char
        Instruction.new(i::ANY)
      else
        Instruction.new(i::CHAR, data: char)
      end
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
    private def optimize_jumps(program)
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
            program[idx] = program[final_t].clone
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

    # LPEG's needfollow (lpcode.c)
    #
    # /*
    # ** Check whether the code generation for the given tree can benefit
    # ** from a follow set (to avoid computing the follow set when it is
    # ** not needed)
    # */
    def need_follow?
      case type
      when CHAR, CHARSET, ANY, NFALSE, NTRUE, AND, NOT, RUNTIME, GRAMMAR, CALL, BEHIND
        false
      when ORDERED_CHOICE, REPEATED
        true
      when CAPTURE
        child.need_follow?
      when SEQ
        right.need_follow?
      else
        raise "Unhandled case #{type} in need_follow?"
      end
    end

    private def verify_grammar
      raise "Not a grammar!" unless type == GRAMMAR

      # /* check for infinite loops inside rules */
      child.each do |rule|
        rule.verify_rule
        raise "Grammar has potential infinite loop in rule '#{rule.data}'" if rule.loops?
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
    def verify_rule
      raise "verify_rule called on something that isn't a rule" unless type == RULE

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

      local_rec.call(self, 0)
    end

    # LPEG's target (lpcode.c)
    #
    # The absolute target of the instruction at index idx
    private def target(program, idx)
      idx + program[idx].offset
    end

    # LPEG's finaltarget (lpcode.c)
    #
    # Find the final [absolute] destination of a sequence of jumps
    private def finaltarget(program, idx)
      idx = target(program, idx) while program[idx]&.op_code == i::JUMP
      idx
    end

    # LPEG's finallabel (lpcode.c)
    #
    # final label (after traversing any jumps)
    private def finallabel(program, idx)
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
      verify_grammar
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
      when NTRUE, NFALSE, ANY
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

        unless initial_nonterminal
          raise "Bad grammar: no rule correspnds to an index of #{initial_rule_dix} for initial nonterminal"
        end

        as_hash[:initial] = initial_nonterminal
        @data = as_hash
      end

      # Canonical representations of keys (symbols) and values (patterns)
      grammar_hash = {}
      data.each do |nonterminal, pattern|
        nonterminal = nonterminal.to_sym if nonterminal.is_a?(String)
        raise "Nonterminal symbol can be only a string or a symbol" unless nonterminal.is_a?(Symbol)

        next if nonterminal == :initial # the only case in which we don't specify a rule pattern

        grammar_hash[nonterminal] = RPEG.P(pattern)
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
  end

  ########################################
  # Monkeypatching
  #
  # Very annoyingly, Ruby's #coerce mechanism is only used by the Numeric types. This means it doesn't help with things like the
  # convenient "a" + P(-1). The only way I can think to make it work is to monkeypatch.

  # Technique from https://stackoverflow.com/a/61438012/1299011
  module NonNumericOverloadExtension
    %i[+ * -].each do |sym|
      define_method sym do |other|
        return RPEG.P(self).send(sym, other) if other.is_a?(Pattern)

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
