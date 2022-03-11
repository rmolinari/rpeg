# Code directly related to captures.
#
# In terms of LPEG is corresponds to lpcap.h and lpcap.c

# Capture-related data and functionality
module Capture
  KINDS = %i[const position argument simple group backref subst table fold string num query function close runtime].each do |kind|
    const_set kind.upcase, kind
  end

  # Used inside the VM when recording capture information.
  class Breadcrumb
    # From time to time we need to tweak each of these
    attr_accessor :size, :subject_index, :data, :kind

    # q.v. LPEG's Capture struct (lpcap.h)
    #
    # We use size instead of siz, subject_index instead of s, and data instead of idx.
    def initialize(size, subject_index, data, kind)
      raise "Bad Capture kind #{kind}" unless KINDS.include?(kind)

      @size = size
      @subject_index = subject_index
      @data = data
      @kind = kind
    end

    # An "open" capture is a "full capture" if it has non-zero size. See isfullcap in lpcap.c
    def full?
      @size.positive?
    end

    def close?
      @kind == CLOSE
    end

    # The index of the end of the match, q.v. LPEG's closeaddr (lpcap.h)
    def end_index
      @subject_index + size - 1
    end

    # Dynamic because of the setters we sometimes use
    def to_s
      "Breadcrumb size:#{size} sub_idx:#{subject_index} data:#{data.inspect} kind:#{kind}"
    end
  end

  # The result of a table capture. The idea is to mimic a little bit of the functionality of a Lua table which is like a combination
  # Array/Hash.
  #
  # Internally, we have a hash. Indexing can be by (hash) key or (array) index. Getting and setting is supported.
  #
  # The initial, contiguous segment of the array part (non nil values at 0, 1, 2, ..., k) is available from #unpack.
  class TableCapture
    def initialize(hash_part, array_part)
      @data = hash_part.clone
      array_part.each_with_index do |val, idx|
        @data[idx] = val
      end
    end

    # Let i be the smallest natural number such that self[i].nil?. We return [self[0], self[1], ..., self[i-1]]
    def unpack
      (0..).lazy.map { |key| @data[key] }.take_while { |v| !v.nil? }.force
    end

    def each
      @data.each
    end

    # Note that we say false if all keys are positive integers but 0 has no value (and so #unpack returns [])
    def empty?
      size.zero?
    end

    def size
      @data.size
    end

    def [](key)
      @data[key]
    end

    def []=(key, value)
      @data[key] = value
    end

    def delete(key)
      @data.delete(key)
    end

    # We support comparison with
    # - TableCapture, in which case we just compare the data objects
    # - Hash, in which case we check key-by-key
    # - Array, in which case we check index-by-index
    def ==(other)
      case other
      when TableCapture
        @data == other.instance_variable_get(:@data)
      when Hash
        @data == other
      when Array
        @data == other.each_with_index.to_a.map(&:reverse).to_h
      else
        raise "Bad type #{other.class} for =="
      end
    end

    # Very annoyingly, Ruby's #coerce mechanism is only used by the Numeric types. This means we can make convenient checks like
    # table_capture == {} but not {} == table_capture. The only approach I can think of is to monkeypatch Array and Hash.
    #
    # Maybe we shouldn't bother, and just cobble something together for these tests in contexts like unit test classes. It would be
    # nice to define a refinement and use it in a unit test file, but then, for example, assertion functions won't see the
    # refinement as they are defined elsewhere.

    # Technique from https://stackoverflow.com/a/61438012/1299011
    module ArrayHashOverloadExtension
      def ==(other)
        return (other == self) if other.is_a?(TableCapture)

        super
      end
    end

    [::Hash, ::Array].each do |klass|
      klass.class_eval do
        prepend ArrayHashOverloadExtension
      end
    end
  end
end

# q.v. struct StrAux in lpcap.c
#
# value is a Breadcrumb or subject index pair (start stop)
StrAux = Struct.new :isstring, :breadcrumb_idx, :subject_start, :subject_end
MAX_STR_CAPS = 10

# VM and post-match capture support
#
# q.v. LPEG's CaptureState, lpcap.h
#
# As a Ruby class it contains as much of the capture code (lpcap.c) as makes sense. Because of this, it needs to know the
# @subject and the extra_args
class CaptureState
  attr_reader :captures

  def initialize(breadcrumbs, subject, subject_index, extra_args, starting_index:)
    @breadcrumbs = breadcrumbs
    @breadcrumb_idx = starting_index || 0
    @subject = subject.freeze
    @subject_index = subject_index
    @extra_args = extra_args.freeze
    @captures = []
  end

  def capture_all
    push_capture until done?
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
      raise "Cannot pop off a negative number of elements" if num.negative?
      raise "There are not #{num} captures to pop" if num > @captures.size

      @captures.pop(num)
    else
      raise "There is not a capture to pop" unless @captures.size.positive?

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

  # The ending subject index of the _previous_ breadcrumb
  def prev_end_index
    raise "No previous breadcrumb" unless @breadcrumb_idx.positive?

    @breadcrumbs[@breadcrumb_idx - 1].end_index
  end

  # Extract the next capture, returning the number of values obtained.
  private def push_capture
    breadcrumb = current_breadcrumb

    case breadcrumb.kind
    when Capture::CONST, Capture::RUNTIME
      push breadcrumb.data
      advance
      1
    when Capture::POSITION
      push breadcrumb.subject_index
      advance
      1
    when Capture::ARGUMENT
      index = breadcrumb.data
      raise "Reference to absent extra argument ##{index}" if index > @extra_args.size

      # with an Argument Capture the extra arguments are indexed from 1
      push @extra_args[index - 1]
      advance
      1
    when Capture::SIMPLE
      count = push_nested_captures(add_extra: true)
      # We need to make the whole match appear first in the list we just generated
      munge_last!(count) if count > 1
      count
    when Capture::GROUP
      if breadcrumb.data
        # Named group. We don't extract anything but just move forward. A Backref capture might find us later
        seek_next!
        0
      else
        push_nested_captures
      end
    when Capture::BACKREF
      group_name = breadcrumb.data
      bc_idx = @breadcrumb_idx

      seek_back_ref!(group_name) # move to the named group capture
      count = push_nested_captures
      # restore our location and step to the next one
      @breadcrumb_idx = bc_idx
      advance

      count
    when Capture::SUBST
      push extract_subst_capture
      1
    when Capture::TABLE
      push_table_capture
    when Capture::FOLD
      push_fold_capture
    when Capture::STRING
      push extract_string_capture
      1
    when Capture::NUM
      push_num_capture
    when Capture::FUNCTION
      push_function_capture
    when Capture::QUERY
      push_query_capture
    else
      raise "Unhandled capture kind #{breadcrumb.kind}"
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
    open_capture = current_breadcrumb
    advance

    if open_capture.full?
      cpos = open_capture.subject_index
      match_len = open_capture.size - 1
      match_range = cpos...(cpos + match_len)
      push @subject[match_range].join
      return 1
    end

    count = 0
    count += push_capture until current_breadcrumb.close? # Nested captures

    # We have reached our matching close
    close_capture = current_breadcrumb
    advance
    if add_extra || count.zero?
      match_range = (open_capture.subject_index)...(close_capture.subject_index)
      push @subject[match_range].join
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
  #
  # Experimental: return a TableCapture instance
  def push_table_capture
    if current_breadcrumb.full?
      # Empty table
      push []
      advance
      return 1
    end

    advance # move past the open capture
    named_results = {}
    indexed_results = []
    next_index = 0
    until current_breadcrumb.close?
      breadcrumb = current_breadcrumb
      if breadcrumb.kind == Capture::GROUP && breadcrumb.data
        # named group. We only keep track of the *first* value in the group
        push_one_nested_value
        value = pop
        named_results[breadcrumb.data] = value
      else
        # not a named group
        # k is the number we just got. We pop them back and put them in our result object
        k = push_capture
        (0..(k - 1)).to_a.reverse.each do |i|
          indexed_results[next_index + i] = pop
        end
        next_index += k
      end
    end
    advance # skip the close entry

    push Capture::TableCapture.new(named_results, indexed_results)
    1
  end

  # This is LPEG's foldcap (lpcap.c)
  def push_fold_capture
    raise "no initial value for fold capture" if current_breadcrumb.full?

    fn = current_breadcrumb.data.must_be
    advance

    if current_breadcrumb.close? || (n = push_capture).zero?
      raise "no initial value for fold capture"
    end

    # discard all but one capture. This is the first value for the fold accumulator
    pop(n - 1)
    acc = pop
    until current_breadcrumb.close?
      n = push_capture
      acc = fn.call(acc, *pop(n))
    end
    advance # skip close
    push acc
    1
  end

  # Push nested values and then pop off all but one
  def push_one_nested_value
    n = push_nested_captures
    pop(n - 1)
  end

  # This is LPEG's numcap (lpcap.c)
  def push_num_capture
    idx = current_breadcrumb.data
    if idx.zero?
      # skip them all
      seek_next!
      return 0
    end

    n = push_nested_captures
    raise "no capture '#{idx}" if n < idx

    vals = pop(n) # pop them off
    push vals[idx - 1] # push back the one we want
    1
  end

  # This is LPEG's functioncap (lpcap.c)
  def push_function_capture
    proc = current_breadcrumb.data.must_be_a(Proc) # get the proc to call
    n = push_nested_captures # get the nested captures...
    args = pop(n) # ...pop them
    result = proc.call(*args) # ... and pass them to the proc
    # the results, if any, are the capture values
    #
    # The natural thing to do here would be result = Array(result) and just enumerate them to push onto the capture stack. BUT,
    # sometimes proc will return a Hash (when building a grammar in RE, for example) and Array({x: 1}) = [[:x, 1]], which is not
    # what we want. At root, the issue is that Lua is better than Ruby at distinguishing between a function that returns multiple
    # value and one that returns a single value that is an array. The following appears to be what we want, and remember that we
    # need to write capture functions that are careful to distinguish between returning [1,2,3] (multiple captures) and [[1,2,3]]
    # (single capture that is an array).
    #
    # Another gotcha: a function that returns nil does not give a capture, while one that returns [nil] has captured the single
    # value nil.
    if result.is_a?(Array)
      result.each { |cap| push cap }
      result.size
    elsif result
      push result
      1
    else
      0
    end
  end

  # This is LPEG's querycap (lpcap.c)
  def push_query_capture
    hash = current_breadcrumb.data.must_be_a(Hash)
    push_one_nested_value
    query_key = pop # pop it
    result = hash[query_key]
    if result
      push(result)
      1
    else
      0 # no result
    end
  end

  # This is LPEG's substcap (lpcap.c)
  def extract_subst_capture
    breadcrumb = current_breadcrumb
    curr = breadcrumb.subject_index
    result = +""
    if breadcrumb.full?
      result = @subject[curr, breadcrumb.size - 1].join
    else
      advance # skip open
      until current_breadcrumb.close?
        nxt = current_breadcrumb.subject_index
        result << @subject[curr, nxt - curr].join
        if (match = extract_one_string("replacement"))
          result << match
          curr = prev_end_index
        else
          # no capture index
          curr = nxt
        end
      end
      result << @subject[curr, current_breadcrumb.subject_index - curr].join
    end
    advance
    result
  end

  # This is LPEG's stringcap (lpcap.c)
  #
  # We return the result
  def extract_string_capture
    fmt = current_breadcrumb.data.must_be_a(String)
    the_str_caps = str_caps
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
      raise "invalid capture index (#{capture_index})" if capture_index > the_str_caps.size - 1

      str_cap = the_str_caps[capture_index]
      if str_cap.isstring
        result << @subject[(str_cap.subject_start)...(str_cap.subject_end)].join
        next
      end

      cs_index = @breadcrumb_idx
      @breadcrumb_idx = the_str_caps[capture_index].breadcrumb_idx
      val = extract_one_string("capture") # lpeg's addonestring, but return instead of appending to b
      raise "no values in capture index #{capture_index}" unless val

      result << val
      @breadcrumb_idx = cs_index
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
  # We just return the value, or nil if there isn't one
  def extract_one_string(what)
    case current_breadcrumb.kind
    when Capture::STRING
      extract_string_capture
    when Capture::SUBST
      extract_subst_capture
    else
      n = push_capture
      return nil if n.zero?

      pop(n - 1) # just leave one
      res = pop
      # LPEG tests the type of this value with lua_isstring, which returns 1 if the value is a string or a number.
      raise "invalid #{what} value (a #{res.class})" unless res.is_a?(String) || res.is_a?(Numeric)

      res.to_s
    end
  end

  # In LPEG this logic is split between the main VM loop (lpvm.c) and the runtimecap function (lpcap.c). As noted above, the LPEG
  # code is complicated by the need to manage references to objects living on the Lua stack to avoid C-side memory leaks. We don't
  # have to worry about such things
  #
  # We start at the CLOSE_RUN_TIME breadcrumb and
  #  - change the CLOSE_RUN_TIME to a regular CLOSE
  #  - find the matching OPEN and grab the Proc that we need to call
  #  - push the nested captures and immediately pop them
  #  - pass the necessary arguments to proc: the subject, the current posistion, and the just-popped captures
  #  - clear out all the breadcrumbs after the OPEN
  #
  # We return a [bc, result] pair.
  #  - bc is the new "breadcrumb count" for the VM, as we discard the existing captures for the RunTime grouping.
  #  - result is the result of the Proc call
  def run_time_capture
    seek_matching_open!
    current_breadcrumb.kind.must_be(Capture::GROUP)
    open_cap_idx = index

    proc = current_breadcrumb.data.must_be_a(Proc) # get the proc to call

    @subject_as_str ||= @subject.join
    args = [@subject_as_str, @subject_index]
    n = push_nested_captures
    args += pop(n) # prepare arguments for the function
    result = Array(proc.call(*args)) # ... and pass them to the proc

    [open_cap_idx + 1, result]
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
  def str_caps
    result = []
    first_aux = StrAux.new
    first_aux.isstring = true
    first_aux.subject_start = current_breadcrumb.subject_index
    result << first_aux

    first_is_full = current_breadcrumb.full?
    unless first_is_full
      advance # move past the Open
      until current_breadcrumb.close?
        if result.size > MAX_STR_CAPS
          seek_next! # just skip it
        elsif current_breadcrumb.kind == Capture::SIMPLE
          result += str_caps # get the matches recursively
        else
          # Not a string
          aux = StrAux.new
          aux.isstring = false
          aux.breadcrumb_idx = index
          seek_next!
          result << aux
        end
      end
    end
    result[0].subject_end = current_breadcrumb.end_index
    advance # skip capture close/full capture
    result
  end

  # Search backwards from the current breadcrumb for the start of the group capture with the given name.
  #
  # If we find it the state index is updated appropriately.
  # If we don't find it we raise an exception.
  #
  # This is LPEG's findback() (lpcap.c)
  def seek_back_ref!(group_name)
    group_name.must_be
    while @breadcrumb_idx.positive?
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
      raise "subject index underflow in seek_open!" if @breadcrumb_idx.negative?

      if current_breadcrumb.close?
        n += 1
      elsif !current_breadcrumb.full?
        # It's an open of some sort
        return if n.zero?

        n -= 1
      end
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
  def munge_last!(num)
    return if num == 1
    raise "Bad munge argument" unless num.positive?
    raise "Not enough values in array to munge it" if num > @captures.size

    tail = @captures.pop(num)
    last = tail.pop
    @captures << last
    @captures += tail
  end
end
