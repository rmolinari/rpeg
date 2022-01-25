require_relative 'captures'

# Instances are generated during program generation in Pattern and consumed in the ParsingMachine
#
# - op_code: the instruction op
# - offset: the address offset used in jumps, calls, etc.
# - aux: extra information used by instructions like capture
#   - in LPEG this is used to carefully pack data by bit-twiddling, etc., but we can use anything, such as structs, etc., as needed
# - data: this is called "key" in LPEG and is used to store pointers to Lua-based objects, etc.
#   - we will just store Ruby objects here.
#   - it contains things like the set of characters for Charset instructions, etc.
# - dec: "decorations" for other things like labels that might be useful later in debugging, etc.
#   - it is ignored by the VM
class Instruction
  OP_CODES = %i[
    char charset any jump choice call return commit back_commit
    partial_commit span op_end fail fail_twice unreachable
    open_capture close_capture close_run_time full_capture behind
    test_char test_charset test_any
  ].each do |op|
    const_set op.upcase, op
  end

  OP_WIDTH = OP_CODES.map(&:length).max
  DECORATION_WIDTH = 15

  attr_reader :op_code, :data, :aux
  attr_accessor :offset, :dec

  def initialize(op_code, offset: nil, data: nil, aux: nil, dec: nil)
    raise "Bad instruction op_code #{op_code}" unless OP_CODES.include?(op_code)

    @op_code = op_code
    @offset = offset
    @data = data
    @aux = aux
    @dec = dec
  end

  def to_s
    return @to_s if @to_s

    str = (dec || "").to_s.rjust(DECORATION_WIDTH) + " :"
    str << op_code.to_s.upcase.rjust(OP_WIDTH + 1)

    str << " offset: #{offset}" if [TEST_CHAR, TEST_ANY, TEST_CHARSET].include?(op_code)

    case op_code
    when CHAR, TEST_CHAR
      str << "  #{data.dump}"
    when BEHIND
      str << "  #{aux}"
    when CHARSET, SPAN, TEST_CHARSET
      str << "  #{charset_rep(data)}"
    when JUMP, CHOICE, CALL, COMMIT, BACK_COMMIT, PARTIAL_COMMIT
      str << "  #{offset}"
    when RETURN, OP_END, FAIL, FAIL_TWICE, UNREACHABLE, ANY, TEST_ANY
    # no-op
    when OPEN_CAPTURE, CLOSE_CAPTURE, FULL_CAPTURE, CLOSE_RUN_TIME
      str << "  data:#{data}, aux:#{aux}"
    else
      raise "Unhandled op_code #{op_code} in Instruction#to_s"
    end
    @to_s = str
  end

  # A shorter representation of a charset
  private def charset_rep(char_set)
    return "" if char_set.empty?

    bools = []
    char_set.each do |ch|
      bools[ch.ord] = true
    end

    # attach an artificial false bool to trigger ship-out
    bools << false

    parts = []

    open = false
    first = last = nil
    bools.each_with_index do |present, idx|
      if present
        if open
          last = idx
        else
          # start a new range
          first = idx
          last = idx
          open = true
        end
      elsif open
        # a subrange just closed
        if last == first
          parts << first.chr(Encoding::UTF_8).dump
        else
          parts << (first.chr(Encoding::UTF_8) + ".." + last.chr(Encoding::UTF_8)).dump
        end
        first = last = nil
        open = false
      end
    end

    parts.join(", ")
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
    @bread_count = 0 # the number of breadcrumbs in @breadcrumbs (some in the array may be stale)

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

  def run
    step until done?
  end

  def step
    i = Instruction # shorthand

    if @i_ptr == :fail
      handle_fail_ptr
      return
    end

    # raise "current instruction pointer #{@i_ptr} is negative" if @i_ptr < 0

    instr = @program[@i_ptr]

    case instr.op_code
    when i::CHAR
      check_char(instr.data == @subject[@subject_index])
    when i::CHARSET
      check_char(instr.data.include?(@subject[@subject_index]))
    when i::ANY
      check_char(@subject_index < @subject.size)
    when i::TEST_CHAR
      test_char(instr.data == @subject[@subject_index], instr.offset)
    when i::TEST_CHARSET
      test_char(instr.data.include?(@subject[@subject_index]), instr.offset)
    when i::TEST_ANY
      test_char(@subject_index < @subject.size, instr.offset)
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
      stack_top.bread_count = @bread_count
      @i_ptr += instr.offset
    when i::BACK_COMMIT
      # A combination of a fail and a commit. We backtrack, but then jump to the specified instruction rather than using the
      # backtrack label. It's used for the AND pattern. See Ierusalimschy, 4.4
      stack_top = pop(:state)
      @subject_index = stack_top.subject_index
      @bread_count = stack_top.bread_count
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
    when i::CLOSE_RUN_TIME
      # The LPEG code for runtime captures is very complicated. Reading through it, it appears that the complexity comes from
      # needing to carefully manage the capture breadcrumbs wrt to the Lua values living on the Lua stack to avoid memory
      # leaks. We don't have to worry about that here, as everything is in Ruby and we can leave the hard stuff to the garbage
      # collector. The remaining work is little more than we have with a function capture.
      result = run_time_capture
      handle_run_time_capture_result(result)
    when i::OPEN_CAPTURE
      record_capture(instr, size: 0, subject_index: @subject_index)
    when i::CLOSE_CAPTURE
      # As in LPEG: "if possible, turn capture into a full capture"
      raise "Close capture without an open" unless @bread_count.positive?

      lc = @breadcrumbs[@bread_count - 1].must_be # still on the breadcrumb list
      if lc.size.zero? && (@subject_index - lc.subject_index) < 255 # TODO: should we care about an upper bound here?
        # The previous breadcrumb was an OPEN, and we are closing it
        lc.size = @subject_index - lc.subject_index + 1
        @i_ptr += 1
      else
        record_capture(instr, size: 1, subject_index: @subject_index)
      end
    when i::FULL_CAPTURE
      # We have an all-in-one match, and the "capture length" tells us how far back in the subject the match started.
      len = (instr.aux[:capture_length] || 0).must_be(Integer)
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

  ########################################
  # Support for a debugger

  # These are internals that aren't useful in the usual case
  attr_reader :program, :subject, :subject_index, :extra_args, :stack, :i_ptr

  def breadcrumbs
    @breadcrumbs[0, @bread_count]
  end

  #
  ########################################

  # For this and the handling of the foo_CAPTURE op codes above, see the corresponding LPEG code in lpvm.c
  #
  # In that code, captop points to the "next" or "new" capture info, so captop - 1 is the current top.
  private def record_capture(instr, size:, subject_index:)
    add_capture Capture::Breadcrumb.new(size, subject_index, instr.data, instr.aux[:kind].must_be)
    @i_ptr += 1
  end

  private def add_capture(breadcrumb)
    @breadcrumbs[@bread_count] = breadcrumb
    @bread_count += 1
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

  # React to a character match or failure in one of the TestFoo instruction
  #
  # IMPORTANT NOTE
  #
  # Ierusalimschy's paper describes these as consuming the next character, and code generated for things like char sequences being
  # tweaked to take this into account. BUT the LPEG code does it differently. These _check_ the current character but do not consume
  # it: the following test is expected to do so. During code generation the "currently controlling" TEST_FOO is passed along so
  # followup checks can be optimized. See codechar and codecharset in lpcode.c.
  private def test_char(success, offset)
    @i_ptr += success ? 1 : offset
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
      @bread_count = top.bread_count
    end
  end

  ########################################
  # Stack manipulation

  Frame = Struct.new :type, :i_ptr, :subject_index, :bread_count

  # We push either
  # - an instruction pointer, which may later be used to jump, etc, or
  # - the current state with an offset, which is the [instr ptr + offset, subject_index, breadcrumb list] triple.
  private def push(type, offset)
    raise "Must push something onto stack" unless offset
    raise "Bad stack frame type" unless %i[instruction state].include?(type)

    frame = if type == :state
              Frame.new(type, @i_ptr + offset, @subject_index, @bread_count)
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
    return unless expected_type

    raise "Top of stack is of type #{frame.type}, not of expected type #{expected_type}" unless frame.type == expected_type
  end

  ########################################
  # Capture extraction code

  # From the ICloseRuntIme main-loop switch statement in lpvm.c
  def handle_run_time_capture_result(results)
    directive, *dyn_captures = results
    unless directive
      @i_ptr = :fail
      return
    end

    @subject_index = if directive == true
                       @subject_index
                     else
                       directive.must_be_a(Integer)
                       if directive < @subject_index || directive > @subject.size
                         raise 'invalid position returned by match-time capture'
                       end

                       directive
                     end

    if dyn_captures.empty?
      # no dynamic captures. Just get rid of the OPEN capture we still have
      @bread_count -= 1
    else
      # This is LPEG's adddyncaptures in lpvm.c
      @breadcrumbs[@bread_count - 1].data = nil # make the group capture an anonymous group
      dyn_captures.each do |cap_val|
        # LPEG uses a special RUNTIME capture kind here to help find these things later if they need to be removed. We don't appear
        # to need it - we could just use a CONST capture. But let's follow LPEG just in case.
        add_capture Capture::Breadcrumb.new(1, @subject_index, cap_val, Capture::RUNTIME)
      end
      add_capture Capture::Breadcrumb.new(1, @subject_index, nil, Capture::CLOSE) # close the group
    end
    @i_ptr += 1
  end

  # Returns the captures obtained when we ran the machine.
  #
  # If there are no captures we return the final index into the subject string. This is typically one past the matched section.
  # If there is exactly one capture we return it.
  # If there are multiple captures we return them in an array.
  #
  # The capture code in LPEG (mostly in lpcap.c) looks complicated at first but it is made up of a bunch of pieces that each do one
  # thing and coordinate well togehter. Some extra complexity comes from the manual memory management required in C and the need to
  # interact with Lua values - this appears to be especially the case with the Runtime capture code, which is bewildering at first
  # view. Porting it one capture kind at a time let me understand it at some level as I went.
  #
  # Basic model:
  #
  # - We push Breadcrumb objects onto the stack as we run the VM based on the instructions generated from the patterns. We never pop
  #   anything from the stack: the Captures are breadcrumbs that let us work out after the fact what happend. Things do get removed
  #   from the Capture stack but only at backtrack points because a match has failed.
  # - The End instruction tacks on an unbalanced CloseCapture. This appears to be simply an end-marker like the null string
  #   terminator. We don't do this
  # - After the VM runs we analyze the Breadcrumbs to calculate the captures. We go back and forth through the data. So isn't not a
  #   stack, but an array.
  #
  # This method plays the same role as LPEG's getcaptures (lpcap.c)
  def captures
    raise "Cannot call #captures unless machine ran sucessfully" unless done? && success?

    @capture_state = new_capture_state
    @capture_state.capture_all

    result = @capture_state.captures

    return @subject_index if result.empty?
    return result.first if result.size == 1

    result
  end

  # This stub needs to be in ParsingMachine and not CaptureState because it must modify @bread_count
  def run_time_capture
    # We need point to the close capture we just hit. LPEG is tricksy here: there isn't actually a CLOSE capture/breadcrumb yet, but
    # the data structure - an array of Capture objects - means that the "next capture" memory be interpreted as a Capture. For once
    # we have to do something manually that in the C code happens "automatically"
    add_capture Capture::Breadcrumb.new(0, @subject_index, nil, Capture::CLOSE)
    capture_state = new_capture_state(@bread_count - 1) # start on the CLOSE we just tacked on

    @bread_count, result = capture_state.run_time_capture
    result
  end

  def new_capture_state(starting_index = nil)
    CaptureState.new(@breadcrumbs[0, @bread_count], @subject, @subject_index, @extra_args, starting_index:)
  end
end
