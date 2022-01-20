#!/usr/bin/env ruby

require 'byebug'

require_relative 'pattern'

# A straight port of LPEG's re module, though without any support for locales
module RE
  extend self

  @mem = {} # memo space for patterns
  @fmem = {}
  @gmem = {}

  # What does "compiled" mean here?
  #
  # Oh. Maybe it is the Pattern build from the regexp-y thing.
  def compile(p, *defs)
    # return p if Pattern.type(p) == "pattern" # -- already compiled

    cp = PATTERN.match(p, 0, *defs)
    raise "incorrect pattern" unless cp

    cp
  end

  def match(s, p, i = 0)
    cp = @mem[p]

    unless cp
      cp = compile(p)
      @mem[p] = cp
    end
    cp.match(s, i)
  end

  def find(s, p, i = 0)
    cp = @fmem[p]
    unless cp
      cp = compile(p) / 0
      cp = Pattern.P([m.Cp() * cp * mm.Cp() + 1 * mm.V(0)])
      fmem[p] = cp
    end

    i, e = cp.match(s, i)

    return [i, e - 1] if i
  end

  def gsub(s, p, rep)
    g = @gmem[p] || {}  #-- ensure gmem[p] is not collected while here. What does that mean?
    @gmem[p] = g
    cp = g[rep]
    unless cp
      cp = compile(p)
      cp = Pattern.Cs((cp / rep + 1)**0)
      g[rep] = cp
    end
    cp.match(s)
  end

  # How to expose?
  private def internals
    m = Pattern

    any = m.P(1)
    lower = m.R("az")
    upper = m.R("AZ")
    alpha = lower + upper
    digit = m.R("09")
    alnum = alpha + digit
    space = m.S(" \n\t")
    printable = m.R(' ~')
    x = {
      nl: m.P("\n"),
      alpha:,
      digit:,
      lower:,
      upper:,
      space:,
      graph: printable - space,
      alnum:,
      xdigit: digit + m.R("af", "AF"),
      punct: printable - (space + alnum),
      cntrl: any - printable
    }
    %i[alpha cntrl digit graph lower punct space upper alnum xdigit].each do |key|
      short = key.to_s[0].to_sym
      x[short] = x[key]
      x[short.upcase] = any - x[key]
    end
    predef = x

    # p_I = m.P(->(s,i) { print "#{i}   #{s[0, i-2]}"; return i }) # Diagnostic?
    name = m.R("AZ", "az", "__") * m.R("AZ", "az", "__", "09")**0
    p_S = (predef[:space] + "--" * (any - predef[:nl])**0)**0

    arrow = p_S * "<-"
    seq_follow = m.P("/") + ")" + "}" + ":}" + "~}" + "|}" + (name * arrow) + -1
    name = m.C(name)

    # -- a defined name only have meaning in a given environment
    p_Def = name * m.Carg(1)

    num = m.C(m.R("09")**1) * p_S / ->(s) { s.to_i }
    p_String = "'" * m.C((any - "'")**0) * "'" + '"' * m.C((any - '"')**0) * '"'

    defined = "%" * p_Def / lambda do |c, defs|
      cat = (defs && defs[c]) || predef[c]
      raise "name '#{c}' undefined" unless cat

      cat
    end

    p_Range = m.Cs(any * (m.P("-") / "") * (any - "]")) / ->(s) { m.R(s) }
    item = (defined + p_Range + m.C(any)) / ->(a) { m.P(a) }

    p_Class = "[" *
              m.C(m.P("^")**-1) * # -- optional complement symbol
              m.Cf(item * (item - "]")**0, ->(y, z) { y + z }) / ->(c, p) { c == "^" ? any - p : p } *
              "]"

    patt_error = lambda do |s, i, *|
      msg = s.length < i + 20 ? s[i...] : "#{s[i, 20]}..."
      msg = "pattern error near '#{msg}'"
      raise msg # re.lua has error(msg, 2) but I don't know what that does
    end

    mult = lambda do |patt, n|
      np = m.P(true)
      while n >= 1
        np *= patt if n.odd?
        patt *= patt
        n /= 2
      end
      np
    end

    # I don't know what this is doing
    equalcap = lambda do |s, i, c|
      return nil unless c.is_a?(String)

      e = c.length + i
      if s[i..(e-1)] == c
        e
      else
        nil
      end
    end

    getdef = lambda do |id, defs|
      c = defs && defs[id]
      raise "undefined name: #{id}" unless c

      c
    end

    adddef = lambda do |t, k, exp|
      if t[k] then
        error("'#{k}' already defined as a rule")
      else
        t[k] = exp
      end
      return t
    end

    # local function firstdef (n, r) return adddef({n}, n, r) end
    # Is this right?
    firstdef = ->(n, r) { adddef.call({}, n, r) }

    f_NT = lambda do |n, b|
      raise "rule '#{n}' used outside a grammar" unless b

      m.V(n)
    end

    # -- match a name and return a group of its corresponding definition
    # -- and 'f' (to be folded in 'Suffix')
    defwithfunc = ->(f) { m.Cg(p_Def / getdef * m.Cc(f)) }

    patt_add = ->(p1, p2) { p1 + p2 }
    patt_mul = ->(p1, p2) { p1 * p2 }
    patt_rpt = ->(p1, n)  { p1**n }
    patt_replace = ->(p1, x) { p1 / x }
    pos_capture = ->(*) { m.Cp }

    call_patt = lambda do |fun|
      ->(*args) { Pattern.send(fun, *args) }
    end

    # The big guy! Wow. This will take some debugging
    exp = m.P(
      {
        initial: :Exp,
        Exp: p_S * (
          m.V("Grammar") +
          m.Cf(m.V("Seq") * ("/" * p_S * m.V("Seq"))**0, patt_add)
        ),
        Seq: m.Cf(m.Cc(m.P("")) * m.V("Prefix")**0, patt_mul) * (+seq_follow + patt_error),
        Prefix: ("&" * p_S * m.V("Prefix") / ->(p) { +p } +
                 "!" * p_S * m.V("Prefix") / ->(p) { -p } +
                 m.V("Suffix")),
        Suffix: m.Cf(m.V("Primary") * p_S *
                     ((m.P("+") * m.Cc(1, patt_rpt) +
                       m.P("*") * m.Cc(0, patt_rpt) +
                       m.P("?") * m.Cc(-1, patt_rpt) +
                       "^" * (
                         m.Cg(num * m.Cc(mult)) +
                         m.Cg(m.C(m.S("+-") * m.R("09")**1) * m.Cc(patt_rpt))
                       ) +
                       "->" * p_S * (
                         m.Cg((p_String + num) * m.Cc(patt_replace)) +
                         m.P("{}") * m.Cc(nil, call_patt[:Ct]) +
                         defwithfunc[patt_replace]
                       ) +
                       "=>" * p_S * defwithfunc[call_patt[:Cmt]] +
                       "~>" * p_S * defwithfunc[call_patt[:Cf]]
                      ) * p_S
                     )**0, ->(a, b, f) { f.call(a,b) }),
        Primary: (
          "(" * m.V("Exp") * ")" +
          p_String / call_patt.call(:P) +
          p_Class +
          defined +
          "{:" * (name * ":" + m.Cc(nil)) * m.V("Exp") * ":}" / ->(n, p) { m.Cg(p, n) } +
          "=" * name / ->(n) { m.Cmt(m.Cb(n), ->(*args) { equalcap[*args] }) } +
          m.P("{}") / pos_capture +
          "{~" * m.V("Exp") * "~}" / call_patt.call(:Cs) +
          "{|" * m.V("Exp") * "|}" / call_patt.call(:Ct) +
          "{" * m.V("Exp") * "}" / call_patt.call(:C) +
          m.P(".") * m.Cc(any) +
          (name * -arrow + "<" * name * ">") * m.Cb("G") / ->(*args) { f_NT.call(*args) }
        ),
        Definition: name * arrow * m.V("Exp"),
        Grammar: (
          m.Cg(m.Cc(true), "G") *
          m.Cf(
            m.V("Definition") / firstdef * m.Cg(m.V("Definition"))**0,
            adddef
          ) / call_patt.call(:P)
        )
      }
    )

    p_S * m.Cg(m.Cc(false), "G") * exp / call_patt.call(:P) * (-any + patt_error)
  end

  PATTERN = internals
end
