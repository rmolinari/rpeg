#!/usr/bin/env ruby

require 'byebug'

require_relative 'rpeg'

# A straight port of LPEG's re module, though without any support for locales
module RE
  extend self

  Pattern = RPEG::Pattern

  @mem = {} # memo space for patterns
  @fmem = {}
  @gmem = {}

  # What does "compiled" mean here?
  #
  # Oh. Maybe it is the Pattern built from the regexp-y thing.
  def compile(p, *defs)
    return p if p.is_a?(Pattern)

    defs = [{}] if defs.empty? # for the sake of p_def, below

    cp = PATTERN.match(p, 0, *defs)
    raise "incorrect pattern" unless cp

    cp
  end

  def match(s, p, i = 0)
    cp = (@mem[p] ||= compile(p))
    cp.match(s, i)
  end

  def find(s, p, i = 0)
    cp = @fmem[p]
    unless cp
      cp = compile(p) / 0
      cp = RPEG.P([RPEG.Cp() * cp * RPEG.Cp() + 1 * RPEG.V(0)])
      @fmem[p] = cp
    end

    i, e = cp.match(s, i)

    return [i, e - 1] if i
  end

  def gsub(s, p, rep)
    g = @gmem[p] || {} #-- ensure gmem[p] is not collected while here. What does that mean?
    @gmem[p] = g
    cp = g[rep]
    unless cp
      cp = compile(p)
      cp = RPEG.Cs((cp / rep + 1)**0)
      g[rep] = cp
    end
    cp.match(s)
  end

  private def internals
    m = RPEG

    any = m.P(1)
    lower = m.R("az")
    upper = m.R("AZ")
    alpha = lower + upper
    digit = m.R("09")
    alnum = alpha + digit
    space = m.S(" \n\t")
    printable = m.R(' ~')
    word = alnum
    predef = {
      alpha:, digit:, lower:, upper:, space:, alnum:, word:,
      graph: printable - space,
      xdigit: digit + m.R("af", "AF"),
      punct: printable - (space + alnum),
      cntrl: any - printable
    }
    predef.keys.each do |key|
      short = key.to_s[0].to_sym
      predef[short] = predef[key]
      predef[short.upcase] = any - predef[key]
    end
    predef[:nl] = m.P("\n")

    name = m.R("AZ", "az", "__") * m.R("AZ", "az", "__", "09")**0
    p_space = (predef[:space] + "--" * (any - predef[:nl])**0)**0

    arrow = p_space * "<-"
    seq_follow = m.P("/") + ")" + "}" + ":}" + "~}" + "|}" + (name * arrow) + -1
    name = m.C(name)

    # -- a defined name only have meaning in a given environment
    p_def = name * m.Carg(1)

    num = m.C(m.R("09")**1) * p_space / ->(s) { s.to_i }
    p_spacetring = "'" * m.C((any - "'")**0) * "'" + '"' * m.C((any - '"')**0) * '"'

    defined = "%" * p_def / lambda do |c, defs|
      cat = (defs && (defs[c] || defs[c.to_sym])) || predef[c.to_sym]
      raise "name '#{c}' undefined" unless cat

      cat
    end

    # Why do we have this as well as defined ?
    getdef = lambda do |id, defs|
      c = defs && (defs[id] || defs[id.to_sym])
      raise "undefined name: #{id}" unless c

      c
    end

    p_range = m.Cs(any * (m.P("-") / "") * (any - "]")) / ->(s) { m.R(s) }
    item = (defined + p_range + m.C(any)) / ->(a) { m.P(a) }

    p_class = "[" *
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

    equalcap = lambda do |s, i, c|
      return nil unless c.is_a?(String)

      e = c.length + i
      e if s[i..(e - 1)] == c
    end

    adddef = lambda do |t, k, exp|
      if t[k]
        error("'#{k}' already defined as a rule")
      else
        t[k] = exp
      end
      return t
    end

    firstdef = ->(n, r) { adddef.call({}, n, r) }

    f_open_call = lambda do |n, b|
      raise "rule '#{n}' used outside a grammar" unless b

      m.V(n)
    end

    # -- match a name and return a group of its corresponding definition
    # -- and 'f' (to be folded in 'Suffix')
    defwithfunc = ->(f) { m.Cg(p_def / getdef * m.Cc(f)) }

    patt_add = ->(p1, p2) { p1 + p2 }
    patt_mul = ->(p1, p2) { p1 * p2 }
    patt_rpt = ->(p1, n)  { p1**n }
    patt_replace = ->(p1, rep) { p1 / rep }
    pos_capture = ->(*) { m.Cp }
    tonumber = ->(s) { Integer(s) }

    call_patt = lambda do |fun|
      ->(*args) { RPEG.send(fun, *args) }
    end

    exp = m.P(
      {
        initial: :Exp,
        Exp: p_space * (
          m.V("Grammar") +
          m.Cf(m.V("Seq") * ("/" * p_space * m.V("Seq"))**0, patt_add)
        ),
        Seq: m.Cf(m.Cc(m.P("")) * m.V("Prefix")**0, patt_mul) * (+seq_follow + patt_error),
        Prefix: ("&" * p_space * m.V("Prefix") / ->(p) { +p } +
                 "!" * p_space * m.V("Prefix") / ->(p) { -p } +
                 m.V("Suffix")),
        Suffix: m.Cf(m.V("Primary") * p_space *
                     ((m.P("+") * m.Cc(1, patt_rpt) +
                       m.P("*") * m.Cc(0, patt_rpt) +
                       m.P("?") * m.Cc(-1, patt_rpt) +
                       "^" * (
                         m.Cg(num * m.Cc(mult)) +
                         m.Cg(m.C(m.S("+-") * m.R("09")**1) / tonumber * m.Cc(patt_rpt))
                       ) +
                       "->" * p_space * (
                         m.Cg((p_spacetring + num) * m.Cc(patt_replace)) +
                         m.P("{}") * m.Cc(nil, call_patt[:Ct]) +
                         defwithfunc[patt_replace]
                       ) +
                       "=>" * p_space * defwithfunc[call_patt[:Cmt]] +
                       "~>" * p_space * defwithfunc[call_patt[:Cf]]
                      ) * p_space
                     )**0, ->(a, b, f) { f.call(a, b) }),
        Primary: (
          "(" * m.V("Exp") * ")" +
          p_spacetring / call_patt.call(:P) +
          p_class +
          defined +
          "{:" * (name * ":" + m.Cc(nil)) * m.V("Exp") * ":}" / ->(n, p) { m.Cg(p, n) } +
          "=" * name / ->(n) { m.Cmt(m.Cb(n), ->(*args) { equalcap[*args] }) } +
          m.P("{}") / pos_capture +
          "{~" * m.V("Exp") * "~}" / call_patt.call(:Cs) +
          "{|" * m.V("Exp") * "|}" / call_patt.call(:Ct) +
          "{" * m.V("Exp") * "}" / call_patt.call(:C) +
          m.P(".") * m.Cc(any) +
          (name * -arrow + "<" * name * ">") * m.Cb("G") / ->(*args) { f_open_call.call(*args) }
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

    p_space * m.Cg(m.Cc(false), "G") * exp / call_patt.call(:P) * (-any + patt_error)
  end

  PATTERN = internals
end
