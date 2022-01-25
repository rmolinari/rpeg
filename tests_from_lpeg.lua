#!/usr/bin/env lua

-- A copy of LPEG's test.lua. As I migrate these test cases into the RPEG code I delete them from here

-- $Id: test.lua $

-- require"strict"    -- just to be pedantic

local m = require"lpeg"


-- for general use
local a, b, c, d, e, f, g, p, t


-- compatibility with Lua 5.2
local unpack = rawget(table, "unpack") or unpack
local loadstring = rawget(_G, "loadstring") or load


local any = m.P(1)
local space = m.S" \t\n"^0

local function checkeq (x, y, p)
if p then print(x,y) end
  if type(x) ~= "table" then assert(x == y)
  else
    for k,v in pairs(x) do checkeq(v, y[k], p) end
    for k,v in pairs(y) do checkeq(v, x[k], p) end
  end
end


local mt = getmetatable(m.P(1))


local allchar = {}
for i=0,255 do allchar[i + 1] = i end
allchar = string.char(unpack(allchar))
assert(#allchar == 256)

local function cs2str (c)
  return m.match(m.Cs((c + m.P(1)/"")^0), allchar)
end

local function eqcharset (c1, c2)
  assert(cs2str(c1) == cs2str(c2))
end


print"General tests for LPeg library"

assert(type(m.version()) == "string")
print("version " .. m.version())
assert(m.type("alo") ~= "pattern")
assert(m.type(io.input) ~= "pattern")
assert(m.type(m.P"alo") == "pattern")

-- tests for locale
do
  assert(m.locale(m) == m)
  local t = {}
  assert(m.locale(t, m) == t)
  local x = m.locale()
  for n,v in pairs(x) do
    assert(type(n) == "string")
    eqcharset(v, m[n])
  end
end

local digit = m.S"0123456789"
local upper = m.S"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
local lower = m.S"abcdefghijklmnopqrstuvwxyz"
local letter = m.S"" + upper + lower
local alpha = letter + digit + m.R()

local word = alpha^1 * (1 - alpha)^0
local eos = m.P(-1)

function basiclookfor (p)
  return m.P {
    [1] = p + (1 * m.V(1))
  }
end

function caplookfor (p)
  return basiclookfor(p:C())
end

assert(m.match(caplookfor(letter^1), "   4achou123...") == "achou")
a = {m.match(caplookfor(letter^1)^0, " two words, one more  ")}
checkeq(a, {"two", "words", "one", "more"})

assert(m.match( basiclookfor((#m.P(b) * 1) * m.Cp()), "  (  (a)") == 7)



-- test for alternation optimization
assert(m.match(m.P"a"^1 + "ab" + m.P"x"^0, "ab") == 2)
assert(m.match((m.P"a"^1 + "ab" + m.P"x"^0 * 1)^0, "ab") == 3)
assert(m.match(m.P"ab" + "cd" + "" + "cy" + "ak", "98") == 1)
assert(m.match(m.P"ab" + "cd" + "ax" + "cy", "ax") == 3)
assert(m.match("a" * m.P"b"^0 * "c"  + "cd" + "ax" + "cy", "ax") == 3)
assert(m.match((m.P"ab" + "cd" + "ax" + "cy")^0, "ax") == 3)
assert(m.match(m.P(1) * "x" + m.S"" * "xu" + "ay", "ay") == 3)
assert(m.match(m.P"abc" + "cde" + "aka", "aka") == 4)
assert(m.match(m.S"abc" * "x" + "cde" + "aka", "ax") == 3)
assert(m.match(m.S"abc" * "x" + "cde" + "aka", "aka") == 4)
assert(m.match(m.S"abc" * "x" + "cde" + "aka", "cde") == 4)
assert(m.match(m.S"abc" * "x" + "ide" + m.S"ab" * "ka", "aka") == 4)
assert(m.match("ab" + m.S"abc" * m.P"y"^0 * "x" + "cde" + "aka", "ax") == 3)
assert(m.match("ab" + m.S"abc" * m.P"y"^0 * "x" + "cde" + "aka", "aka") == 4)
assert(m.match("ab" + m.S"abc" * m.P"y"^0 * "x" + "cde" + "aka", "cde") == 4)
assert(m.match("ab" + m.S"abc" * m.P"y"^0 * "x" + "ide" + m.S"ab" * "ka", "aka") == 4)
assert(m.match("ab" + m.S"abc" * m.P"y"^0 * "x" + "ide" + m.S"ab" * "ka", "ax") == 3)
assert(m.match(m.P(1) * "x" + "cde" + m.S"ab" * "ka", "aka") == 4)
assert(m.match(m.P(1) * "x" + "cde" + m.P(1) * "ka", "aka") == 4)
assert(m.match(m.P(1) * "x" + "cde" + m.P(1) * "ka", "cde") == 4)
assert(m.match(m.P"eb" + "cd" + m.P"e"^0 + "x", "ee") == 3)
assert(m.match(m.P"ab" + "cd" + m.P"e"^0 + "x", "abcd") == 3)
assert(m.match(m.P"ab" + "cd" + m.P"e"^0 + "x", "eeex") == 4)
assert(m.match(m.P"ab" + "cd" + m.P"e"^0 + "x", "cd") == 3)
assert(m.match(m.P"ab" + "cd" + m.P"e"^0 + "x", "x") == 1)
assert(m.match(m.P"ab" + "cd" + m.P"e"^0 + "x" + "", "zee") == 1)
assert(m.match(m.P"ab" + "cd" + m.P"e"^1 + "x", "abcd") == 3)
assert(m.match(m.P"ab" + "cd" + m.P"e"^1 + "x", "eeex") == 4)
assert(m.match(m.P"ab" + "cd" + m.P"e"^1 + "x", "cd") == 3)
assert(m.match(m.P"ab" + "cd" + m.P"e"^1 + "x", "x") == 2)
assert(m.match(m.P"ab" + "cd" + m.P"e"^1 + "x" + "", "zee") == 1)
assert(not m.match(("aa" * m.P"bc"^-1 + "aab") * "e", "aabe"))

assert(m.match("alo" * (m.P"\n" + -1), "alo") == 4)


-- bug in 0.12 (rc1)
assert(m.match((m.P"\128\187\191" + m.S"abc")^0, "\128\187\191") == 4)

assert(m.match(m.S"\0\128\255\127"^0, string.rep("\0\128\255\127", 10)) ==
    4*10 + 1)

-- optimizations with optional parts
assert(m.match(("ab" * -m.P"c")^-1, "abc") == 1)
assert(m.match(("ab" * #m.P"c")^-1, "abd") == 1)
assert(m.match(("ab" * m.B"c")^-1, "ab") == 1)
assert(m.match(("ab" * m.P"cd"^0)^-1, "abcdcdc") == 7)

assert(m.match(m.P"ab"^-1 - "c", "abcd") == 3)

p = ('Aa' * ('Bb' * ('Cc' * m.P'Dd'^0)^0)^0)^-1
assert(p:match("AaBbCcDdBbCcDdDdDdBb") == 21)


-- bug in 0.12.2
-- p = { ('ab' ('c' 'ef'?)*)? }
p = m.C(('ab' * ('c' * m.P'ef'^-1)^0)^-1)
s = "abcefccefc"
assert(s == p:match(s))
 

pi = "3.14159 26535 89793 23846 26433 83279 50288 41971 69399 37510"
assert(m.match(m.Cs((m.P"1" / "a" + m.P"5" / "b" + m.P"9" / "c" + 1)^0), pi) ==
  m.match(m.Cs((m.P(1) / {["1"] = "a", ["5"] = "b", ["9"] = "c"})^0), pi))
print"+"


-- test for error messages
local function checkerr (msg, f, ...)
  local st, err = pcall(f, ...)
  assert(not st and m.match({ m.P(msg) + 1 * m.V(1) }, err))
end

checkerr("rule '1' may be left recursive", m.match, { m.V(1) * 'a' }, "a")
checkerr("rule '1' used outside a grammar", m.match, m.V(1), "")
checkerr("rule 'hiii' used outside a grammar", m.match, m.V('hiii'), "")
checkerr("rule 'hiii' undefined in given grammar", m.match, { m.V('hiii') }, "")
checkerr("undefined in given grammar", m.match, { m.V{} }, "")

checkerr("rule 'A' is not a pattern", m.P, { m.P(1), A = {} })
checkerr("grammar has no initial rule", m.P, { [print] = {} })

-- grammar with a long call chain before left recursion
p = {'a',
  a = m.V'b' * m.V'c' * m.V'd' * m.V'a',
  b = m.V'c',
  c = m.V'd',
  d = m.V'e',
  e = m.V'f',
  f = m.V'g',
  g = m.P''
}
checkerr("rule 'a' may be left recursive", m.match, p, "a")

-- Bug in peephole optimization of LPeg 0.12 (IJmp -> ICommit)
-- the next grammar has an original sequence IJmp -> ICommit -> IJmp L1
-- that is optimized to ICommit L1

p = m.P { (m.P {m.P'abc'} + 'ayz') * m.V'y'; y = m.P'x' }
assert(p:match('abcx') == 5 and p:match('ayzx') == 5 and not p:match'abc')


do
  -- large dynamic Cc
  local lim = 2^16 - 1
  local c = 0
  local function seq (n) 
    if n == 1 then c = c + 1; return m.Cc(c)
    else
      local m = math.floor(n / 2)
      return seq(m) * seq(n - m)
    end
  end
  p = m.Ct(seq(lim))
  t = p:match('')
  assert(t[lim] == lim)
  checkerr("too many", function () p = p / print end)
  checkerr("too many", seq, lim + 1)
end


do
  -- nesting of captures too deep
  local p = m.C(1)
  for i = 1, 300 do
    p = m.Ct(p)
  end
  checkerr("too deep", p.match, p, "x")
end


-- tests for non-pattern as arguments to pattern functions

p = { ('a' * m.V(1))^-1 } * m.P'b' * { 'a' * m.V(2); m.V(1)^-1 }
assert(m.match(p, "aaabaac") == 7)

p = m.P'abc' * 2 * -5 * true * 'de'  -- mix of numbers and strings and booleans

assert(p:match("abc01de") == 8)
assert(p:match("abc01de3456") == nil)

p = 'abc' * (2 * (-5 * (true * m.P'de')))

assert(p:match("abc01de") == 8)
assert(p:match("abc01de3456") == nil)

p = { m.V(2), m.P"abc" } *
     (m.P{ "xx", xx = m.P"xx" } + { "x", x = m.P"a" * m.V"x" + "" })
assert(p:match("abcaaaxx") == 7)
assert(p:match("abcxx") == 6)


print('+')


-- bug in 0.10 (rechecking a grammar, after tail-call optimization)
m.P{ m.P { (m.P(3) + "xuxu")^0 * m.V"xuxu", xuxu = m.P(1) } }


-- test for grammars (errors deep in calling non-terminals)
g = m.P{
  [1] = m.V(2) + "a",
  [2] = "a" * m.V(3) * "x",
  [3] = "b" * m.V(3) + "c"
}

assert(m.match(g, "abbbcx") == 7)
assert(m.match(g, "abbbbx") == 2)


-- tests for \0
assert(m.match(m.R("\0\1")^1, "\0\1\0") == 4)
assert(m.match(m.S("\0\1ab")^1, "\0\1\0a") == 5)
assert(m.match(m.P(1)^3, "\0\1\0a") == 5)
assert(not m.match(-4, "\0\1\0a"))
assert(m.match("\0\1\0a", "\0\1\0a") == 5)
assert(m.match("\0\0\0", "\0\0\0") == 4)
assert(not m.match("\0\0\0", "\0\0"))


-- bug in 0.9
assert(m.match(('a' * #m.P'b'), "ab") == 2)
assert(not m.match(('a' * #m.P'b'), "a"))

assert(not m.match(#m.S'567', ""))
assert(m.match(#m.S'567' * 1, "6") == 2)



-- this grammar does need backtracking info.
local lim = 10000
p = m.P{ '0' * m.V(1) + '0' }
checkerr("stack overflow", m.match, p, string.rep("0", lim))
m.setmaxstack(2*lim)
checkerr("stack overflow", m.match, p, string.rep("0", lim))
m.setmaxstack(2*lim + 4)
assert(m.match(p, string.rep("0", lim)) == lim + 1)

-- this repetition should not need stack space (only the call does)
p = m.P{ ('a' * m.V(1))^0 * 'b' + 'c' }
m.setmaxstack(200)
assert(p:match(string.rep('a', 180) .. 'c' .. string.rep('b', 180)) == 362)

m.setmaxstack(100)   -- restore low limit

-- tests for optional start position
assert(m.match("a", "abc", 1))
assert(m.match("b", "abc", 2))
assert(m.match("c", "abc", 3))
assert(not m.match(1, "abc", 4))
assert(m.match("a", "abc", -3))
assert(m.match("b", "abc", -2))
assert(m.match("c", "abc", -1))
assert(m.match("abc", "abc", -4))   -- truncate to position 1

assert(m.match("", "abc", 10))   -- empty string is everywhere!
assert(m.match("", "", 10))
assert(not m.match(1, "", 1))
assert(not m.match(1, "", -1))
assert(not m.match(1, "", 0))

print("+")


-- tests for Lua functions

t = {}
s = ""
p = m.P(function (s1, i) assert(s == s1); t[#t + 1] = i; return nil end) * false
s = "hi, this is a test"
assert(m.match(((p - m.P(-1)) + 2)^0, s) == string.len(s) + 1)
assert(#t == string.len(s)/2 and t[1] == 1 and t[2] == 3)

assert(not m.match(p, s))

p = mt.__add(function (s, i) return i end, function (s, i) return nil end)
assert(m.match(p, "alo"))

p = mt.__mul(function (s, i) return i end, function (s, i) return nil end)
assert(not m.match(p, "alo"))


t = {}
p = function (s1, i) assert(s == s1); t[#t + 1] = i; return i end
s = "hi, this is a test"
assert(m.match((m.P(1) * p)^0, s) == string.len(s) + 1)
assert(#t == string.len(s) and t[1] == 2 and t[2] == 3)

t = {}
p = m.P(function (s1, i) assert(s == s1); t[#t + 1] = i;
                         return i <= s1:len() and i end) * 1
s = "hi, this is a test"
assert(m.match(p^0, s) == string.len(s) + 1)
assert(#t == string.len(s) + 1 and t[1] == 1 and t[2] == 2)

p = function (s1, i) return m.match(m.P"a"^1, s1, i) end
assert(m.match(p, "aaaa") == 5)
assert(m.match(p, "abaa") == 2)
assert(not m.match(p, "baaa"))

checkerr("invalid position", m.match, function () return 2^20 end, s)
checkerr("invalid position", m.match, function () return 0 end, s)
checkerr("invalid position", m.match, function (s, i) return i - 1 end, s)
checkerr("invalid position", m.match,
             m.P(1)^0 * function (_, i) return i - 1 end, s)
assert(m.match(m.P(1)^0 * function (_, i) return i end * -1, s))
checkerr("invalid position", m.match,
             m.P(1)^0 * function (_, i) return i + 1 end, s)
assert(m.match(m.P(function (s, i) return s:len() + 1 end) * -1, s))
checkerr("invalid position", m.match, m.P(function (s, i) return s:len() + 2 end) * -1, s)
assert(not m.match(m.P(function (s, i) return s:len() end) * -1, s))
assert(m.match(m.P(1)^0 * function (_, i) return true end, s) ==
       string.len(s) + 1)
for i = 1, string.len(s) + 1 do
  assert(m.match(function (_, _) return i end, s) == i)
end

p = (m.P(function (s, i) return i%2 == 0 and i end) * 1
  +  m.P(function (s, i) return i%2 ~= 0 and i + 2 <= s:len() and i end) * 3)^0
  * -1
assert(p:match(string.rep('a', 14000)))

-- tests for Query Replacements


-- long strings for string capture
l = 10000
s = string.rep('a', l) .. string.rep('b', l) .. string.rep('c', l)

p = (m.C(m.P'a'^1) * m.C(m.P'b'^1) * m.C(m.P'c'^1)) / '%3%2%1'

assert(p:match(s) == string.rep('c', l) ..
                     string.rep('b', l) .. 
                     string.rep('a', l))

print"+"



-- tests for loop checker

local function isnullable (p)
  checkerr("may accept empty string", function (p) return p^0 end, m.P(p))
end

local function find (p, s)
  return m.match(basiclookfor(p), s)
end


local function badgrammar (g, expected)
  local stat, msg = pcall(m.P, g)
  assert(not stat)
  if expected then assert(find(expected, msg)) end
end

-- good x bad grammars
m.P{ ('a' * m.V(1))^-1 }
m.P{ -('a' * m.V(1)) }
m.P{ ('abc' * m.V(1))^-1 }
m.P{ -('abc' * m.V(1)) }
badgrammar{ #m.P('abc') * m.V(1) }
badgrammar{ -('a' + m.V(1)) }
m.P{ #('a' * m.V(1)) }
badgrammar{ #('a' + m.V(1)) }
m.P{ m.B{ m.P'abc' } * 'a' * m.V(1) }
badgrammar{ m.B{ m.P'abc' } * m.V(1) }
badgrammar{ ('a' + m.P'bcd')^-1 * m.V(1) }


-- simple tests for maximum sizes:
local p = m.P"a"
for i=1,14 do p = p * p end

p = {}
for i=1,100 do p[i] = m.P"a" end
p = m.P(p)


-- strange values for rule labels

p = m.P{ "print",
     print = m.V(print),
     [print] = m.V(_G),
     [_G] = m.P"a",
   }

assert(p:match("a"))

-- initial rule
g = {}
for i = 1, 10 do g["i"..i] =  "a" * m.V("i"..i+1) end
g.i11 = m.P""
for i = 1, 10 do
  g[1] = "i"..i
  local p = m.P(g)
  assert(p:match("aaaaaaaaaaa") == 11 - i + 1)
end

print"+"


t = {}
function foo (p) t[#t + 1] = p; return p .. "x" end

p = m.Cg(m.C(2)    / foo, "x") * m.Cb"x" *
    m.Cg(m.Cb('x') / foo, "x") * m.Cb"x" *
    m.Cg(m.Cb('x') / foo, "x") * m.Cb"x" *
    m.Cg(m.Cb('x') / foo, "x") * m.Cb"x"
x = {p:match'ab'}
checkeq(x, {'abx', 'abxx', 'abxxx', 'abxxxx'})
checkeq(t, {'ab',
            'ab', 'abx',
            'ab', 'abx', 'abxx',
            'ab', 'abx', 'abxx', 'abxxx'})



-- tests for match-time captures






-- old bug: optimization of concat with fail removed match-time capture
p = m.Cmt(0, function (s) p = s end) * m.P(false)
assert(not p:match('alo'))
assert(p == 'alo')


-- ensure that failed match-time captures are not kept on Lua stack
do
  local t = {__mode = "kv"}; setmetatable(t,t)
  local c = 0

  local function foo (s,i)
    collectgarbage();
    assert(next(t) == "__mode" and next(t, "__mode") == nil)
    local x = {}
    t[x] = true
    c = c + 1
    return i, x
  end

  local p = m.P{ m.Cmt(0, foo) * m.P(false) + m.P(1) * m.V(1) + m.P"" }
  p:match(string.rep('1', 10))
  assert(c == 11)
end


-- Return a match-time capture that returns 'n' captures
local function manyCmt (n)
    return m.Cmt("a", function ()
             local a = {}; for i = 1, n do a[i] = n - i end
             return true, unpack(a)
           end)
end

-- bug in 1.0: failed match-time that used previous match-time results
do
  local x
  local function aux (...) x = #{...}; return false end
  local res = {m.match(m.Cmt(manyCmt(20), aux) + manyCmt(10), "a")}
  assert(#res == 10 and res[1] == 9 and res[10] == 0)
end


-- bug in 1.0: problems with math-times returning too many captures
do
  local lim = 2^11 - 10
  local res = {m.match(manyCmt(lim), "a")}
  assert(#res == lim and res[1] == lim - 1 and res[lim] == 0)
  checkerr("too many", m.match, manyCmt(2^15), "a")
end


-------------------------------------------------------------------
-- Tests for 're' module
-------------------------------------------------------------------

local re = require "re"

local match, compile = re.match, re.compile





-- re.find discards any captures




p = re.compile[[
  block <- {| {:ident:space*:} line
           ((=ident !space line) / &(=ident space) block)* |}
  line <- {[^%nl]*} %nl
  space <- '_'     -- should be ' ', but '_' is simpler for editors
]]

t= p:match[[
1
__1.1
__1.2
____1.2.1
____n
2
__2.1
]]
checkeq(t, {"1", {"1.1", "1.2", {"1.2.1", "", ident = "____"}, ident = "__"},
            "2", {"2.1", ident = "__"}, ident = ""})


-- testing pre-defined names
assert(os.setlocale("C") == "C")

function eqlpeggsub (p1, p2)
  local s1 = cs2str(re.compile(p1))
  local s2 = string.gsub(allchar, "[^" .. p2 .. "]", "")
  -- if s1 ~= s2 then print(#s1,#s2) end
  assert(s1 == s2)
end


eqlpeggsub("%w", "%w")
eqlpeggsub("%a", "%a")
eqlpeggsub("%l", "%l")
eqlpeggsub("%u", "%u")
eqlpeggsub("%p", "%p")
eqlpeggsub("%d", "%d")
eqlpeggsub("%x", "%x")
eqlpeggsub("%s", "%s")
eqlpeggsub("%c", "%c")

eqlpeggsub("%W", "%W")
eqlpeggsub("%A", "%A")
eqlpeggsub("%L", "%L")
eqlpeggsub("%U", "%U")
eqlpeggsub("%P", "%P")
eqlpeggsub("%D", "%D")
eqlpeggsub("%X", "%X")
eqlpeggsub("%S", "%S")
eqlpeggsub("%C", "%C")

eqlpeggsub("[%w]", "%w")
eqlpeggsub("[_%w]", "_%w")
eqlpeggsub("[^%w]", "%W")
eqlpeggsub("[%W%S]", "%W%S")

re.updatelocale()




-- testing error messages in re

local function errmsg (p, err)
  checkerr(err, re.compile, p)
end

errmsg('aaaa', "rule 'aaaa'")
errmsg('a', 'outside')
errmsg('b <- a', 'undefined')
errmsg("x <- 'a'  x <- 'b'", 'already defined')
errmsg("'a' -", "near '-'")


print"OK"


