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





do   -- match-time captures cannot be optimized away
  local touch = 0
  f = m.P(function () touch = touch + 1; return true end)

  local function check(n) n = n or 1; assert(touch == n); touch = 0 end

  assert(m.match(f * false + 'b', 'a') == nil); check()
  assert(m.match(f * false + 'b', '') == nil); check()
  assert(m.match( (f * 'a')^0 * 'b', 'b') == 2); check()
  assert(m.match( (f * 'a')^0 * 'b', '') == nil); check()
  assert(m.match( (f * 'a')^-1 * 'b', 'b') == 2); check()
  assert(m.match( (f * 'a')^-1 * 'b', '') == nil); check()
  assert(m.match( ('b' + f * 'a')^-1 * 'b', '') == nil); check()
  assert(m.match( (m.P'b'^-1 * f * 'a')^-1 * 'b', '') == nil); check()
  assert(m.match( (-m.P(1) * m.P'b'^-1 * f * 'a')^-1 * 'b', '') == nil);
     check()
  assert(m.match( (f * 'a' + 'b')^-1 * 'b', '') == nil); check()
  assert(m.match(f * 'a' + f * 'b', 'b') == 2); check(2)
  assert(m.match(f * 'a' + f * 'b', 'a') == 2); check(1)
  assert(m.match(-f * 'a' + 'b', 'b') == 2); check(1)
  assert(m.match(-f * 'a' + 'b', '') == nil); check(1)
end


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

p = (m.P(function () return true, "a" end) * 'a'
  + m.P(function (s, i) return i, "aa", 20 end) * 'b'
  + m.P(function (s,i) if i <= #s then return i, "aaa" end end) * 1)^0

t = {p:match('abacc')}
checkeq(t, {'a', 'aa', 20, 'a', 'aaa', 'aaa'})


-------------------------------------------------------------------
-- Tests for 're' module
-------------------------------------------------------------------

local re = require "re"

local match, compile = re.match, re.compile



assert(match("a", ".") == 2)
assert(match("a", "''") == 1)
assert(match("", " ! . ") == 1)
assert(not match("a", " ! . "))
assert(match("abcde", "  ( . . ) * ") == 5)
assert(match("abbcde", " [a-c] +") == 5)
assert(match("0abbc1de", "'0' [a-c]+ '1'") == 7)
assert(match("0zz1dda", "'0' [^a-c]+ 'a'") == 8)
assert(match("abbc--", " [a-c] + +") == 5)
assert(match("abbc--", " [ac-] +") == 2)
assert(match("abbc--", " [-acb] + ") == 7)
assert(not match("abbcde", " [b-z] + "))
assert(match("abb\"de", '"abb"["]"de"') == 7)
assert(match("abceeef", "'ac' ? 'ab' * 'c' { 'e' * } / 'abceeef' ") == "eee")
assert(match("abceeef", "'ac'? 'ab'* 'c' { 'f'+ } / 'abceeef' ") == 8)

assert(re.match("aaand", "[a]^2") == 3)

local t = {match("abceefe", "( ( & 'e' {} ) ? . ) * ")}
checkeq(t, {4, 5, 7})
local t = {match("abceefe", "((&&'e' {})? .)*")}
checkeq(t, {4, 5, 7})
local t = {match("abceefe", "( ( ! ! 'e' {} ) ? . ) *")}
checkeq(t, {4, 5, 7})
local t = {match("abceefe", "(( & ! & ! 'e' {})? .)*")}
checkeq(t, {4, 5, 7})

assert(match("cccx" , "'ab'? ('ccc' / ('cde' / 'cd'*)? / 'ccc') 'x'+") == 5)
assert(match("cdx" , "'ab'? ('ccc' / ('cde' / 'cd'*)? / 'ccc') 'x'+") == 4)
assert(match("abcdcdx" , "'ab'? ('ccc' / ('cde' / 'cd'*)? / 'ccc') 'x'+") == 8)

assert(match("abc", "a <- (. a)?") == 4)
b = "balanced <- '(' ([^()] / balanced)* ')'"
assert(match("(abc)", b))
assert(match("(a(b)((c) (d)))", b))
assert(not match("(a(b ((c) (d)))", b))

b = compile[[  balanced <- "(" ([^()] / balanced)* ")" ]]
assert(b == m.P(b))
assert(b:match"((((a))(b)))")

local g = [[
  S <- "0" B / "1" A / ""   -- balanced strings
  A <- "0" S / "1" A A      -- one more 0
  B <- "1" S / "0" B B      -- one more 1
]]
assert(match("00011011", g) == 9)

local g = [[
  S <- ("0" B / "1" A)*
  A <- "0" / "1" A A
  B <- "1" / "0" B B
]]
assert(match("00011011", g) == 9)
assert(match("000110110", g) == 9)
assert(match("011110110", g) == 3)
assert(match("000110010", g) == 1)

s = "aaaaaaaaaaaaaaaaaaaaaaaa"
assert(match(s, "'a'^3") == 4)
assert(match(s, "'a'^0") == 1)
assert(match(s, "'a'^+3") == s:len() + 1)
assert(not match(s, "'a'^+30"))
assert(match(s, "'a'^-30") == s:len() + 1)
assert(match(s, "'a'^-5") == 6)
for i = 1, s:len() do
  assert(match(s, string.format("'a'^+%d", i)) >= i + 1)
  assert(match(s, string.format("'a'^-%d", i)) <= i + 1)
  assert(match(s, string.format("'a'^%d", i)) == i + 1)
end
assert(match("01234567890123456789", "[0-9]^3+") == 19)


assert(match("01234567890123456789", "({....}{...}) -> '%2%1'") == "4560123")
t = match("0123456789", "{| {.}* |}")
checkeq(t, {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9"})
assert(match("012345", "{| (..) -> '%0%0' |}")[1] == "0101")

assert(match("abcdef", "( {.} {.} {.} {.} {.} ) -> 3") == "c")
assert(match("abcdef", "( {:x: . :} {.} {.} {.} {.} ) -> 3") == "d")
assert(match("abcdef", "( {:x: . :} {.} {.} {.} {.} ) -> 0") == 6)

assert(not match("abcdef", "{:x: ({.} {.} {.}) -> 2 :} =x"))
assert(match("abcbef", "{:x: ({.} {.} {.}) -> 2 :} =x"))

-- tests for comments in 're'
e = compile[[
A  <- _B   -- \t \n %nl .<> <- -> --
_B <- 'x'  --]]
assert(e:match'xy' == 2)

-- tests for 're' with pre-definitions
defs = {digits = m.R"09", letters = m.R"az", _=m.P"__"}
e = compile("%letters (%letters / %digits)*", defs)
assert(e:match"x123" == 5)
e = compile("%_", defs)
assert(e:match"__" == 3)

e = compile([[
  S <- A+
  A <- %letters+ B
  B <- %digits+
]], defs)

e = compile("{[0-9]+'.'?[0-9]*} -> sin", math)
assert(e:match("2.34") == math.sin(2.34))


function eq (_, _, a, b) return a == b end

c = re.compile([[
  longstring <- '[' {:init: '='* :} '[' close
  close <- ']' =init ']' / . close
]])

assert(c:match'[==[]]===]]]]==]===[]' == 17)
assert(c:match'[[]=]====]=]]]==]===[]' == 14)
assert(not c:match'[[]=]====]=]=]==]===[]')

c = re.compile" '[' {:init: '='* :} '[' (!(']' =init ']') .)* ']' =init ']' !. "

assert(c:match'[==[]]===]]]]==]')
assert(c:match'[[]=]====]=][]==]===[]]')
assert(not c:match'[[]=]====]=]=]==]===[]')

assert(re.find("hi alalo", "{:x:..:} =x") == 4)
assert(re.find("hi alalo", "{:x:..:} =x", 4) == 4)
assert(not re.find("hi alalo", "{:x:..:} =x", 5))
assert(re.find("hi alalo", "{'al'}", 5) == 6)
assert(re.find("hi aloalolo", "{:x:..:} =x") == 8)
assert(re.find("alo alohi x x", "{:word:%w+:}%W*(=word)!%w") == 11)

-- re.find discards any captures
local a,b,c = re.find("alo", "{.}{'o'}")
assert(a == 2 and b == 3 and c == nil)

local function match (s,p)
  local i,e = re.find(s,p)
  if i then return s:sub(i, e) end
end
assert(match("alo alo", '[a-z]+') == "alo")
assert(match("alo alo", '{:x: [a-z]+ :} =x') == nil)
assert(match("alo alo", "{:x: [a-z]+ :} ' ' =x") == "alo alo")

assert(re.gsub("alo alo", "[abc]", "x") == "xlo xlo")
assert(re.gsub("alo alo", "%w+", ".") == ". .")
assert(re.gsub("hi, how are you", "[aeiou]", string.upper) ==
               "hI, hOw ArE yOU")

s = 'hi [[a comment[=]=] ending here]] and [=[another]]=]]'
c = re.compile" '[' {:i: '='* :} '[' (!(']' =i ']') .)* ']' { =i } ']' "
assert(re.gsub(s, c, "%2") == 'hi  and =]')
assert(re.gsub(s, c, "%0") == s)
assert(re.gsub('[=[hi]=]', c, "%2") == '=')

assert(re.find("", "!.") == 1)
assert(re.find("alo", "!.") == 4)

function addtag (s, i, t, tag) t.tag = tag; return i, t end

c = re.compile([[
  doc <- block !.
  block <- (start {| (block / { [^<]+ })* |} end?) => addtag
  start <- '<' {:tag: [a-z]+ :} '>'
  end <- '</' { =tag } '>'
]], {addtag = addtag})

x = c:match[[
<x>hi<b>hello</b>but<b>totheend</x>]]
checkeq(x, {tag='x', 'hi', {tag = 'b', 'hello'}, 'but',
                     {'totheend'}})


-- test for folding captures
c = re.compile([[
  S <- (number (%s+ number)*) ~> add
  number <- %d+ -> tonumber
]], {tonumber = tonumber, add = function (a,b) return a + b end})
assert(c:match("3 401 50") == 3 + 401 + 50)

-- tests for look-ahead captures
x = {re.match("alo", "&(&{.}) !{'b'} {&(...)} &{..} {...} {!.}")}
checkeq(x, {"", "alo", ""})

assert(re.match("aloalo",
   "{~ (((&'al' {.}) -> 'A%1' / (&%l {.}) -> '%1%1') / .)* ~}")
       == "AallooAalloo")

-- bug in 0.9 (and older versions), due to captures in look-aheads
x = re.compile[[   {~ (&(. ([a-z]* -> '*')) ([a-z]+ -> '+') ' '*)* ~}  ]]
assert(x:match"alo alo" == "+ +")

-- valid capture in look-ahead (used inside the look-ahead itself)
x = re.compile[[
      S <- &({:two: .. :} . =two) {[a-z]+} / . S
]]
assert(x:match("hello aloaLo aloalo xuxu") == "aloalo")


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
____
2
__2.1
]]
checkeq(t, {"1", {"1.1", "1.2", {"1.2.1", "", ident = "____"}, ident = "__"},
            "2", {"2.1", ident = "__"}, ident = ""})


-- nested grammars
p = re.compile[[
       s <- a b !.
       b <- ( x <- ('b' x)? )
       a <- ( x <- 'a' x? )
]]

assert(p:match'aaabbb')
assert(p:match'aaa')
assert(not p:match'bbb')
assert(not p:match'aaabbba')

-- testing groups
t = {re.match("abc", "{:S <- {:.:} {S} / '':}")}
checkeq(t, {"a", "bc", "b", "c", "c", ""})

t = re.match("1234", "{| {:a:.:} {:b:.:} {:c:.{.}:} |}")
checkeq(t, {a="1", b="2", c="4"})
t = re.match("1234", "{|{:a:.:} {:b:{.}{.}:} {:c:{.}:}|}")
checkeq(t, {a="1", b="2", c="4"})
t = re.match("12345", "{| {:.:} {:b:{.}{.}:} {:{.}{.}:} |}")
checkeq(t, {"1", b="2", "4", "5"})
t = re.match("12345", "{| {:.:} {:{:b:{.}{.}:}:} {:{.}{.}:} |}")
checkeq(t, {"1", "23", "4", "5"})
t = re.match("12345", "{| {:.:} {{:b:{.}{.}:}} {:{.}{.}:} |}")
checkeq(t, {"1", "23", "4", "5"})


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


-- testing nested substitutions x string captures

p = re.compile[[
      text <- {~ item* ~}
      item <- macro / [^()] / '(' item* ')'
      arg <- ' '* {~ (!',' item)* ~}
      args <- '(' arg (',' arg)* ')'
      macro <- ('apply' args) -> '%1(%2)'
             / ('add' args) -> '%1 + %2'
             / ('mul' args) -> '%1 * %2'
]]

assert(p:match"add(mul(a,b), apply(f,x))" == "a * b + f(x)")

rev = re.compile[[ R <- (!.) -> '' / ({.} R) -> '%2%1']]

assert(rev:match"0123456789" == "9876543210")


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


