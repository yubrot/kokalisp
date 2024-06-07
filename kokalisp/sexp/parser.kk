module kokalisp/sexp/parser

import std/text/parse
import std/num/float64
import kokalisp/sexp

pub fun parse-sexp(input : string) : div either<string, sexp<a>>
  parse-all(input.slice, sexp).either

pub fun parse-program(input : string) : div either<string, list<sexp<a>>>
  // Unfortunately, Koka compiler v3.1.2 results in a segmentation fault if the
  // parser input is too large. Therefore, we split the input in an ad hoc manner.
  fun split-input-ad-hoc(input-lines : list<string>, acc : string)
    match input-lines
      Cons(line, lines) | line.starts-with("(").is-just ->
        Cons(acc, split-input-ad-hoc(lines, line ++ "\n"))
      Cons(line, lines) ->
        split-input-ad-hoc(lines, acc ++ line ++ "\n")
      Nil ->
        [acc]

  with handler
    return(r) Right(r)
    final ctl throw-exn(e) Left(e.message)

  split-input-ad-hoc(input.split("\n"), "").flatmap fn(input-part)
    match parse-all(input-part.slice, program)
      ParseOk(ss, _) -> ss
      ParseError(e, _) -> throw(e)

fun parse-all(input : sslice, p: () -> <parse|e> a) : e parse-error<a>
  parse-eof(input) { lead(amb, p) }

fun program() : <parse | _> list<sexp<a>>
  many(sexp)

fun sexp() : <parse | _> sexp<a>
  fun list-inner()
    optional(SNil)
      val init = many1(sexp)
      val last = optional(SNil) { term("."); sexp() }
      init.foldr(last, SCons)
  fun parens-list() between { term("(") } { term(")") } { list-inner() }
  fun brackets-list() between { term("[") } { term("]") } { list-inner() }
  fun quote() lead { term("'") } { sexp/quote(sexp()) }
  fun quasiquote() lead { term("`") } { sexp/quasiquote(sexp()) }
  fun unquote() lead { term(",") } { sexp/unquote(sexp()) }
  fun unquote-splicing() lead { term(",@") } { sexp/unquote-splicing(sexp()) }
  fun num() SNum(lex(number))
  fun sym() SSym(lex(symbol))
  fun str() SStr(lex(string))
  fun true() lead { term("#t") } { SBool(True) }
  fun false() lead { term("#f") } { SBool(False) }
  choose([parens-list, brackets-list, quote, quasiquote, unquote-splicing, unquote, num, sym, str, true, false])

fun number() : parse float64
  fun sign() one-of-or("+-", '+').string
  fun digits() chars-are("digit", is-digit).string
  fun frac() pstring(".") ++ digits()
  fun exp() one-of("eE").string ++ one-of-or("+-", '+').string ++ digits()
  val num = sign() ++ digits() ++ optional("", frac) ++ optional("", exp)
  match num.parse-float64
    Just(n) -> n
    Nothing -> fail("Unsupported number: " ++ num)

fun symbol() : parse string
  fun is-special(c : char) "!$%&*+-./:<=>?@^_~".contains(c.string)
  fun first() chars-are("symbol", fn(c) is-alpha(c) || is-special(c)).string
  fun rest() chars-are("symbol", fn(c) is-alpha(c) || is-special(c) || is-digit(c)).string
  val sym = first() ++ optional("", rest)
  if sym == "." then fail("Cannot use '.' as a symbol")
  sym

fun string() : parse string
  fun chars() chars-are("chars", fn(c) c != '"' && c != '\\').string
  fun escape-seq() char('\\').string ++ one-of("\\tn\"").string
  between { char('"') } { char('"') }
    val s = many { choose([chars, escape-seq]) }.join("")
    match try({ s.kokalisp-unescape })
      Ok(unescaped-s) -> unescaped-s
      Error(e) -> fail(e.message)

fun term(s: string) : parse ()
  lex { pstring(s) }.ignore

// -------------------
// lex

fun lex(p : () -> parse a) : parse a
  follow(p, amb)

fun amb() : parse ()
  many { choose([whitespace, comment]).ignore }.ignore

fun comment() : parse string
  char(';')
  optional("") { chars-are("comment", fn(c) c != '\n').string }

// -------------------

// l *> x <* r
pub fun between(l : () -> e l, r : () -> e r, x : () -> e a) : e a
  l()
  val a = x()
  r()
  a

// l *> x
pub fun lead(l : () -> e l, x : () -> e a) : e a
  l()
  x()

// x <* r
pub fun follow(x : () -> e a, r : () -> e r) : e a
  val a = x()
  r()
  a
