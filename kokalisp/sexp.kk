// Provides S-expression data type
module kokalisp/sexp

import std/core/undiv
import std/num/float64

pub type sexp<a>
  SNum(value : float64)
  SSym(value : string)
  SStr(value : string)
  SCons(car : sexp<a>, cdr : sexp<a>)
  SNil
  SBool(value : bool)
  SPure(value : a)

pub fun list/sexp(list : list<sexp<a>>) : sexp<a>
  list.foldr(SNil, SCons)

pub fun list(s : sexp<a>) : maybe<list<sexp<a>>>
  match s
    SNil -> Just([])
    SCons(car, cdr) -> cdr.list.map fn(ss) Cons(car, ss)
    _ -> Nothing

pub fun test(s : sexp<a>) : bool
  match s
    SBool(value) -> value
    _ -> True

pub fun map(s : sexp<a>, f : a -> e b) : e sexp<b>
  match s
    SNum(value) -> SNum(value)
    SSym(value) -> SSym(value)
    SStr(value) -> SStr(value)
    SCons(car, cdr) -> SCons(car.map(f), cdr.map(f))
    SNil -> SNil
    SBool(value) -> SBool(value)
    SPure(a) -> SPure(f(a))

pub fun quote(s)
  ["quote".SSym, s].sexp

pub fun quasiquote(s)
  ["quasiquote".SSym, s].sexp

pub fun unquote(s)
  ["unquote".SSym, s].sexp

pub fun unquote-splicing(s)
  ["unquote-splicing".SSym, s].sexp

pub fun show(s : sexp<a>, ?pure/show : a -> e string) : e string
  // FIXME: How to replace this pretend-no-div with pretend-decreasing?
  with pretend-no-div
  s.show-div

fun show-div(s : sexp<a>, ?pure/show : a -> e string) : <div | e> string
  match s
    SCons(SSym("quote"), SCons(s, SNil)) -> "'" ++ s.show-div
    SCons(SSym("quasiquote"), SCons(s, SNil)) -> "`" ++ s.show-div
    SCons(SSym("unquote"), SCons(s, SNil)) -> "," ++ s.show-div
    SCons(SSym("unquote-splicing"), SCons(s, SNil)) -> ",@" ++ s.show-div
    SNum(value) -> value.show
    SSym(value) -> value
    SStr(value) -> value.show
    SCons(car, cdr) -> "(" ++ car.show-div ++ cdr.show-cdr ++ ")"
    SNil -> "()"
    SBool(True) -> "#t"
    SBool(False) -> "#f"
    SPure(p) -> mask<div> { p.show }

fun show-cdr(s : sexp<a>, ?pure/show : a -> e string) : <div | e> string
  match s
    SNil -> ""
    SCons(car, cdr) -> " " ++ car.show-div ++ cdr.show-cdr
    _ -> " . " ++ s.show-div
