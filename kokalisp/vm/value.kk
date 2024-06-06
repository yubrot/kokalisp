module kokalisp/vm/value

import kokalisp/sexp
import kokalisp/sexp/pattern
import kokalisp/vm/code
import kokalisp/vm/env
import kokalisp/vm/syntax

pub alias value = sexp<native>

pub type native
  NFun(env : env<value>, pattern : pattern, code : code)
  // To avoid using [recursive types](https://koka-lang.github.io/koka/doc/book.html#sec-inductive-co-inductive-and-recursive-types),
  // we don't use `builtin` directly, but use indirect references for now.
  // I'm encountering type errors when using recursive types, but I'm not sure if
  // it's a Koka's implementation bug, limitation, or a problem with the program.
  // It seems that recursive types with aliases introduce a problem?
  // https://github.com/koka-lang/koka/discussions/280
  NBuiltin(id : builtin-id)
  NCont(cont : cont)
  NMacro(env : env<value>, pattern : pattern, code : code)
  NSyntax(impl : syntax)
  NVec(items : vector<ref<global, value>>)

pub fun native/show(n : native) : <div, st<global>> string
  match n
    NFun(_, _, _) -> "<fun>"
    NBuiltin(_) -> "<builtin>"
    NCont(_) -> "<builtin>" // for Rosetta-Lisp compatibility
    NMacro(_, _, _) -> "<macro>"
    NSyntax(_) -> "<syntax>"
    NVec(items) ->
      val s = SCons(SSym("vec"), items.list.foldr(SNil) fn(a, b) SCons(!a, b))
      s.show

abstract value struct builtin-id
  index : int

pub fun builtin-id/show(id : builtin-id) : string
  "#" ++ id.index.show

pub struct cont
  stack : list<value>
  env : env<value>
  code : code
  dump : list<(env<value>, code)>

pub alias builtin = list<value> -> <builtin-op, io> ()

pub effect builtin-op
  fun gensym() : string
  fun push(v : value) : ()
  fun apply(f : value, args : list<value>) : ()
  fun apply-never(f : value, args : list<value>) : ()
  fun capture-cont() : value
  fun eval(v : value) : either<string, value>
  fun macroexpand(v : value, recurse : bool) : either<string, value>

// Registry of indirect references from builtin-id to builtin
pub value struct builtin-registry
  implementations : vector<builtin>
  symbols : list<string>

pub fun builtin-registry(list : list<(string, builtin)>) : builtin-registry
  Builtin-registry(list.map(snd).vector, list.map(fst))

pub fun builtin-registry/resolve(reg : builtin-registry, b : string) : maybe<builtin-id>
  match reg.symbols.index-of(fn(sym) sym == b)
    -1 -> Nothing
    index -> Just(Builtin-id(index))

pub fun builtin-registry/implementation(reg : builtin-registry, id : builtin-id) : maybe<builtin>
  reg.implementations.at(id.index)
