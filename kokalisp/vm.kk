module kokalisp/vm

import kokalisp/support/void
import kokalisp/sexp
import kokalisp/sexp/pattern
pub import kokalisp/vm/code
pub import kokalisp/vm/env
pub import kokalisp/vm/syntax
pub import kokalisp/vm/value

// Root of Kokalisp language processor state
pub value struct vm
  toplevel : env<value>

pub fun new() : stenv vm
  val toplevel = env/empty(Nothing)
  syntax/list.foreach fn((name, syn)) toplevel.define(name, SPure(NSyntax(syn)))
  Vm(toplevel)

pub fun refer(vm : vm, s : sexp<a>) : stenv maybe<value>
  match s
    SSym(sym) -> vm.toplevel.find(sym)
    _ -> Nothing

pub fun compile(vm : vm, s : sexp<a>, ?pure/show : a -> string) : <exn, stenv> code
  match s
    SSym(sym) -> [Ldv(sym)]
    SCons(_, _) -> match s.list
      Just(Cons(f, args)) -> match vm.refer(f)
        Just(SPure(NSyntax(syn))) ->
          with handler
            fun compile-subpart(a) vm.compile(a)
            final ctl syntax-error(msg) throw("Syntax error: " ++ msg)
          (syn.compile)(args)
        _ -> vm.compile(f) ++ args.flatmap(fn(arg) vm.compile(arg)) ++ [App(args.length)]
      _ -> throw("Improper list: " ++ s.show(?pure/show = fn(_) "<...>"))
    _ -> [Ldc(s.map fn(a) throw("Found " ++ a.show ++ " during compilation"))]
