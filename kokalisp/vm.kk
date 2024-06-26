module kokalisp/vm

import kokalisp/support/void
import kokalisp/support/raise
import kokalisp/sexp
import kokalisp/sexp/pattern
pub import kokalisp/vm/code
pub import kokalisp/vm/env
pub import kokalisp/vm/syntax
pub import kokalisp/vm/value

// Root of the Kokalisp language processor state
pub value struct vm
  toplevel : env<value>
  id-gen : ref<global, int>
  builtins : builtin-registry

pub fun new(builtins : list<(string, builtin)>) : stenv vm
  val toplevel = env/empty(Nothing)
  syntax/list.foreach fn((name, syn)) toplevel.define(name, SPure(NSyntax(syn)))
  Vm(toplevel, ref(0), builtin-registry(builtins))

pub fun refer(vm : vm, s : sexp<a>) : stenv maybe<value>
  match s
    SSym(sym) -> vm.toplevel.find(sym)
    _ -> Nothing

pub fun compile(vm : vm, s : sexp<a>) : stenv either<(sexp<a>, string), code>
  with raise/either
  vm.compile-step(s)

pub fun macroexpand(vm : vm, value : value, recurse : bool = True) : io-noexn either<eval-error, value>
  with raise/either
  vm.macroexpand-step(value, recurse)

pub fun eval(vm : vm, value : value) : io-noexn either<eval-error, value>
  with raise/either
  val macroexpanded-value = vm.macroexpand-step(value)
  val code = raise/map(CompileError) { vm.compile-step(macroexpanded-value) }
  vm.exec(code)

pub fun eval-all(vm : vm, values : list<value>) : io-noexn either<eval-error, value>
  match values
    Cons(v, Nil) -> vm.eval(v)
    Cons(v, vv) -> match vm.eval(v)
      Right(_) -> vm.eval-all(vv)
      Left(e) -> Left(e)
    Nil -> Right(SNil)

pub value type eval-error
  CompileError(error : (value, string))
  ExecError(msg : string)
  InternalError(msg : string)

pub fun eval-error/show(e : eval-error) : _ string
  match e
    CompileError((value, msg)) -> "Compile error: " ++ msg ++ ": " ++ value.show
    ExecError(msg) -> "Execution error: " ++ msg
    InternalError(msg) -> "Internal error: " ++ msg

// -------------------
// internals

alias eval = <raise<eval-error>, io-noexn>

fun next-id(vm : vm) : stenv int
  set(vm.id-gen, !vm.id-gen + 1)
  !vm.id-gen

fun compile-step(vm : vm, s : sexp<a>) : <raise<(sexp<a>, string)>, stenv> code
  match s
    SSym(sym) -> [Ldv(sym)]
    SCons(_, _) -> match s.list
      Just(Cons(f, args)) -> match vm.refer(f)
        Just(SPure(NSyntax(syn))) ->
          with handler
            fun compile-subpart(a) vm.compile-step(a)
            final ctl syntax-error(msg) raise((s, "Syntax error: " ++ msg))
          (syn.compile)(args)
        _ -> vm.compile-step(f) ++ args.flatmap(fn(arg) vm.compile-step(arg)) ++ [App(args.length)]
      _ -> raise((s, "Improper list"))
    _ -> [Ldc(s.map fn(a) raise((s, "Found non-S-expression")))]

fun macroexpand-step(vm : vm, value : value, recurse : bool = True) : eval value
  match value.list
    Just(Cons(m, args)) -> match vm.refer(m)
      Just(SPure(NMacro(menv, mpat, mbody))) ->
        val macroexec-env = env/empty(Just(menv))
        macroexec-env.bind-args(mpat, args)
        val expanded-value = vm.exec(mbody, macroexec-env)
        if recurse
          then vm.macroexpand-step(expanded-value)
          else expanded-value
      Just(SPure(NSyntax(syn))) ->
        if recurse
          then
            val expanded-args =
              syn.expand-pattern.zipwith(args, fn(p, arg) if p then vm.macroexpand-step(arg) else arg) ++
              args.drop(syn.expand-pattern.length).map(fn(p) vm.macroexpand-step(p))
            Cons(m, expanded-args).sexp
          else value
      _ -> vm.macroexpand-children(value, recurse)
    _ -> vm.macroexpand-children(value, recurse)

fun macroexpand-children(vm : vm, value : value, recurse : bool = True) : eval value
  if recurse
    then match value
      SCons(a, b) -> SCons(vm.macroexpand-step(a), vm.macroexpand-children(b))
      s -> s
    else value

fun exec(vm : vm, code : code, env : env<value> = vm.toplevel) : eval value
  Exec(ref([]), ref(env), ref(code), ref([]), vm).run-loop

// Root of the execution state
struct exec
  stack : ref<global, list<value>>
  env : ref<global, env<value>>
  code : ref<global, code>
  dump : ref<global, list<(env<value>, code)>>
  vm : vm

fun exec/run-loop(e : exec) : eval value
  match !e.code
    Cons(i, is) ->
      set(e.code, is)
      e.inst(i)
      e.run-loop
    [] -> e.pop

fun exec/inst(e : exec, i : inst) : eval ()
  match i
    Ldc(v) -> e.push(v.map(absurd))
    Ldv(k) -> match (!e.env).find(k)
      Just(v) -> e.push(v)
      Nothing -> raise(ExecError("Undefined variable: " ++ k))
    Ldf(pattern, code) -> e.push(SPure(NFun(!e.env, pattern, code)))
    Ldm(pattern, code) -> e.push(SPure(NMacro(!e.env, pattern, code)))
    Ldb(b) -> match e.vm.builtins.resolve(b)
      Just(id) -> e.push(SPure(NBuiltin(id)))
      Nothing -> raise(ExecError("Unsupported builtin: " ++ b))
    Sel(a, b) ->
      val branch-env = env/empty(Just(!e.env))
      val branch-code = if e.pop.test then a else b
      e.enter(branch-env, branch-code)
    App(argc) ->
      var args := []
      for(1, argc) fn(_) args := Cons(e.pop, args)
      val f = e.pop
      e.apply(f, args)
    Leave -> e.leave
    Pop -> e.pop.ignore
    Def(k) ->
      val v = e.pop
      (!e.env).define(k, v)
    Set(k) ->
      val v = e.pop
      match (!e.env).set(k, v)
        Just(_) -> ()
        Nothing -> raise(ExecError("Undefined variable: " ++ k))

fun exec/apply(e : exec, f : value, args : list<value>) : eval ()
  match f
    SPure(NFun(fenv, fpat, fbody)) ->
      val new-env = env/empty(Just(fenv))
      new-env.bind-args(fpat, args)
      e.enter(new-env, fbody)
    SPure(NBuiltin(id)) -> match e.vm.builtins.implementation(id)
      Just(impl) ->
        with final ctl throw-exn(exn) raise(ExecError(exn.show))
        with handler
          fun gensym() "#sym." ++ e.vm.next-id.show
          fun push(value) e.push(value)
          fun apply(f', args') e.apply(f', args')
          fun apply-never(f', args') e.apply-never(f', args')
          fun capture-cont() SPure(NCont(Cont(!e.stack, !e.env, !e.code, !e.dump)))
          fun eval(v) e.vm.eval(v).map-left(show)
          fun macroexpand(v, recurse) e.vm.macroexpand(v, recurse).map-left(show)
        impl(args)
      Nothing -> raise(InternalError("Unknown builtin: " ++ id.show))
    SPure(NCont(cont)) ->
      set(e.stack, cont.stack)
      set(e.env, cont.env)
      set(e.code, cont.code)
      set(e.dump, cont.dump)
      match args
        [] -> e.push(SNil)
        [x] -> e.push(x)
        _ -> raise(ExecError("Multiple values are not implemented"))
    _ -> raise(ExecError("Cannot call: " ++ f.show))

fun exec/apply-never(e : exec, f : value, args : list<value>) : eval ()
  set(e.stack, [])
  set(e.code, [Leave])
  set(e.dump, [])
  e.apply(f, args)

fun exec/push(e : exec, v : value) : eval ()
  set(e.stack, Cons(v, !e.stack))

fun exec/pop(e : exec) : eval value
  match !e.stack
    Cons(x, xs) -> { set(e.stack, xs); x }
    [] -> raise(InternalError("Inconsistent stack"))

fun exec/is-tail(e : exec) : eval bool
  // NOTE: inlining this match clause to exec/enter causes bus error (compiler bug)
  match !e.code
    [Leave] -> True
    _ -> False

fun exec/enter(e : exec, env' : env<value>, code' : code) : eval ()
  if !e.is-tail then set(e.dump, Cons((!e.env, !e.code), !e.dump))
  set(e.env, env')
  set(e.code, code')

fun exec/leave(e : exec) : eval ()
  match !e.dump
    Cons((env', code'), ds) ->
      set(e.dump, ds)
      set(e.env, env')
      set(e.code, code')
    [] -> raise(InternalError("Inconsistent stack"))

fun env/bind-args(env : env<value>, pat : pattern, args : list<value>) : eval ()
  match pat.bind(args)
    Just(binds) -> binds.foreach fn((k, v)) env.define(k, v)
    Nothing -> raise(ExecError("Arguments mismatch: expected " ++ pat.expectation))

fun either/map-left(e : either<a, b>, f : a -> e c) : e either<c, b>
  match e
    Left(l) -> Left(f(l))
    Right(r) -> Right(r)
