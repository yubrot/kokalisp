module kokalisp/driver/builtins

import std/num/float64
import kokalisp/support/os
import kokalisp/sexp
import kokalisp/vm

pub val list : list<(string, builtin)> = [
  ("cons", builtin/cons),

  ("exit", builtin/exit),
  ("error", builtin/error),

  ("gensym", builtin/gensym),

  ("car", builtin/car),
  ("cdr", builtin/cdr),

  ("apply", builtin/apply),
]

val builtin : builtin -> builtin = id

val builtin/cons = builtin fn(args)
  match args
    [car, cdr] -> push(SCons(car, cdr))
    _ -> throw("cons takes two arguments")

val builtin/exit = builtin fn(args)
  match args
    [] -> exit(0)
    [SNum(n)] -> exit(n.int)
    _ -> throw("exit takes an exitcode")

val builtin/error = builtin fn(args)
  match args
    [] -> throw("error called")
    [SStr(s)] -> throw(s)
    _ -> throw("error takes an error message")

val builtin/gensym = builtin fn(args)
  match args
    [] -> push(SSym(gensym()))
    _ -> throw("gensym takes no argument")

val builtin/car = builtin fn(args)
  match args
    [SCons(a, _)] -> push(a)
    _ -> throw("car takes a cons")

val builtin/cdr = builtin fn(args)
  match args
    [SCons(_, a)] -> push(a)
    _ -> throw("cdr takes a cons")

val builtin/apply = builtin fn(args)
  val f-args = match args
    [x, xx] -> xx.list.map fn(xxs) (x, xxs)
    _ -> Nothing
  match f-args
    Just((f, xs)) -> value/apply(f, xs)
    Nothing -> throw("apply takes a function and an argument list")
