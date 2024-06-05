// Syntax in this language. Each corresponds to a specific VM machine code.
module kokalisp/vm/syntax

import kokalisp/sexp
import kokalisp/sexp/pattern
import kokalisp/vm/code

pub value struct syntax
  expand-pattern : list<bool>
  compile : forall<a> (args : list<sexp<a>>) -> compile-syntax<a> code

pub effect compile-syntax<a>
  fun compile-subpart(a : sexp<a>) : code
  final ctl syntax-error(msg : string) : b

pub val list : list<(string, syntax)> = [
  ("def", syntax/def),
  ("set!", syntax/set),
  ("begin", syntax/begin),
  ("if", syntax/if),
  ("fun", syntax/fun),
  ("macro", syntax/macro),
  ("builtin", syntax/builtin),
  ("quote", syntax/quote),
]

val syntax/def = Syntax([False, True]) fn(args)
  match args
    [SSym(sym), x] -> compile-subpart(x) ++ [Def(sym), Ldc(SNil)]
    _ -> syntax-error("expected (def sym x)")

val syntax/set = Syntax([False, True]) fn(args)
  match args
    [SSym(sym), x] -> compile-subpart(x) ++ [Set(sym), Ldc(SNil)]
    _ -> syntax-error("expected (set! sym x)")

val syntax/begin = Syntax([], compile-seq)

val syntax/if = Syntax([]) fn(args)
  match args
    [c, t, e] -> compile-subpart(c) ++ [Sel(compile-subpart(t) ++ [Leave], compile-subpart(e) ++ [Leave])]
    _ -> syntax-error("expected (if cond then else)")

val syntax/fun = Syntax([False]) fn(args)
  match args
    Cons(p, body) -> [Ldf(compile-pattern(p, "(fun pattern body...)"), compile-seq(body) ++ [Leave])]
    Nil -> syntax-error("expected (fun pattern body...)")

val syntax/macro = Syntax([False]) fn(args)
  match args
    Cons(p, body) -> [Ldm(compile-pattern(p, "(macro pattern body...)"), compile-seq(body))]
    Nil -> syntax-error("expected (macro pattern body...)")

val syntax/builtin = Syntax([False]) fn(args)
  match args
    [SSym(sym)] -> [Ldb(sym)]
    _ -> syntax-error("expected (builtin sym)")

val syntax/quote = Syntax([False]) fn(args)
  match args
    [s] ->
      val c = s.map fn(_) syntax-error("cannot embed non-S-expression values for (quote expr)")
      [Ldc(c)]
    _ -> syntax-error("expected (quote expr)")

fun compile-pattern(p : sexp<a>, form : string) : compile-syntax<a> pattern
  match p.pattern
    Right(pat) -> pat
    Left(s) -> syntax-error("unsupported pattern " ++ s.show(?pure/show = fn(_) "<...>") ++ " for " ++ form)

fun compile-seq(args : list<sexp<a>>) : compile-syntax<a> code
  match args
    [] -> [Ldc(SNil)]
    [x] -> compile-subpart(x)
    Cons(x, xs) -> compile-subpart(x) ++ [Pop] ++ compile-seq(xs)
