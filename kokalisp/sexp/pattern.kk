// LISP functions pattern abstraction
// e.g.
// (a b c) -> Pattern(fixed=["a", "b", "c"], rest=Nothing)
// (x . y) -> Pattern(fixed=["x"], rest=Just("y"))
// xs      -> Pattern(fixed=[], rest=Just("xs"))
module kokalisp/sexp/pattern

import kokalisp/support/void
import kokalisp/sexp

pub value struct pattern
  fixed : list<string>
  rest : maybe<string>

pub fun show(p : pattern) : string
  show(p.fixed.map(SSym).foldr(p.rest.maybe(SNil, SSym), SCons) : sexp<void>)

pub fun expectation(p : pattern) : string
  (if p.rest.is-nothing then "" else "at least ") ++ p.fixed.length.show ++ " arguments"

pub fun bind(p : pattern, args: list<sexp<a>>) : maybe<list<(string, sexp<a>)>>
  val argc = args.length
  val expect = p.fixed.length
  if argc < expect || p.rest.is-nothing && expect < argc
    then Nothing
    else Just(p.fixed.zip(args) ++ maybe(p.rest, [], fn(rest) [(rest, args.drop(p.fixed.length).sexp)]))

pub fun sexp/pattern(pat : sexp<a>) : either<sexp<a>, pattern>
  match pat
    SSym(sym) ->
      Right(Pattern([], Just(sym)))
    SCons(SSym(sym), cdr) ->
      cdr.pattern.map fn(Pattern(fixed, rest))
        Pattern(Cons(sym, fixed), rest)
    SNil ->
      Right(Pattern([], Nothing))
    _ ->
      Left(pat)
