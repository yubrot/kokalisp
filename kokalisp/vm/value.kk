module kokalisp/vm/value

import kokalisp/sexp
import kokalisp/sexp/pattern
import kokalisp/vm/code
import kokalisp/vm/env
import kokalisp/vm/syntax

pub alias value = sexp<native>

pub type native
  // Use env<sexp<native>> instead of env<value> (compiler bug?)
  NFun(env : env<sexp<native>>, pattern : pattern, code : code)
  NMacro(env : env<sexp<native>>, pattern : pattern, code : code)
  NSyntax(impl : syntax)

pub fun native/show(n : native) : string
  match n
    NFun(_, _, _) -> "<fun>"
    NMacro(_, _, _) -> "<macro>"
    NSyntax(_) -> "<syntax>"
