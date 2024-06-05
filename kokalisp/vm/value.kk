module kokalisp/vm/value

import kokalisp/sexp
import kokalisp/vm/syntax

pub alias value = sexp<native>

pub type native
  NSyntax(impl : syntax)

pub fun native/show(n : native) : string
  match n
    NSyntax(_) -> "<syntax>"
