module kokalisp/support/void

import std/core/unsafe

pub fun absurd(a : void) : a
  with unsafe-total
  throw("void/absurd")

pub fun show(a : void) : string
  absurd(a)
