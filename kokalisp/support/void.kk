module kokalisp/support/void

import std/core/undiv

pub fun absurd(a : void) : a
  // this is safe since void has no constructor
  absurd(pretend-decreasing(a))

pub fun show(a : void) : string
  absurd(a)
