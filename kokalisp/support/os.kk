module kokalisp/support/os

import std/num/int32

pub fun exit(exitcode : int) : div ()
  exit'(exitcode.i32)

inline fip extern exit'(_ : int32) : ()
  c "exit"
