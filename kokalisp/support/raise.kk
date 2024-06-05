// A typed exception effect.
module kokalisp/support/raise

pub effect raise<a>
  final ctl raise(x : a) : b

pub fun either(action : () -> <raise<l> | e> r) : e either<l, r>
  with final ctl raise(l) Left(l)
  Right(action())

pub fun map(f : l -> <raise<m> | e> m, action : () -> <raise<l>, raise<m> | e> r) : <raise<m> | e> r
  with final ctl raise(l) raise(f(l))
  action()
