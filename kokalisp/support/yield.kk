// A generator effect.
module kokalisp/support/yield

pub effect yield<a>
  fun yield(x : a) : ()

pub fun for(generator : () -> <yield<a> | e> (), body : a -> e ()) : e ()
  handle generator
    fun yield(a) body(a)
