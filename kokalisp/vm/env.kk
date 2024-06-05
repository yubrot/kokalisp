// VM environment implementation. Supports key-value read write and scope chain.
module kokalisp/vm/env

import kokalisp/support/list

// Since We don't expect env to be used on the local heap, we define a short effect alias
pub alias stenv = <st<global>, div>

pub struct env<a>
  entries : ref<global, list<(string, a)>>
  parent : maybe<env<a>>

pub fun empty(parent : maybe<env<a>>) : stenv env<a>
  Env(ref([]), parent)

pub fun define(env : env<a>, k : string, v : a) : stenv ()
  set(env.entries, (!env.entries).insert(k, v))

pub fun find(env : env<a>, k : string) : stenv maybe<a>
  match (!env.entries).lookup(k)
    Just(v) -> Just(v)
    Nothing ->
      with parent <- maybe(env.parent, Nothing)
      parent.find(k)

pub fun set(env : env<a>, k : string, v : a) : stenv maybe<a>
  match (!env.entries).lookup(k)
    Just(prev-v) ->
      set(env.entries, (!env.entries).insert(k, v))
      Just(prev-v)
    Nothing ->
      with parent <- maybe(env.parent, Nothing)
      parent.set(k, v)

