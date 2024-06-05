// list<a> utilities
module kokalisp/support/list

pub fun lookup(list : list<(k, a)>, k : k, ?k/(==): (l : k, r : k) -> bool) : maybe<a>
  match list
    Cons(entry, entries) ->
      if entry.fst == k
        then Just(entry.snd)
        else entries.lookup(k)
    Nil ->
      Nothing

pub fun insert(list : list<(k, a)>, k : k, a : a, ?k/(==): (l : k, r : k) -> bool) : list<(k, a)>
  match list
    Cons(entry, entries) ->
      if entry.fst == k
        then Cons((entry.fst, a), entries)
        else Cons(entry, entries.insert(k, a))
    Nil ->
      [(k, a)]
