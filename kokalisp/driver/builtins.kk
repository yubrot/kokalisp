module kokalisp/driver/builtins

import std/num/float64
import std/os/file
import std/os/path
import std/os/readline
import kokalisp/support/os
import kokalisp/sexp
import kokalisp/sexp/pattern // NOTE: Why this is required (compiler bug?)
import kokalisp/vm

pub val list : list<(string, builtin)> = [
  ("cons", builtin/cons),

  ("exit", builtin/exit),
  ("error", builtin/error),

  ("gensym", builtin/gensym),

  ("car", builtin/car),
  ("cdr", builtin/cdr),

  ("apply", builtin/apply),

  ("num?", builtin/test-num),
  ("sym?", builtin/test-sym),
  ("str?", builtin/test-str),
  ("cons?", builtin/test-cons),
  ("nil?", builtin/test-nil),
  ("bool?", builtin/test-bool),
  ("proc?", builtin/test-proc),
  ("meta?", builtin/test-meta),
  ("vec?", builtin/test-vec),

  ("+", builtin/add),
  ("-", builtin/sub),
  ("*", builtin/mul),
  ("/", builtin/div),
  ("%", builtin/mod),

  ("=", builtin/eq),
  ("<", builtin/lt),
  (">", builtin/gt),
  ("<=", builtin/le),
  (">=", builtin/ge),

  ("call/cc", builtin/call-cc),
  ("never", builtin/never),

  ("str", builtin/str),
  ("str-char-at", builtin/str-char-at),
  ("str-length", builtin/str-length),
  ("str-concat", builtin/str-concat),
  ("substr", builtin/substr),
  ("sym->str", builtin/sym-to-str),
  ("num->str", builtin/num-to-str),
  ("str->num", builtin/str-to-num),

  ("vec", builtin/vec),
  ("vec-make", builtin/vec-make),
  ("vec-length", builtin/vec-length),
  ("vec-get", builtin/vec-get),
  ("vec-set!", builtin/vec-set),
  ("vec-copy!", builtin/vec-copy),

  ("read-file-text", builtin/read-file-text),
  ("write-file-text", builtin/write-file-text),
  ("read-console-line", builtin/read-console-line),
  ("write-console", builtin/write-console),

  ("eval", builtin/eval),
  ("macroexpand", builtin/macroexpand("macroexpand", True)),
  ("macroexpand-1", builtin/macroexpand("macroexpand-1", False)),
]

val builtin : builtin -> builtin = id

val builtin/cons = builtin fn(args)
  match args
    [car, cdr] -> push(SCons(car, cdr))
    _ -> throw("cons takes two arguments")

val builtin/exit = builtin fn(args)
  match args
    [] -> exit(0)
    [SNum(n)] -> exit(n.int)
    _ -> throw("exit takes an exitcode")

val builtin/error = builtin fn(args)
  match args
    [] -> throw("error called")
    [SStr(s)] -> throw(s)
    _ -> throw("error takes an error message")

val builtin/gensym = builtin fn(args)
  match args
    [] -> push(SSym(gensym()))
    _ -> throw("gensym takes no argument")

val builtin/car = builtin fn(args)
  match args
    [SCons(a, _)] -> push(a)
    _ -> throw("car takes a cons")

val builtin/cdr = builtin fn(args)
  match args
    [SCons(_, a)] -> push(a)
    _ -> throw("cdr takes a cons")

val builtin/apply = builtin fn(args)
  val f-args = match args
    [x, xx] -> xx.list.map fn(xxs) (x, xxs)
    _ -> Nothing
  match f-args
    Just((f, xs)) -> value/apply(f, xs)
    Nothing -> throw("apply takes a function and an argument list")

fun builtin-test(cond)
  builtin fn(args)
    match args
      [x] -> push(SBool(cond(x)))
      _ -> throw("cdr takes one argument")

val builtin/test-num = builtin-test fn(x) match x { SNum(_) -> True; _ -> False }
val builtin/test-sym = builtin-test fn(x) match x { SSym(_) -> True; _ -> False }
val builtin/test-str = builtin-test fn(x) match x { SStr(_) -> True; _ -> False }
val builtin/test-cons = builtin-test fn(x) match x { SCons(_, _) -> True; _ -> False }
val builtin/test-nil = builtin-test fn(x) match x { SNil -> True; _ -> False }
val builtin/test-bool = builtin-test fn(x) match x { SBool(_) -> True; _ -> False }
val builtin/test-proc = builtin-test fn(x) match x { SPure(NFun(_)) -> True; SPure(NBuiltin(_)) -> True; SPure(NCont(_)) -> True; _ -> False }
val builtin/test-meta = builtin-test fn(x) match x { SPure(NMacro(_)) -> True; SPure(NSyntax(_)) -> True; _ -> False }
val builtin/test-vec = builtin-test fn(x) match x { SPure(NVec(_)) -> True; _ -> False }

fun builtin-arithmetic(op, zero, one, cat)
  val zero' = match zero
    Nothing -> fn() throw(op ++ " takes at least one argument")
    Just(z) -> fn() z
  builtin fn(args)
    val nums = args.map fn(arg)
      match arg
        SNum(n) -> n
        _ -> throw(op ++ " takes numbers")
    val result =
      match nums
        [] -> zero'()
        [o] -> one(o)
        Cons(n, ns) -> ns.foldl(n, cat)
    push(SNum(result))

val builtin/add = builtin-arithmetic("+", Just(0.0), id, (+))
val builtin/sub = builtin-arithmetic("-", Nothing, (~), (-))
val builtin/mul = builtin-arithmetic("*", Just(1.0), id, (*))
val builtin/div = builtin-arithmetic("/", Nothing, fn(n) 1.0 / n, (/))
val builtin/mod = builtin-arithmetic("%", Nothing, id, (%))

val builtin/eq = builtin fn(args)
  fun equal(a, b)
    match (a, b)
      (SNum(n), SNum(m)) -> n == m
      (SSym(n), SSym(m)) -> n == m
      (SStr(n), SStr(m)) -> n == m
      (SCons(n1, n2), SCons(m1, m2)) -> equal(n1, m1) && equal(n2, m2)
      (SNil, SNil) -> True
      (SBool(n), SBool(m)) -> n == m
      _ -> False

  match args
    [] -> push(SBool(True))
    Cons(a, bs) -> push(SBool(bs.all fn(b) equal(a, b)))

fun builtin-compare(op, num-cmp, str-cmp)
  fun compare-all(f, x, xs)
    match xs
      [] -> True
      Cons(y, ys) -> f(x, y) && compare-all(f, y, ys)

  builtin fn(args)
    val result =
      match args
        [] -> True
        Cons(SNum(num), xs) ->
          val nums = xs.map fn(arg)
            match arg
              SNum(a) -> a
              _ -> throw(op ++ " takes numbers")
          compare-all(num-cmp, num, nums)
        Cons(SStr(str), xs) ->
          val strs = xs.map fn(arg)
            match arg
              SStr(a) -> a
              _ -> throw(op ++ " takes strings")
          compare-all(str-cmp, str, strs)
        _ -> throw(op ++ " is only defined for strings or numbers")
    push(SBool(result))

val builtin/lt = builtin-compare("<", fn(a, b) a < b, fn(a, b) a < b) // (<) breaks syntax highlight
val builtin/gt = builtin-compare(">", (>), (>))
val builtin/le = builtin-compare("<=", (<=), (<=))
val builtin/ge = builtin-compare(">=", (>=), (>=))

val builtin/call-cc = builtin fn(args)
  match args
    [f] -> apply(f, [capture-cont()])
    _ -> throw("call/cc takes one argument")

val builtin/never = builtin fn(args)
  match args
    Cons(f, xs) -> value/apply-never(f, xs)
    _ -> throw("never takes at least one argument")

val builtin/str = builtin fn(args)
  val chars = args.map fn(arg)
    match arg
      SNum(c)
        | 0.0 <= c && c <= 0x10FFFF.0 -> c.int.char
        | True -> throw("Each character of string must be inside the range 0-0x10ffff")
      _ -> throw("str takes numbers")
  push(SStr(chars.string))

val builtin/str-char-at = builtin fn(args)
  match args
    [SStr(s), SNum(index)] ->
      val char = maybe(s.first.advance(index.int).next, SNil) fn((c, _)) SNum(c.int.float64)
      push(char)
    _ -> throw("str-char-at takes a string and index")

val builtin/str-length = builtin fn(args)
  match args
    [SStr(s)] -> push(SNum(s.count.float64))
    _ -> throw("str-length takes a string")

val builtin/str-concat = builtin fn(args)
  var result := ""
  args.foreach fn(arg)
    match arg
      SStr(s) -> result := result ++ s
      _ -> throw("str-concat takes strings")
  push(SStr(result))

val builtin/substr = builtin fn(args)
  match args
    [SStr(s), SNum(index), SNum(length)] ->
      val subs = s.first(length.int).advance(index.int)
      if subs.count != length.int
        then throw("Index out of range")
        else push(SStr(subs.string))
    _ -> throw("substr takes a string, index, and length")

val builtin/sym-to-str = builtin fn(args)
  match args
    [SSym(s)] -> push(SStr(s))
    _ -> throw("sym->str takes a symbol")

val builtin/num-to-str = builtin fn(args)
  match args
    [SNum(s)] -> push(SStr(s.show))
    _ -> throw("num->str takes a number")

val builtin/str-to-num = builtin fn(args)
  match args
    [SStr(s)] -> match s.parse-float64
      Just(n) -> push(SNum(n))
      Nothing -> push(SNil)
    _ -> throw("num->str takes a symbol")

val builtin/vec = builtin fn(args)
  push(SPure(NVec(args.map(ref).vector)))

val builtin/vec-make = builtin fn(args)
  match args
    [SNum(length), init] -> push(SPure(NVec(vector-init(length.int) fn(_) ref(init))))
    _ -> throw("vec-make takes a length and init")

val builtin/vec-length = builtin fn(args)
  match args
    [SPure(NVec(v))] -> push(SNum(v.length.float64))
    _ -> throw("vec-length takes a vec")

val builtin/vec-get = builtin fn(args)
  match args
    [SPure(NVec(v)), SNum(index)] -> push(maybe(v.at(index.int), SNil, (!)))
    _ -> throw("vec-get takes a vec and index")

val builtin/vec-set = builtin fn(args)
  match args
    [SPure(NVec(v)), SNum(index), item] -> match v.at(index.int)
      Just(r) ->
        r := item
        push(SNil)
      Nothing -> throw("Index out of range")
    _ -> throw("vec-set! takes a vec, index, and value")

val builtin/vec-copy = builtin fn(args)
  match args
    [SPure(NVec(dest)), SNum(ds), SPure(NVec(src)), SNum(ss), SNum(l)] ->
      val dest-start = ds.int
      val src-start = ss.int
      val len = l.int
      fun run()
        for(len) fn(i)
          set(dest.at(dest-start + i).unjust, !src.at(src-start + i).unjust)
      match try(run)
        Ok(_) -> push(SNil)
        Error(_) -> throw("Index out of range")
    _ -> throw("vec-copy! takes a dest, dest-start-index, src, src-start-index, and length")

fun try-io(action)
  match try(action)
    Ok(r) -> SCons(SBool(True), r)
    Error(error) -> SCons(SBool(False), SStr(error.message))

val builtin/read-file-text = builtin fn(args)
  match args
    [SStr(filepath)] -> push(try-io { read-text-file(filepath.path).SStr })
    _ -> throw("read-file-text takes a filepath")

val builtin/write-file-text = builtin fn(args)
  match args
    [SStr(filepath), SStr(contents)] -> push(try-io { write-text-file(filepath.path, contents); SNil })
    _ -> throw("write-file-text takes a filepath and contents")

val builtin/read-console-line = builtin fn(args)
  match args
    [] -> push(try-io { readline().SStr })
    _ -> throw("read-console-line takes no argument")

val builtin/write-console = builtin fn(args)
  match args
    [SStr(text)] -> push(try-io { print(text); SNil })
    _ -> throw("write-console takes no argument")

pub fun builtin/args(env-args : list<string>) : builtin
  builtin fn(args)
    match args
      [] -> push(env-args.map(SStr).sexp)
      _ -> throw("args takes no argument")

val builtin/eval = builtin fn(args)
  match args
    [s] -> match eval(s)
      Right(r) -> push(SCons(SBool(True), r))
      Left(e) -> push(SCons(SBool(False), SStr(e)))
    _ -> throw("eval takes one argument")

fun builtin/macroexpand(name : string, recurse : bool) : builtin
  builtin fn(args)
    match args
      [s] -> match macroexpand(s, recurse)
        Right(r) -> push(SCons(SBool(True), r))
        Left(e) -> push(SCons(SBool(False), SStr(e)))
      _ -> throw(name ++ " takes one argument")
