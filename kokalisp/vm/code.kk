// VM machine code.
module kokalisp/vm/code

import std/core/undiv
import kokalisp/support/void
import kokalisp/sexp
import kokalisp/sexp/pattern

pub alias code = list<inst>

pub type inst
  Ldc(constant : sexp<void>)
  Ldv(variable : string)
  Ldf(pattern : pattern, code : code)
  Ldm(pattern : pattern, code : code)
  Ldb(name : string)
  Sel(a : code, b : code)
  App(argc : int)
  Leave
  Pop
  Def(variable : string)
  Set(variable : string)

pub fun show(c : code) : string
  with pretend-no-div
  var id := 0
  var bufs := []
  with fun add-block(header)
    val label = "[" ++ id.show ++ " " ++ header ++ "]"
    val buf = ref("")
    id := id + 1
    bufs := Cons(buf, bufs)
    (label, buf)
  print/code("entry", c)
  bufs.foldr("") fn(buf, ret) ret ++ !buf

alias print<h> = <st<h>, add-block<h>, div>

effect add-block<h>
  fun add-block(header : string) : (string, ref<h, string>)

fun print/code(header : string, code : code) : print<h> string
  val (label, buf) = add-block(header)
  buf := label ++ "\n"
  code.foreach fn(i) buf := !buf ++ "  " ++ print/inst(i) ++ "\n"
  label

fun print/inst(inst : inst) : print<h> string
  match inst
    Ldc(constant) -> "ldc " ++ constant.show
    Ldv(variable) -> "ldv " ++ variable
    Ldf(pattern, c) -> "ldf " ++ print/code("fun " ++ pattern.show, c)
    Ldm(pattern, c) -> "ldm " ++ print/code("macro " ++ pattern.show, c)
    Ldb(name) -> "ldb " ++ name
    Sel(a, b) -> "sel " ++ print/code("then", a) ++ " " ++ print/code("else", b)
    App(argc) -> "app " ++ argc.show
    Leave -> "leave"
    Pop -> "pop"
    Def(variable) -> "def " ++ variable
    Set(variable) -> "set " ++ variable
