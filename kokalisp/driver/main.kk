module kokalisp/driver/main

import std/os/env
import std/os/file
import std/os/path
import kokalisp/driver/test-runner
import kokalisp/sexp
import kokalisp/sexp/pattern
import kokalisp/vm/code

fun main()
  match get-args()
    Cons("-test", tests) ->
      tests.foreach fn(test)
        val src = read-text-file(path(test))
        run-tests(src)
      // TMP 1
      println([Ldv("+"), Ldv("foo"), Ldv("*"), Ldv("bar"), Ldv("baz"), App(2), Ldc(SNum(111.0)), App(3)])
      println([Ldv("a"), Sel([Ldv("b"), Leave], [Ldb("c"), Leave])])
      println([Ldv("a"), Sel([Ldv("b"), Leave], [Ldb("c"), Sel([Ldv("d"), Leave], [Ldv("e"), Leave]), Leave])])
      // TMP 2
      println(SSym("foo").pattern.maybe)
      println(SCons(SSym("foo"), SSym("bar")).pattern.maybe)
      println([SSym("a"), SSym("b"), SSym("c")].sexp.pattern.maybe)
    _ -> println("not implemented")
