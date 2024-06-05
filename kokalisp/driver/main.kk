module kokalisp/driver/main

import std/os/env
import std/os/file
import std/os/path
import kokalisp/driver/test-runner

fun main()
  match get-args()
    Cons("-test", tests) ->
      tests.foreach fn(test)
        val src = read-text-file(path(test))
        run-tests(src)
    _ -> println("not implemented")
