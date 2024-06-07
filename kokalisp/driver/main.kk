module kokalisp/driver/main

import std/os/env
import std/os/file
import std/os/path
import std/os/readline
import kokalisp/support/os
import kokalisp/sexp
import kokalisp/sexp/parser
import kokalisp/vm
import kokalisp/driver/bootcode
import kokalisp/driver/test-runner
import kokalisp/driver/builtins

fun main()
  match get-args()
    [] ->
      println("[kokalisp REPL]")
      val vm = init-vm(True, [])
      run-repl(vm)
    Cons("-test", tests) ->
      tests.foreach fn(test)
        run-tests(init-vm(False, []), read-text-file(test.path))
    args ->
      val (files, inner-args) = collect-files(args, [])
      val vm = init-vm(True, inner-args)
      files.foreach fn(file)
        val source = read-text-file(file.path)
        match exec-string(vm, source)
          Right(_) -> ()
          Left(e) ->
            println(file ++ ": " ++ e)
            exit(1)

fun init-vm(load-bootcode : bool, args : list<string>) : io-noexn vm
  val builtins = Cons(("args", builtin/args(args)), builtins/list)
  val vm = vm/new(builtins)
  if load-bootcode then
    match exec-string(vm, bootcode)
      Right(_) -> ()
      Left(e) ->
        println("<bootcode>: " ++ e)
        exit(1)
  vm

fun exec-string(vm : vm, source : string) : io-noexn either<string, value>
  match parse-program(source)
    Right(ss) -> match vm.eval-all(ss)
      Right(v) -> Right(v)
      Left(e) -> Left(e.show)
    Left(e) -> Left("Parse error: " ++ e)

fun run-repl(vm : vm) : io-noexn ()
  print("> ")
  match try(readline)
    Ok(line) ->
      val result = match exec-string(vm, line)
        Right(v) -> v.show
        Left(e) -> e
      println(result)
      run-repl(vm)
    Error(_) -> ()

fun collect-files(xs : list<string>, files : list<string>) : (list<string>, list<string>)
  match xs
    Cons("--", xx) -> (files.reverse, xx)
    Cons(x, xx) -> collect-files(xx, Cons(x, files))
    Nil -> (files.reverse, [])
