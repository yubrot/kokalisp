module kokalisp/driver/test-runner

import kokalisp/support/yield
import kokalisp/support/raise
import kokalisp/support/void
import kokalisp/support/os
import kokalisp/sexp
import kokalisp/sexp/parser
import kokalisp/vm
import kokalisp/driver/builtins

pub fun run-tests(src : string) : io ()
  val vm = vm/new(builtins/list)
  for { read-testcases(src) } fn(tc)
    match try({ tc.command.run(vm) })
      Ok(_) ->
        println("PASS " ++ tc.title)
      Error(e) ->
        println("FAIL " ++ tc.title ++ " " ++ e.message)
        exit(1)

value struct testcase
  title : string
  command : command

type command
  ParseSuccess(input : string, result : string)
  ParseFailure(input : string)
  CompileSuccess(input : string, result : string)
  CompileFailure(input : string)
  EvalSuccess(input : string, result : string)
  EvalFailure(input : string)
  EvalAll(input : string)

// XXX: This noinline is required to avoid strange compiler bug
noinline fun command/run(command: command, vm : vm) : io ()
  match command
    ParseSuccess(input, result) ->
      parse-sexp(input).expect-success("Parse", id).show.expect-equal("Parse", result)
    ParseFailure(input) ->
      parse-sexp(input).expect-failure("Parse")
    CompileSuccess(input, result) ->
      parse-sexp(input).expect-success("Parse", id).sexp-compile(vm).expect-success("Compile", snd).show.expect-equal("Compile", result)
    CompileFailure(input) ->
      parse-sexp(input).expect-success("Parse", id).sexp-compile(vm).expect-failure("Compile")
    EvalSuccess(input, result) ->
      parse-sexp(input).expect-success("Parse", id).sexp-eval(vm).expect-success("Eval", show).show.expect-equal("Eval", result)
    EvalFailure(input) ->
      parse-sexp(input).expect-success("Parse", id).sexp-eval(vm).expect-failure("Eval")
    EvalAll(input) ->
      parse-program(input).expect-success("Parse", id).foreach fn(s)
        s.sexp-eval(vm).expect-success("Eval", show).ignore

fun parse-sexp(input : string) : _ either<string, sexp<void>>
  parse(input.slice, sexp)

fun parse-program(input : string) : _ either<string, list<sexp<void>>>
  parse(input.slice, program)

fun sexp-compile(s : sexp<void>, vm : vm) : _ either<(sexp<void>, string), code>
  either { vm.compile(s) }

fun sexp-eval(s : sexp<void>, vm : vm) : _ either<eval-error, value>
  either { vm.eval(s.map(absurd)) }

fun expect-success(result : either<a, b>, step : string, msg : a -> _ string) : _ b
  match result
    Right(a) -> a
    Left(e) -> throw(step ++ " failed: " ++ msg(e))

fun expect-failure(result : either<a, b>, step : string, ?show : b -> _ string) : _ ()
  match result
    Right(a) -> throw(step ++ " succeed: " ++ a.show)
    Left(_) -> ()

fun expect-equal(result : string, step : string, expect : string) : _ ()
  val actual = result.trim
  if actual != expect.trim then
    throw(step ++ " result differs: " ++ actual)

fun read-testcases(src : string) : <pure, yield<testcase>> ()
  var stream := src.lines
  fun has-next()
    match stream
      Cons(next, _) -> next.is-notempty
      _ -> False
  fun next()
    match stream
      [] -> throw("broken test input")
      Cons(line, rest) -> { stream := rest; line }
  fun next-n(num : string)
    match num.parse-int
      Just(n) -> list(1, n, fn(_) next()).join("\n")
      Nothing -> throw("broken test input")

  while(has-next)
    val title = next()
    val command-header = next()
    val command =
      match command-header.split(" ")
        ["PARSE_SUCCESS", n, m] ->
          val input = next-n(n);
          val result = next-n(m);
          ParseSuccess(input, result)
        ["PARSE_FAILURE", n] ->
          val input = next-n(n)
          ParseFailure(input)
        ["COMPILE_SUCCESS", n, m] ->
          val input = next-n(n)
          val result = next-n(m)
          CompileSuccess(input, result)
        ["COMPILE_FAILURE", n] ->
          val input = next-n(n)
          CompileFailure(input)
        ["EVAL_SUCCESS", n, m] ->
          val input = next-n(n)
          val result = next-n(m)
          EvalSuccess(input, result)
        ["EVAL_FAILURE", n] ->
          val input = next-n(n)
          EvalFailure(input)
        ["EVAL_ALL", n] ->
          val input = next-n(n)
          EvalAll(input)
        _ ->
          throw("broken test input: unknown command " ++ command-header)
    yield(Testcase(title, command))
