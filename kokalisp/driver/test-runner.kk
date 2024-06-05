module kokalisp/driver/test-runner

import kokalisp/support/yield
import kokalisp/support/void
import kokalisp/support/os
import kokalisp/sexp
import kokalisp/sexp/parser

pub fun run-tests(src : string) : io ()
  for { read-testcases(src) } fn(tc)
    match try({ tc.command.run })
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

fun command/run(command: command) : pure ()
  match command
    ParseSuccess(input, result) ->
      parse-sexp(input).expect-success("Parse", id).expect-equal("Parse", result)
    ParseFailure(input) ->
      parse-sexp(input).expect-failure("Parse")
    _ -> () // TODO

fun parse-sexp(input : string) : div either<string, sexp<void>>
  parse(input.slice, sexp)

fun expect-success(result : either<a, b>, step : string, msg : a -> string) : exn b
  match result
    Right(a) -> a
    Left(e) -> throw(step ++ " failed: " ++ msg(e))

fun expect-failure(result : either<a, b>, step : string, ?show : b -> string) : exn ()
  match result
    Right(a) -> throw(step ++ " succeed: " ++ a.show)
    Left(_) -> ()

fun expect-equal(result : a, step : string, expect : string, ?show : a -> string) : exn ()
  val actual = result.show.trim
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
