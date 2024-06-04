module kokalisp/driver/test-runner

import kokalisp/support/yield

pub fun run-tests(src : string) : io ()
  for { read-testcases(src) } fn(tc)
    println(tc.show)

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

fun show(c : testcase) : string
  c.title ++ ": " ++ (match c.command
    ParseSuccess(_, _) -> "ParseSuccess"
    ParseFailure(_) -> "ParseFailure"
    CompileSuccess(_, _) -> "CompileSuccess"
    CompileFailure(_) -> "CompileFailure"
    EvalSuccess(_, _) -> "EvalSuccess"
    EvalFailure(_) -> "EvalFailure"
    EvalAll(_) -> "EvalAll"
  )

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
