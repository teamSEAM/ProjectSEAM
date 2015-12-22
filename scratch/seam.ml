type action = Ast | Compile

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  (* print_string (Ast.string_of_program program) *)
  print_string (Compile.translate program)
