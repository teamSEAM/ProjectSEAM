type action = Ast | Interpret | Bytecode | Compile

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let verified = Semantic.check program in
  let compiled = Compile.translate program in
  print_endline "Hello?"
