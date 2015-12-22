type action = Ast | Compile

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in



    let verified = Semantic.semantic_check program in
    try
        let result =
            if (String.compare verified "") == 0 then
                    Compile.translate program
            else
                    (
                        print_endline verified;
                        exit 1;
                    )
            in
        print_endline result;
    with
        Parsing.Parse_error ->
                (
                    print_endline "Parsing error!";
                    exit 1;
                )
        | _ -> exit 1


  (* print_string (Ast.string_of_program program) *)
   (*  print_string (Compile.translate program)  *)
