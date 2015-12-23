open Printf

let _ = 
    try 
        let lexbuf = Lexing.from_channel stdin in
        let program = Parser.program Scanner.token lexbuf in
        let verified = Semantic.semantic_check program in
        let result =
            if (String.compare verified "") == 0 then
                Compile.translate program
            else
                (
                    output_string stderr verified;
                    output_string stderr "Continuing anyways...\n";
                    Compile.translate program
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
