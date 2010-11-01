open Parse

let main_module = "main"

let rec main lexbuf = 
  try
    while true do
      let _ = print_string "expr> "; flush stdout in
      let toplevel = parse lexbuf in
	Pipeline.push "main" toplevel true
    done
  with 
      Lexer.Eof ->
	exit 0
    | Parsing.Parse_error ->
        ignore (print_endline ("Parse error in expr"));
	main lexbuf
  
let _ =
  let initialBasis = Ast.Open ("initial_basis") in
  let _ = Pipeline.push main_module initialBasis false in
  let lexbuf = Lexing.from_channel stdin in
    main lexbuf
