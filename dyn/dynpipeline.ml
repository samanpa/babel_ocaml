open Parse
open Ast
open Utils


let print_toplevel toplevel =
  let toplevel_str = Ast.string_of_toplevel toplevel in
  let _ = print_endline ("parsed expr:\n\t" ^ toplevel_str) in
    toplevel

(* push a top level expression into the code generation pipeline *)
let rec push toplevel =
  toplevel >>
(*    print_toplevel >> *)
    handle_top_level 

and handle_top_level = function
  | Extern _ -> ()
  | Open (file_name) ->
      let toplevels = parse_file (file_name ^ ".ts") in
      let fn toplevel = push toplevel in
      let _ = List.map fn toplevels in
	()
  | Expr (ex) ->
      ex >>
	Elaborate.elaborate >>
	Lambda.of_ast >>
	Eval.eval >>
	InitialBasis.to_string >>
	print_endline
;;

