open Ast

(* entry points into the parser *)
let parse lexbuf =
  let toplevel = Parser.toplevel Lexer.token lexbuf in
    toplevel
;;

let rec parse_top_level lexbuf acc =
  try
    parse_top_level lexbuf ((parse lexbuf)::acc)
  with
      Lexer.Eof -> acc
  
let parse_file file_name =
  try
    let in_channel = open_in file_name in
    let lexbuf = Lexing.from_channel in_channel in
    let items = parse_top_level lexbuf [] in
      List.rev items
  with
      Sys_error x -> print_endline x; []
;;

