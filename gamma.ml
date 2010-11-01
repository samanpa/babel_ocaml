open Types

type gamma = Gamma of rank * (name, termType) Hashtbl.t


let gammaExtend (Gamma (rank, tbl) as gamma) name tp
    = Hashtbl.add tbl name tp; gamma

let gammaExtendLam (Gamma (rank, tbl) as gamma) name tp
    = let _ = Hashtbl.add tbl name tp in
      Gamma (rank + 1, tbl)

let gammaDepth (Gamma (rank, _)) =
    rank

let gammaFind (Gamma (rank, gamma)) name
    = try
      Hashtbl.find gamma name
    with Not_found ->
      failwith (name ^ " not found in gamma")

let gamma0elems = [
  ("true",  TApp ("bool", []));
  ("false", TApp ("bool", []));
  ("nop",   TApp ("->", [
		    TApp ("unit", []);
		    TApp ("unit", [])]))
]

let gamma0 =
  let gamma = Gamma (0, Hashtbl.create 0) in
(*
  let parse str =
    let lexbuf = Lexing.from_string str in
    let astTy = Parser.term_type Lexer.token lexbuf in
    let ty = 
  in
  let add (nm, value) = gammaExtend gamma nm (parse value) in
  let _ = List.map add gamma0elems in
*)
    gamma


let gammaCoDomain (Gamma (rank, gamma)) =
  let func _ codom lst = codom::lst in
    Hashtbl.fold func gamma []
