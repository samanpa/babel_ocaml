(* An intermediate language suitable for codegeneration *)
open Texpr
open Typing

type fnDef = | Ext (* externally defined e.g. in C *)
	     | Def of cgil
(* typed lambda intermidiate language
 * suitable for code generation       *)
and cgil = | Lint of int
	   | Lfloat of float
	   | Ltuple of cgil list
	   | Lstr of string
	   | Lvar of name
	   | Lapply of cgil * cgil list
	   | Lfn of name * t * name list * fnDef
	   | Lif of cgil * cgil * cgil
	   | Llet of name * cgil
	   | Lseq of cgil * cgil

let rec from_lty = function
  | IntEx lit           -> Lint (lit)
  | FloatEx lit         -> Lfloat (lit)
  | RecordEx t          -> Ltuple (List.map from_lty t)
  | VarEx (nm)          -> Lvar (nm)
  | StrEx (s)           -> Lstr (s)
  | CallEx (f, a)       -> Lapply (from_lty f, [from_lty a])
  | IfEx (cond, e1, e2) -> Lif (from_lty cond, from_lty e1, from_lty e2)
  | LamEx (t, p, body)  -> 
      let nm = Utils.get_new_name "fn" in
	mk_fun nm t p body
  | LetEx (nm, LamEx (t, p, body), e2)  ->
      let fn = mk_fun nm t p body in
	Lseq (fn, from_lty e2)
  | LetEx (nm, e1, e2)  ->
      let llet = Llet (nm, from_lty e1) in
	Lseq (llet, from_lty e2)
and mk_fun nm t p body =
  let body = match body with 
    | Texpr.Ext -> Ext
    | Texpr.Def body -> Def (from_lty body)
  in
  let t = convert_type t in
    Lfn (nm, t, p, body)

let wrap (ty, lty) =
  let cg   = from_lty lty in
  let body = Def cg in
  let nm   = Utils.get_new_name "fn" in
  let fty  = FunTy (Curried, [], ty) in
    Lfn (nm, fty, [], body)

let rec to_string = function
  | Lfn (nm, t, _, b) -> begin
      match b with
	| Ext -> "external " ^ "fun " ^ nm ^ ": " ^ (string_of_ty t) ^ "\n"
	| Def b -> "fun " ^ nm ^ ": " ^ (string_of_ty t) ^ "\n\t" ^ (to_string b)
    end
  | Llet (nm, e)       -> "let " ^ nm ^ " = " ^ (to_string e)
  | Lvar (nm)          -> nm
  | Lstr s             -> "\"" ^ s ^ "\""
  | Lint lit           -> string_of_int lit
  | Lseq (e1, e2)      -> (to_string e1) ^ ";\n" ^ (to_string e2)
  | Lapply (e1, args)  -> let args = List.map to_string args in
                          let e1 = to_string e1 in
			  let args = String.concat "," args in
			    e1 ^ "(" ^ args ^ ")"
  | Lif (cond, e1, e2) -> let cond = to_string cond in
                          let e1 = to_string e1 in
			  let e2 = to_string e2 in
			    "if " ^ cond ^ " then\n" ^ e1 ^ "\nelse\n" ^ e2 
  | Lfloat f           -> string_of_float f
  | Ltuple t           -> let t = List.map to_string t in
                          let t = String.concat ", " t in
			    "(" ^ t ^ ")"
