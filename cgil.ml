(* An intermediate language suitable for codegeneration *)
open Texpr
open Typing

type fnDef = | Ext (* externally defined e.g. in C *)
	     | Def of cgil
(* typed lambda intermidiate language
 * suitable for code generation       *)
and cgil = | Lint of int
	   | Lunit
	   | Lvar of name
	   | Lapply of cgil * cgil list
	   | Lfn of name * t * name list * fnDef
	   | Lif of cgil * cgil * cgil
	   | Llet of name * cgil
	   | Lseq of cgil * cgil

let rec from_lty = function
  | IntEx lit           -> Lint (lit)
  | UnitEx              -> Lunit
  | VarEx (nm)          -> Lvar (nm)
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
  let cg = from_lty lty in
  let nm = Utils.get_new_name "fn" in
    Lfn (nm, FunTy ([], ty), [], Def (cg))

let rec to_string = function
  | Lfn (nm, t, _, b) -> begin
      match b with
	| Ext -> "external " ^ "fun " ^ nm ^ ": " ^ (string_of_ty t) ^ "\n"
	| Def b -> "fun " ^ nm ^ ": " ^ (string_of_ty t) ^ "\n\t" ^ (to_string b)
    end
  | Llet (nm, e)      -> "let " ^ nm ^ " = " ^ (to_string e)
  | Lvar (nm)         -> nm
  | Lint lit          -> string_of_int lit
  | Lseq (e1, e2)     -> (to_string e1) ^ ";\n" ^ (to_string e2)
  | _         -> "to string f"
