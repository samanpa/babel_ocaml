type name  = string

(* any annotation you give to an expression *)
type t =   TApp of string * t list   (* e.g. int, int list *)
	 | TVar of string            (* e.g. 'a, 'b *)

type expr =   Var of name                  (* x *)
	    | IntLit of int                (* 12 *)
	    | UnitLit                      (* () *)
	    | App of expr * expr           (* f x *)
	    | Lam of name list * expr      (* \x y -> x + y *)
	    | Let of name * expr * expr    (* let x = f y in x + 1 *)
	    | If of expr * expr * expr     (* if x > 0 then x else -x *)
	    
(* top level *)
type toplevel =
  | Expr of expr
  | Open of string
  | Extern of string * t


(* string conversion *)
let rec string_of_expr = function
  | IntLit n             -> string_of_int (n)

  | UnitLit              -> "()"

  | Var nm               -> nm

  | Lam (p, body)        -> let body = string_of_expr body in
                            let concat x y = x ^ " " ^ y in
			    let p = List.fold_left concat "" p in
                               "fun" ^ p ^ " -> " ^ body

  | App    (body, arg)   -> let fn = string_of_expr body in
                            let arg = string_of_expr arg in
			      "(" ^ fn ^ " " ^ arg ^ ")"

  | Let (nm, e1, e2)     -> let e1Str = string_of_expr e1 in
                            let e2Str = string_of_expr e2 in
			      "let " ^ nm ^ " = " ^ e1Str ^ " in\n " ^ e2Str ^ "\n"

  | If (con, e1, e2)     -> let con = string_of_expr con in
                            let e1 = string_of_expr e1 in
			    let e2 = string_of_expr e2 in
			      "if " ^ con ^ " then\n " ^ e1 ^ "\nelse \n " ^ e2 ^ "\n"
  
let rec string_of_expr_type = function
  | TApp (con, args) -> let args = List.map string_of_expr_type args in
			let args = match args with 
			  | [] -> ""
			  | _  -> " (" ^ (String.concat " " args) ^ ")"
			in
			  con ^ args
  | TVar var         -> var


let rec string_of_toplevel = function
  | Expr (e)        -> string_of_expr e
  | Open (str)      -> "open " ^ str
  | Extern (nm, tp) -> let tp = string_of_expr_type tp in
      "external " ^ nm ^ " : " ^ tp

(* helper functions *)

let mk_app e1 exprs = let app e1 e2 = App (e1, e2) in
                        List.fold_left app e1 exprs
  
let mk_tuple_type args = TApp ("tuple", args)
let unit_type = mk_tuple_type []
let int_type  = TApp ("int", [])
let mk_fun_type arg ret = TApp ("function",
				 arg :: [ret])
