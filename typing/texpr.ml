open Types

type name = string

type fnDef = | Ext (* externally defined e.g. in C *)
	     | Def of lty
(* typed lambda intermidiate language
 * suitable for code generation       *)
and lty =  | IntEx of int
	   | FloatEx of float
	   | RecordEx of lty list
	   | StrEx of string
	   | SelectEx of lty * int
	   | VarEx of name
	   | CallEx of lty * lty
	   | LamEx of termType * name list * fnDef
	   | IfEx of lty * lty * lty
	   | LetEx of name * lty * lty
;;
    

let rec convert_to_term = function
  | Ast.IntLit n         -> IntLit (n)

  | Ast.FloatLit n       -> FloatLit (n)

  | Ast.StrLit s         -> StrLit (s)

  | Ast.Tuple t          -> Tup (List.map convert_to_term t)

  | Ast.Select (t, i)    -> Sel (convert_to_term t, i)

  | Ast.Var nm           -> Var (nm)

  | Ast.Lam (p, body)    -> Lam (p, convert_to_term body)

  | Ast.App (body, arg)  -> App (convert_to_term body, convert_to_term arg)

  | Ast.Let (nm, e1, e2) -> Let (nm, convert_to_term e1, convert_to_term e2)

  | Ast.If (con, e1, e2) -> If (convert_to_term con, convert_to_term e1, convert_to_term e2)


(* wrap an ast in a function before type checking *)
let convert_ast ast = 
    convert_to_term ast

let rec convert_from_astTy = function
  | Ast.TApp (nm, con)   -> let con = List.map convert_from_astTy con in
                              Types.TApp (nm, con)

  | Ast.TVar (t)         -> Types.TApp (t, [])


let rec string_of_lty = function 
  | VarEx (nm)         -> nm
 
  | RecordEx (t)       -> let t = List.map string_of_lty t in
                          let t = String.concat ", " t in
			    "(" ^ t ^ ")"

  | SelectEx (t, i)    -> (string_of_lty t) ^ "[" ^ (string_of_int i) ^ "]"
  | StrEx (s)          -> "\"" ^ s ^ "\""

  | IntEx (lit)        -> string_of_int lit

  | FloatEx (lit)      -> string_of_float lit

  | CallEx (f, a)      -> let f = string_of_lty f in
                          let a = string_of_lty a in
			    "(" ^ f ^ " " ^ a ^ ")"

  | LamEx (t, p, body) -> let body = match body with
                            | Ext     -> "extern"
			    | Def (b) -> string_of_lty b
                          in 
			  let params = String.concat ", " p in
			  let ty = string_of_type t in
			    "fun " ^ params ^ " : " ^ ty ^ " =\n\t " ^ body

  | LetEx (nm, t1, t2) -> let t1 = string_of_lty t1 in
                          let t2 = string_of_lty t2 in 
			    "let " ^ nm ^ " = " ^ t1 ^ "\n\tin\n\t" ^ t2

  | IfEx (c, t1, t2)   -> let t1 = string_of_lty t1 in
                          let t2 = string_of_lty t2 in
			  let c = string_of_lty c in
			     " if " ^ c ^ "\nthen\n\t" ^ t1 ^ "\nelse\n\t" ^ t2 ^ "\n"


