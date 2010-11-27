open Ast

type name = string

type lambda = | IntEx of int
	      | FloatEx of float
	      | VarEx of name
	      | UnitEx
	      | CallEx of lambda * lambda list
	      | LamEx of name list * lambda
	      | IfEx of lambda * lambda * lambda
	      | LetEx of name * lambda * lambda
	      | StringEx of string

let rec uncurry ex acc = match ex with
  | App (func, arg) -> uncurry func (arg::acc)
  | _ -> ex, acc
and of_ast = function
  | IntLit n -> IntEx (n)
  | FloatLit (f) -> FloatEx (f)
  | Var nm -> VarEx (nm)
  | Lam (p, body) -> LamEx (p, of_ast body)
  | App (func, arg) -> let func, args = uncurry func [arg] in
                         CallEx (of_ast func, List.map of_ast args)
  | Let (nm, e1, e2) -> LetEx (nm, of_ast e1, of_ast e2)
  | If (con, e1, e2) -> IfEx (of_ast con, of_ast e1, of_ast e2)
  | StrLit (str) -> StringEx (str)
  | UnitLit -> UnitEx
