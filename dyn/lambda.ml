
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

let rec of_ast = function
  | Ast.IntLit n -> IntEx (n)
  | Ast.FloatLit (f) -> FloatEx (f)
  | Ast.Var nm -> VarEx (nm)
  | Ast.Lam (p, body) -> LamEx (p, of_ast body)
  | Ast.App (func, args) -> 
      let args = of_ast args in
	CallEx (of_ast func, [args])
  | Ast.Let (nm, e1, e2) -> LetEx (nm, of_ast e1, of_ast e2)
  | Ast.If (con, e1, e2) -> IfEx (of_ast con, of_ast e1, of_ast e2)
  | Ast.StrLit (str) -> StringEx (str)
  | Ast.UnitLit -> UnitEx
