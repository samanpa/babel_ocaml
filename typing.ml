open Types

type id = int

type t = | Unit
	 | Bool
	 | Int
	 | Float
	 | String
	 | FunTy of t list * t
	 | Poly of id list * t
	 | VarTy of id

let rec string_of_ty = function
  | Unit          -> "unit"
  | Bool          -> "bool"
  | Int           -> "int"
  | Float         -> "float"
  | String        -> "string"
  | FunTy (pl, r) -> let pl = List.map string_of_ty pl in
                     let pl = match pl with
		       | []   -> ""
		       | [pl] -> pl
		       | _    -> "(" ^ (String.concat ", " pl) ^ ")"
		     in
		     let r = string_of_ty r in
		       pl ^ " -> " ^ r
  | VarTy (id)    -> string_of_int id
  | Poly (id, ty) -> let ids = List.map string_of_int id in
                     let ids = String.concat ", " ids in
		     let ty  = string_of_ty ty in
		       "forall " ^ ids ^ "." ^ ty


let rec convert_type = function
  | TApp (nm, con)    -> convert_app nm con
  | Forall (ids, tp)  -> Poly (ids, convert_type tp)
  | TVar (tvar)       -> match tvar with
                           | TypeVar (id, Bound) -> VarTy (id)
			   | _                   -> failwith ("bleuh .." ^ (string_of_type_var tvar))
and match_fun acc = function
  | []  -> failwith "no result type found for function"
  | [ret] -> (List.rev acc, ret)
  | param::rest ->  match_fun (param::acc) rest
and convert_app name params = match name with
  | "int"  -> Int
  | "unit" -> Unit
  | "bool" -> Bool
  | "->"   -> let params = List.map convert_type params in
              let params, ret = match_fun [] params in
		FunTy (params, ret)
  | nm    -> failwith ("We can't deal with " ^ nm)


