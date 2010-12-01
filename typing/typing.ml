open Types

type id = int

type curryTy = | Curried
	       | Uncurried
type t = | Unit
	 | Bool
	 | Char
	 | Int
	 | Int64
	 | Int16
	 | Float
	 | String
	 | StructTy of t list
	 | FunTy of curryTy * t list * t
	 | Poly of id list * t
	 | VarTy of id

let rec string_of_ty = function
  | Unit             -> "unit"
  | Bool             -> "bool"
  | Char             -> "char"
  | Int              -> "int"
  | Int16            -> "short"
  | Int64            -> "long"
  | Float            -> "float"
  | String           -> "string"
  | FunTy (_, pl, r) -> let pl = List.map string_of_ty pl in
                        let pl = match pl with
 			  | []   -> ""
			  | [pl] -> pl
			  | _    -> "(" ^ (String.concat ", " pl) ^ ")"
			in
			let r = string_of_ty r in
			  pl ^ " -> " ^ r
  | VarTy (id)       -> string_of_int id
  | StructTy (t)     -> let t = List.map string_of_ty t in
                        let t = String.concat ", " t in
			  "{" ^ t ^ "}"
  | Poly (id, ty)    -> let ids = List.map string_of_int id in
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
  | "int"    -> Int
  | "short"  -> Int16
  | "long"   -> Int64
  | "char"   -> Char
  | "float"  -> Float
  | "unit"   -> Unit
  | "bool"   -> Bool
  | "string" -> String
  | "->"     -> let params = List.map convert_type params in
                let params, ret = match_fun [] params in
		  FunTy (Curried, params, ret)
  | "*"      -> StructTy (List.map convert_type params)
  | nm       -> failwith ("We can't deal with " ^ nm)


