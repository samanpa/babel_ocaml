open Types

type id = int

type curryTy = | Curried
	       | Uncurried
type t = | Unit
	 | Bool
	 | Int
	 | Float
	 | String
	 | FunTy of curryTy * t list * t
	 | Poly of id list * t
	 | VarTy of id

let rec string_of_ty = function
  | Unit             -> "unit"
  | Bool             -> "bool"
  | Int              -> "int"
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
  | Poly (id, ty)    -> let ids = List.map string_of_int id in
                        let ids = String.concat ", " ids in
			let ty  = string_of_ty ty in
			  "forall " ^ ids ^ "." ^ ty

let rec equal e1 e2 = match (e1, e2) with
  | (Unit, Unit)    -> true
  | (Bool, Bool)    -> true
  | (Int, Int)      -> true
  | (Float, Float)  -> true
  | (FunTy (_, p1, r1), FunTy (_, p2, r2)) -> let requal = equal r1 r2 in
                                              let pequal = 
                                                if requal then
						  let le1 = List.length p1 in
						  let le2 = List.length p2 in
						  let res  = 
						    if le1 == le2 then
						      let pequals = List.map2 equal p1 p2 in
						      let id a = a in
							List.for_all id pequals
						    else
						      false
						  in
						    res
						else
						  false
					      in
						pequal
  | _ -> false
;;

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
		FunTy (Curried, params, ret)
  | nm    -> failwith ("We can't deal with " ^ nm)


