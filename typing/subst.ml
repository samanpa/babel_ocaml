(* --------------------------------------------------------------------
 *  A substitution is an idempotent mapping from type variables to types
 * --------------------------------------------------------------------*)

(* A substitution from type variables to types *)
open Types
type sub = Sub of (int, termType) Hashtbl.t

let subEmpty = 
  Sub (Hashtbl.create 0)

let subIsEmpty (Sub s) =
  Hashtbl.length s == 0

let subRemove (Sub s) ids =
  let _ = List.map (fun id -> Hashtbl.remove s id) ids in
    Sub (s)

let subLookup (Sub s) tv =
  try
    Some (Hashtbl.find s (tvId tv))
  with Not_found -> None
	

let subNew ids tps =
  let map = Hashtbl.create 0 in
  let fn id tp = Hashtbl.add map id tp in
  let _ = List.map2 fn ids tps in
    Sub (map)

(*
subSingle :: Id -> Type -> Sub
subSingle id tp
  = Sub (M.singleton id tp)

subLookup :: Sub -> TypeVar -> Maybe Type
subLookup (Sub s) tv
  = M.lookup (tvId tv) s

instance Show Sub where
  show (Sub s)  = show s
*)
