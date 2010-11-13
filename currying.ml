open Cgil
open Utils
open Typing

let type_env : (string, Typing.t) Hashtbl.t =
  Hashtbl.create 16
;;


let type_env_add nm ty =
  let _ = Hashtbl.add type_env nm ty in
    ty
;;


let type_env_find nm =
  Hashtbl.find type_env nm
;;


let uncurry_funty arity ty =
  let fail ty = failwith ("not a function for uncurry ty " ^ (string_of_ty ty)) in
  let rec uncurry arity acc ty = match arity with
    | 0 -> 
	begin
	  match ty with
	    | FunTy (_, _::_, _) -> fail ty
	    | FunTy (_, [], retTy) 
	    | retTy -> (List.rev acc), retTy
	end
    | _ -> 
	match ty with
	  | FunTy (_, [pty], retTy) -> uncurry (arity - 1) (pty::acc) retTy
	  | _ -> fail ty
  in
    match ty with 
      | FunTy (Curried, _, _) -> 
	  let (pty, retTy) = uncurry arity [] ty in
	    FunTy (Uncurried, pty, retTy)
      | _ -> fail ty
;;

let rec count_params = function 
  | Typing.FunTy (_, _, ty) -> 1 + (count_params ty)
  | _ -> 0
;;

let uncurry_funty_unknown_arity nm ty =
  let num_params = count_params ty in
  let ty = uncurry_funty num_params ty in (* uncurry function *)
    type_env_add nm ty
;;

let rec extract_app = function
  | Lapply (func, p1) ->
      let res = match func with
	| Lapply (_, _) -> let res = match extract_app func with
	    | Lapply (f2, params) -> Lapply (f2, params @ p1)
	    | func -> func
	  in
	    res
	| _ -> Lapply (func, p1)
      in
	res
  | ex -> ex
let rec uncurry_application func =
  match extract_app func with
    | Lapply (f, args) -> Lapply (uncurry f, List.map uncurry args)
    | ex -> ex
	
and uncurry ex = 
  let ex =  match ex with
    | Lint _         
    | Lunit
    | Lvar _            -> ex
    | Lapply (f, args)  -> uncurry_application ex
    | Lif (e1, e2, e3)  -> Lif (uncurry e1, uncurry e2, uncurry e3)
    | Llet (nm, e1)     -> Llet (nm, uncurry e1)
    | Lfn (nm, t, p, b) -> let num_args = List.length p in
	                   let t = uncurry_funty num_args t in
                           let _ = type_env_add nm t in
	                      begin
				match b with
				  | Ext     -> Lfn (nm, t, p, b)
				  | Def (b) -> Lfn (nm, t, p, Def (uncurry b))
			      end
    | Lseq (e1, e2)     -> Lseq (uncurry e1, uncurry e2)
  in
    ex
