open Utils
open Ast

let bindings : string Env.t = Env.create None;;

(* Does renaming *)
let rec rename bindings = function
  | Let (nm, e1, e2)   -> let newNm = match bindings.Env.level with
                            | 0 -> nm
			    | _ -> Utils.get_new_name nm 
                          in 
                          let _  = Env.put bindings nm newNm in
			  let e1 = rename bindings e1 in
			  let e2 = rename bindings e2 in
			    Let (newNm, e1, e2)
  | App (e1, e2)       -> let e1 = rename bindings e1 in
                          let e2 = rename bindings e2 in
			    App (e1, e2)
  | Lam (params, body) -> let fenv = Env.create (Some (bindings)) in
                          let rename_param p = 
			    let new_p = Utils.get_new_name p in
			    let _ = print_endline new_p in
			    let _ = print_endline p in
			    let _ = Env.put fenv p new_p in
			      new_p
			  in
			  let params = List.map rename_param params in
			  let body = rename fenv body in
			    Lam (params, body)
  | If (cond, e1, e2)  -> let cond = rename bindings cond in
                          let e1 = rename bindings e1 in
			  let e2 = rename bindings e2 in
			    If (cond, e1, e2)
  | Var (nm)           -> let new_nm_opt = Env.find bindings nm in
                          let new_nm = match new_nm_opt with
			    | Some (new_nm) -> new_nm
			    | None -> failwith (nm ^ " not found in elaborate phase")
			  in
			    Var (new_nm)
  | IntLit _ as e1     -> e1
  | UnitLit as e1      -> e1
;;


let elaborate_extern nm =
  Env.put bindings nm nm

let elaborate ast =
  rename bindings ast
