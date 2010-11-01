open Cgil
open Utils
open Typing

let uncurry_funty ty =
  let rec extract_param_tys acc = function
    | FunTy (pty, retTy) ->
	begin
	  match pty with
	    | [pty]  -> extract_param_tys (pty::acc) retTy
	    | _      -> pty, retTy
	end
    | retTy          -> (List.rev acc), retTy
  in
  let (pty, retTy) = extract_param_tys [] ty in
    FunTy (pty, retTy) 

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
    | Lfn (nm, t, p, b) -> print_endline ("-----" ^nm); let t = uncurry_funty t in
	                      begin
				match b with
				  | Ext     -> Lfn (nm, t, p, b)
				  | Def (b) -> Lfn (nm, t, p, Def (uncurry b))
			      end
    | Lseq (e1, e2)     -> Lseq (uncurry e1, uncurry e2)
  in
    ex
