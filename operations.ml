open Types
open Subst
open Gamma

let compose f g x =
  f (g x)

let check pred msg =
  if pred then
    ()
  else
    failwith msg
;;

let onlyIf pred inf =
  if pred then inf else ()

(* --------------------------------
 * free type variable
 * -------------------------------- *)
let rec ftv tp = 
  match tp with
    | Forall (ids, rho) -> 
	let tvs = ftv rho in
	let func tv = not (List.mem (tvId tv) ids) in
	  List.filter func tvs
    | TApp (nm, tvs) ->
	let tvs' = List.map ftv tvs in
	  List.fold_left union [] tvs'
    | TVar (tv) ->
	begin
	  match tv with
	    | TypeVar (id, Uni (tyRef, _)) ->
		begin
		  match !tyRef with
		    | Some (ty) -> ftv ty
		    | None _ -> [tv]
	      end
	    | _ -> [tv]
	end

let freeTvs tp =
  let tvs = ftv tp in
    List.filter (fun tv -> isUni (tvFlavour tv)) tvs

let freeSkolems tp =
  let tvs = ftv tp in
    List.filter (fun tv -> isSkolem (tvFlavour tv)) tvs



(* **************
 * instantiation
 * Instantiate the the "some" quantifiers of an annotation to fresh type variables 
 **************** *)
let rec (|->) sub tp =
  match tp with
    | Forall (ids, rho) ->
	let srho = (subRemove sub ids) |-> rho in
	  Forall (ids, srho)
    | TApp (nm, tys) ->
	let tys = List.map ((|->) sub) tys in
	  TApp (nm, tys)
    | TVar (tv) -> match tv with
	| TypeVar (id, (Uni (tpRef, _))) ->
	    begin
	      match !tpRef with
		| Some (tp) -> sub |-> tp
		| None      -> match subLookup sub tv  with
		    | Some (newTp) -> newTp
		    | None         -> tp
	    end
	| _ -> match subLookup sub tv with (* replace even bound ones useful for instantantiation *)
	    | Some (newTp) -> newTp
	    | None         -> tp

let rec subst tp = 
  match tp with
    | Forall (ids, rho) -> 
	let srho = subst rho in
	  Forall (ids, srho)
    | TApp (nm, tys) -> 
	let stys = List.map subst tys in
	  TApp (nm, stys)
    | TVar (TypeVar (_, Uni (tyRef, _))) ->
	begin
	  match !tyRef with
	    | Some (t) ->
		let ft = subst t in
		let _ = tyRef := (Some ft) in 
		  ft
	    | None     -> tp
	end
    | _ -> tp

let listSubst tps =
  List.map subst tps

let instantiate tp = 
  let t = ground tp in
    match t with
      | Forall (ids, rho) ->
	  let tvs  = freshTVars rankInf (List.length ids) in
	  let srho = (subNew ids tvs) |-> rho in
	    srho
      | rho -> rho
	  
let instantiateAnnot rank (Annot (ids, tp)) =
  let tvs = freshTVars rank (List.length ids) in
  let stp = (subNew ids tvs) |-> tp in
    (tvs, stp)


(* ------------------------------------------------------------------------
   -- Generalize
   -- Uses efficient generalization by lambda-ranking unifiable variables. See:
   -- See: George Kuan and David McQueen, "Efficient ML type inference with ranked type variables"
   ------------------------------------------------------------------------*)

let rec generalize gamma tp =
  let tvsTp = freeTvs tp in
  let depth = gammaDepth gamma in
  let func = function
    | TypeVar (_, (Uni (_, rref))) ->  let rank = !rref in rank  > depth
    | _ -> false
  in
  let tvs = List.filter func tvsTp in
  let _ = assertGen tvsTp tvs in (* assert that we generalize correctly *)
  let bvs = freshTypeVars Bound (List.length tvs) in
  let func bound tv =
    match tv with
      | TypeVar (_, Uni (tyRef, _)) ->
	  tyRef := Some (TVar bound)
      | _ -> raise (Invalid_argument "don't understand the haskell code")
  in
  let _ = List.map2 func bvs tvs in
  let stp = subst tp in
    mkForall (List.map tvId bvs) stp
and assertGen tvsTp tvs = ()
    (* assert that we generalize correctly *)
(*
    = let tvsG  = freeTvs (gammaCoDomain gamma) in
let tvsX = (tvsTp `diff` tvsG)
           if (sort tvs /= sort tvsX)
           then failure ("different generalization:\n tvs:  " ++ show (sort tvs) ++ "\ntvsX: " ++ show (show tvsX))
           else return ()
*)

(* Skolemize a quantified type and return the newly introduced skolem variables *)
let skolemize tp =
  let t = ground tp in
    match t with
      | Forall (ids, rho) ->
          let sks     = freshSkolems (List.length ids) in
	  let tvar sk = TVar (sk) in
          let srho    = subNew ids (List.map tvar sks) |-> rho in
            (* message ("skolemize: " ++ show tp ++ " to " ++ show srho) *)
            (sks, srho)
      | _ -> ([],tp)
