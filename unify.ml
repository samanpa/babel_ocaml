open Operations
open Types
open Subst

let rec matchfun tp =
  let rho = instantiate tp in
    match rho with
      | TApp ("->", arg::[res]) ->
          let sarg = subst arg in
            (sarg,res)
      | TVar (TypeVar (_, (Uni (tyref, rref)))) -> 
	  let mtp = !tyref in
	    begin
              match mtp with
		| Some tp -> matchfun tp 
		| None    -> 
		    let rank = ! rref in
                    let arg  = freshTVar rank in
                    let res  = freshTVar rank in
		    let _    = tyref := Some (mkFun arg res) in
                      (arg,res)
	    end
      | _ -> failwith ("applying a non-function: " ^ (string_of_type tp))

let rec unify t1 t2 = 
  match (t1, t2) with
    | (TApp (tcon1, arg1), TApp (tcon2, arg2)) when (compare tcon1 tcon2) == 0 ->
	List.iter2 unify arg1 arg2
    | (Forall (ids1, r1), Forall (ids2, r2)) when List.length (ids1) == List.length (ids2) ->
	let sks = freshSkolems (List.length ids1) in
	let tvar x = TVar (x) in
	let rho1 = subNew ids1 (List.map tvar sks) |-> r1 in
	let rho2 = subNew ids2 (List.map tvar sks) |-> r2 in
	let _ = unify rho1 rho2 in
	  (* check for escaping skolems *)
	let sk1 = freeSkolems t1 in
	let sk2 = freeSkolems t2 in
	  check (disjoint sks (union sk1 sk2))
             ("type is not polymorphic enough in unify:\n type1: " 
	      ^ (string_of_type t1) ^ "\n type2: " ^ (string_of_type t2))
    | (TVar v1, t2) when isUni (tvFlavour v1) ->
	unifyVar v1 t2
    | (t1, TVar v2) when isUni (tvFlavour v2) ->
	unifyVar v2 t1
    | _ -> failwith ("can not unify (" ^ (string_of_type t1) ^ ","  ^ (string_of_type t2))
(* Unify a variable *)
and unifyVar (TypeVar (id1, uni) as tv) tp2 =
  match uni with
    | Uni (ref1, rref1) ->
	begin
	  match !ref1 with
	    | Some tp1 -> unify tp1 tp2
	    | None     -> match tp2 with
		| TVar (TypeVar (id2, (Uni (ref2, rref2)))) ->
		    let  mtp2 = !ref2 in
		      begin 
			match mtp2 with
			    (* note: we can't shorten here since tv could be an element of tp3 *)
			  | Some (tp3)  -> unify (TVar (tv)) tp3 
			  | None        ->
			      (* adjust the lambda-rank of the unifiable variable *)
			      let _ =  ref1 := Some (tp2) in 
			      let rank1 = !rref1 in
			      let rank2 = !rref2 in
				onlyIf (rank2 > rank1) (rref2 := rank1)
		      end
		| _  ->
		    let tvs = freeTvs tp2 in
		      (* occurs check *)
		    let _ = check (not (List.mem tv tvs)) 
		      ("infinite type: " ^ (string_of_type_var tv) ^ " and "
		       ^ (string_of_type tp2)) in
		      (* adjust the lambda-rank of the unifiable variables in tp2 *)
		    let _ = ref1 := Some (tp2) in
		    let rank1 = !rref1 in
		      adjustRank rank1 tp2
	end
    | _ -> failwith "this should not happen"

and adjustRank rank tp =
  match tp with
    | TVar (TypeVar (id2, (Uni (ref2, rref2)))) ->
	begin
          match !ref2 with
            | Some (tp) -> adjustRank rank tp
            | None      -> 
		let rank2 = !rref2 in
                  onlyIf (rank2 > rank) (rref2 := rank)
	end
    | Forall (ids, rho) -> adjustRank rank rho
    | TApp (nm, tps)     ->
	List.iter (fun tp -> adjustRank rank tp) tps
    | _               -> ()


(* -------------------------------------------------------------------------
 * Subsumption
 * "subsume tp1 tp2" returns a substitution S, such that
 * we can instantiate tp2 to some type tp3 and S(tp1) = S(tp3)
 * -------------------------------------------------------------------------- *)
let subsume tp1 tp2 =
  let (sks,rho1) = skolemize tp1 in
  let rho2 = instantiate tp2 in
  let _ = unify rho1 rho2 in
        (* check for escaping skolems *)
  let sk1 = freeSkolems tp1 in
  let sk2 = freeSkolems tp2 in
    check (disjoint sks  (union sk1  sk2))
              ("type is not polymorphic enough in subsume:\n type1: " ^
		 (string_of_type tp1) ^ "\n type2: " ^ (string_of_type tp2))

