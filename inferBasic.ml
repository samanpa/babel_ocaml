open Types
open Gamma
open Operations
open Unify
open Texpr

let rec infer gamma = function
  | IntLit (n)           -> (IntEx (n), TApp ("int", []))

  | UnitLit              -> (UnitEx, TApp ("unit", []))

  | Var (nm)             -> 
      let _ = print_endline nm in
      let t = gammaFind gamma nm in
	VarEx (nm), t

  | Let (nm, expr, body) ->
      let (expr, tp) = infer gamma expr in
      let gamma = gammaExtend gamma nm tp in
      let (bodyEx, ty) = infer gamma body in
	LetEx (nm, expr, bodyEx), ty

  | Lam (params, expr)   ->
      (* We can treat this as an annotated lambda expression:
       *   \x -> e => \(x :: some a.a) -> a *)
      let annot = List.map (fun nm -> (nm, annotAny)) params in
      let alam = ALam (annot, expr) in
	infer gamma alam

  | ALam (annotNms, expr) ->
      let rank = (gammaDepth gamma) + 1 in
      (* instantiate the "some" quantifiers *)
      let instantiateAnnot (nm,ann) = 
	let (some, tp1) = instantiateAnnot rank ann in
	  (nm, some, tp1)
      in
      let instLst = List.map instantiateAnnot annotNms in
      let gammaExtendLam (nm,_,tp1) gamma = gammaExtendLam gamma nm tp1 in
      let newGamma = List.fold_right gammaExtendLam instLst gamma in
      let (expr, tp2)   = infer newGamma expr in
      (* lambda bodies are instantiated *)
      let rho2  = instantiate tp2 in
        (* check that we don't infer polytypes for arguments *)
	(*   
      let ssome = listSubst some in
          let _= check (all isTau ssome) ("Using unannoted parameter(s) polymorphically with type(s): " ++ show ssome) in    
*)
      (*  generalize the result *)
      let extractTy (_, _, ty) = ty in
      let funTy = List.fold_right (fun an tp1 -> mkFun (extractTy an) tp1) instLst rho2 in
      let funTy = generalize gamma funTy in
      let params = List.map (fun (nm,_) -> nm) annotNms in
	LamEx (funTy, params, Def (expr)), funTy
 
  | App (e1, e2) ->
      let (e1, tp1) = infer gamma e1 in
      let (arg, res) = matchfun tp1 in
      let (e2, tp2) = infer gamma e2 in
      let _ = subsume arg tp2 in
      let appTy = generalize gamma res in
	CallEx (e1, e2), appTy
   | term ->  failwith ("can not infer type for " ^ (string_of_term term))


let inferType term =
  infer gamma0 term
