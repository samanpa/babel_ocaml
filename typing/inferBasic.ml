open Types
open Gamma
open Operations
open Unify
open Texpr
open List

let rec infer gamma = function
  | IntLit (n)           -> (IntEx (n), TApp ("int", []))

  | FloatLit (f)         -> (FloatEx (f), TApp ("float", []))

  | Tup (t)              -> let fst (a, _) = a in
                            let snd (_, a) = a in
			    let res = List.map (infer gamma) t in
			    let ty = TApp ("*", List.map snd res) in
			    let ex = RecordEx (List.map fst res) in
			      ex, ty
      
  | Sel (ex, i)          -> let (ex, ty) = infer gamma ex in
                            let ty = match ty with
			      | TApp ("*", args) -> List.nth args i
			      | _ -> failwith "Can not select from a non tuple"
			    in
			      SelectEx (ex, i), ty
  | StrLit (s)           -> (StrEx (s), TApp ("string", []))

  | Var (nm)             -> 
      let t = gammaFind gamma nm in
	VarEx (nm), t

  | Let (nm, (Lam _ as lam), letBody) ->
      (* Add function name to gamma to allow recursion to work *)
      let (some, lamTy) = instantiateAnnot (gammaDepth gamma) annotAny in
      let gamma         = gammaExtend gamma nm lamTy in
      let (lamExpr, ty) = infer gamma lam in
      let gamma         = gammaExtend gamma nm ty in
      let (bodyExp, ty) = infer gamma letBody in
	LetEx (nm, lamExpr, bodyExp), ty

  | Let (nm, expr, body) ->
      let (expr, tp) = infer gamma expr in
      let gamma = gammaExtend gamma nm tp in
      let (bodyEx, ty) = infer gamma body in
	LetEx (nm, expr, bodyEx), ty

  | Lam (params, expr)   ->
      (* We can treat this as an annotated lambda expression:
       *   \x -> e => \(x :: some a.a) -> a *)
      let annot = List.map (fun _ -> annotAny) params in
      let alam = ALam (params, annot, expr) in
	infer gamma alam

  | ALam (params, annots, expr) ->
      let extractTp (_, ty)   = ty in
      let gammaExtendLam param ty gamma = gammaExtendLam gamma param ty in

      let rank     = (gammaDepth gamma) + 1 in
      (* instantiate the "some" quantifiers *)
      let someTps  = map (instantiateAnnot rank) annots in
      let tps      = map extractTp someTps in
      let newGamma = fold_right2 gammaExtendLam params tps gamma in
      let (expr, resTy)   = infer newGamma expr in
      (* lambda bodies are instantiated *)
      let resRho  = instantiate resTy in
      (* check that we don't infer polytypes for arguments *)
	(*   
      let ssome = listSubst some in
          let _= check (all isTau ssome) ("Using unannoted parameter(s) polymorphically with type(s): " ++ show ssome) in    
*)
      (*  generalize the result *)
      let funTy = List.fold_right mkFun tps resRho in
      let funTy = generalize gamma funTy in
	LamEx (funTy, params, Def (expr)), funTy
 
  | App (e1, e2) ->
      let (e1, tp1) = infer gamma e1 in
      let (arg, res) = matchfun tp1 in
      let (e2, tp2) = infer gamma e2 in
      let _ = subsume arg tp2 in
      let appTy = generalize gamma res in
	CallEx (e1, e2), appTy
  | If (cond, e1, e2) ->
      let (cond, condtp) = infer gamma cond in
      let condtp = instantiate condtp in
      let _ = match condtp with
	| TApp ("bool", []) -> ()
	| _ -> failwith ("expected a boolean in if condition but got " ^ (string_of_type condtp))
      in
      let (e1, tp1) = infer gamma e1 in
      let (e2, tp2) = infer gamma e2 in
      let _ = unify tp1 tp2 in
	IfEx (cond, e1, e2), tp1
  | term ->  failwith ("can not infer type for " ^ (string_of_term term))


let inferType term =
  infer gamma0 term
