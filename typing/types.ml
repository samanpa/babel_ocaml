type name = string

type term = | Var of name                            (* x *)
            | IntLit of int                          (* 1 *)
	    | FloatLit of float                      (* 1.2 *)
	    | Tup of term list                       (* (1, 2, 4) *)
	    | StrLit of string                       (* "string lit" *)
	    | If of term * term * term               (* if true then a else b *)
	    | App of term * term                     (* f x *)
	    | Lam of name list * term                (* \x y -> x + y*)
	    | ALam of name list * annot list * term  (* \x::int -> x *)
	    | Let of name * term * term              (* let x = f y in x + 1 *)
	    | Ann of term * annot                    (* (f x) :: int *)

(* type annotation
 * "some a. type". A type annotation is closed under "some" type variables *)
and annot = Annot of id list * termType

and id = int
and rho = termType (* unquantified type: no outer forall *)
and tau = termType (* monomorphic type: no forall at all *)

(* types *)
and termType = Forall of id list * rho          (* forall a1 ... an. rho *)
               | TVar of typeVar                (* 'a *)
	       | TApp of string * termType list (* list int *)

(* Type variables represent substitutable parts of a type,
 * only |Free| variables can be unified*)
and typeVar = TypeVar of id * flavour

and flavour = Bound
              | Skolem
	      | Uni of termType option ref * rank ref (* updateable ref for subst *)

(* The type variable rank is used for efficient generalization
 * The rank corresponds with the depth of the earliest lamda binding that 
 * refers to this type variable. The depth of the outermost lambda binding in 
 * the environment is 0.                  
 * We use an infinite rank (rankInf) for type variables that do not occur in 
 * the environment. 
 * See: George Kuan and David McQueen, "Efficient ML type inference with ranked type variables" *)
and rank = int

(* The 'infinite' rank is used for variables that do not occur in the environment *)
let rankInf : rank = 0x7FFFFFFF

(* accessors *)
let tvId (TypeVar (id, _)) = id

let isUni flavour = 
  match flavour with
    | Uni _ -> true
    | _     -> false

let isSkolem flavour = 
  match flavour with
    | Skolem _ -> true
    | _     -> false

let tvFlavour (TypeVar (_, flavour)) =
  flavour

(* the "any type" annotation: (some a. a)  *)
let annotAny
  = let id = 0 in Annot ([id],  TVar (TypeVar (id, Bound)))


(* helper functions *)
let rec mkForall ids tp
    = match ids with
      | [] -> tp
      | _  -> match tp with
          | Forall (ids2, tp2) -> mkForall (ids @ ids2) tp2
	  | _                  -> Forall (ids, tp)

let mkAnnot ids tp
    = Annot (ids, tp)

let mkFun t1 t2
    = TApp ("->", t1::[t2])

let mkTuple t1 t2
    = TApp ("(,)", t1::[t2])

let ground tp = match tp with
  | TVar (TypeVar (_, (Uni (tyRef, _)))) ->
      begin
	let mtp = !tyRef in
	  match mtp with
	    | Some (t) -> t
	    | None     -> tp
      end
  | _ -> tp

(* *)
let rec string_of_term = function 
  | Var (nm)         -> nm

  | IntLit (lit)     -> string_of_int lit

  | FloatLit (lit)   -> string_of_float lit

  | Tup (t)          -> let t = List.map string_of_term t in
                        let t = String.concat ", " t in
			  "(" ^ t ^ ")"

  | StrLit s         -> "\"" ^ s ^ "\""

  | App (f, a)       -> let f = string_of_term f in
                        let a = string_of_term a in
			  "(" ^ f ^ " " ^ a ^ ")"

  | Lam (p, tm)      -> let tm = string_of_term tm in 
                        let concat x y = x ^ " " ^ y in
			let p = List.fold_left concat "" p in
			  "\\" ^ p ^ " . " ^ tm

  | ALam (p, a, tm)  -> let tm = string_of_term tm in
			let nmAnnot param annot = "(" ^ param ^ " :: " ^ (string_of_annot annot) ^ ")" in
			let nmAnnot = List.map2 nmAnnot p a in
			let nmAnnot = String.concat " " nmAnnot in
                          "\\" ^ nmAnnot ^ " . "  ^ tm

  | Let (nm, t1, t2) -> let t1 = string_of_term t1 in
                        let t2 = string_of_term t2 in 
			  "let " ^ nm ^ " = " ^ t1 ^ " in " ^ t2

  | If (cond, t1, t2)-> let t1 = string_of_term t1 in
                        let t2 = string_of_term t2 in
			let cond = string_of_term cond in
			  " if " ^ cond ^ "\nthen\n\t" ^ t1 ^ "\nelse\n\t" ^ t2 ^ "\n"

  | Ann (tm, annot)  -> let tm = string_of_term tm in
                        let annot = string_of_annot annot in
			  tm ^ ": " ^ annot
      

and string_of_annot (Annot (ids, termType)) =
    let idsStr = string_of_id_list ids in
    let tmStr  = string_of_type termType in
      idsStr ^ "." ^ tmStr
and string_of_id_list = function
  | []      -> ""
  | [id]    -> string_of_int id
  | id::ids -> (string_of_int id) ^ " " ^ (string_of_id_list ids)
and string_of_type = function
  | TApp (nm, args)  -> nm ^ "(" ^  (string_list args) ^ ")"
  | Forall (ids, tv) -> 
      let idStr = string_of_id_list ids in
      let tv = string_of_type tv in
	"forall " ^ idStr ^ " . " ^ tv
  | TVar (typeVar)   -> string_of_type_var typeVar
and string_list = function
  | []      -> ""
  | [ty]    -> string_of_type ty
  | ty::tys -> (string_of_type ty) ^ " " ^ (string_list tys)
and string_of_type_var = function
  | TypeVar (id, flavour) -> 
      let idStr = string_of_int (id) in
      let flv   = string_of_flavour flavour in
	flv ^ "(" ^idStr ^ ")"
and string_of_flavour = function
  | Bound -> "Bound"
  | Skolem -> "Skolem"
  | Uni (_, _) -> "Uni"


(**************
 * fresh type variables
 ************** *)
let rec range i j = if i > j then [] else i :: (range (i+1) j)


let idCnt = ref 0
let freshId () =
  idCnt := !idCnt + 1; !idCnt

(* return a fresh type variable *)
let freshTypeVar fl
  = let id = freshId () in
    TypeVar (id, fl)

(* return fresh type variables of a certain |Flavour| *)
let freshTypeVars fl n
  = List.map (fun _ -> freshTypeVar fl) (range 1 n)

(* return fresh skolem variables *)
let freshSkolems =
  freshTypeVars Skolem

(* return fresh bound variables *)
let freshBounds () =
  freshTypeVars Bound

(* return a fresh unifiable type *)
let freshTVar rank =
  let tvarRef = ref None in
  let rankRef = ref rank in 
  let tv      = freshTypeVar (Uni (tvarRef, rankRef)) in
    TVar tv

(* return fresh unifiable types *)
let freshTVars rank n =
  List.map (fun _ -> freshTVar rank) (range 1 n)


let mkIds names =
  let idMap = Hashtbl.create (List.length names) in
  let getId nm =
    try Hashtbl.find idMap nm
    with Not_found -> freshId ()
  in
    List.map getId names  

(* ----------------------------------------
 * order preserving set operations on lists
 * ---------------------------------------- *)
let union xs ys = 
  let diff = List.filter (fun y -> not (List.mem y xs)) ys in
    xs @ diff
  
let intersect xs ys =
  List.filter (fun y -> List.mem y xs) ys

let disjoint xs ys =
  List.for_all (fun x -> not (List.mem x ys)) xs
