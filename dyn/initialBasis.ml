open Value

let typeof obj = 
  let str =
    match obj with
      | IntValue _    -> "int"
      | Error _       -> "err"
      | Function _    -> "fun"
      | Boolean _     -> "bool"
      | FloatValue _  -> "float"
      | StringValue _ -> "str"
      | ListValue _   -> "list"
      | TupleValue _  -> "tuple"
      | Tensor _      -> "tensor"
      | Nothing       -> "none"
      | Map _         -> "map"
      | Primitive _   -> "primitive"
  in
    StringValue (str)

let rec to_string = function 
  | IntValue (v)  -> string_of_int v
  | Error (s)     -> "ERROR " ^ s
  | Primitive _   -> "primop"
  | Function _    -> "fun"
  | Boolean b     -> string_of_bool b
  | FloatValue f  -> string_of_float f
  | StringValue s -> "\"" ^ s ^  "\""
  | ListValue l   ->
      let l = List.map to_string l in
      let str = String.concat ", " l in
	"[" ^ str ^ "]"
  | TupleValue t  ->
      let t = List.map to_string t in
      let str = String.concat "," t in
	"(" ^ str ^ ")"
  | Tensor t      -> "tensor"
  | Nothing       -> "none"
  | Map (items)   -> 
      let f (x,y) = (to_string x) ^ ": " ^ (to_string y) in
      let l = List.map f !items in
      let str = String.concat ", " l in
	"{" ^ str ^ "}"
and str obj =
  StringValue (to_string obj)

let isTrue = function
  | IntValue (x) -> if x == 0 then false else true
  | Error (x)    -> false
  | Boolean (b)  -> b
  | ListValue l  -> (List.length l) > 0
  | Nothing      -> false
  | _            -> true
;;


(* error operations *)
let con_error str =
  match str with
    | StringValue (str) ->  Error (str)
    | _ -> Error ("can not construct error object")

(* list operations *)
let con elem lst =
  match lst with 
    | ListValue l -> ListValue (elem::l)
    | _ -> Error ("not a list")

let con_tuple lst = 
  TupleValue lst

let isEmpty lst =
  match lst with
    | ListValue l ->
	begin 
	  match l with
	    | [] -> Boolean (true)
	    | _  -> Boolean (false)
	end
    | _ -> Error ("not a lst")

let head lst = 
  match lst with 
    | ListValue l ->
	begin 
	  match l with
	    | a :: b -> a
	    | [] -> Error ("empty list")
	end
    | _ -> Error ("not a list")
	
let tail lst = 
  match lst with 
    | ListValue l ->
	begin 
	  match l with
	    | a :: b -> ListValue (b)
	    | [] -> Error ("empty list")
	end
    | _ -> Error ("not a list")

(* string operations *)
let str_eq str1 str2 = 
  match (str1, str2) with
    | (StringValue str1, StringValue str2) -> Boolean (String.compare str1 str2 == 0)
    | _ -> Error ("not strings ")


(* float operations *)
let to_float = function
  | FloatValue f -> FloatValue f
  | IntValue f   -> FloatValue (float f)
  | n            -> Error ((to_string n) ^ " is not a string")

(* general operations *)
let rec equal_ a b =
  match (a, b) with
    | IntValue i1, IntValue i2 -> i1 == i2
    | StringValue s1, StringValue s2 -> String.compare s1 s2 == 0
    | TupleValue t1, TupleValue t2 ->
	if List.length t1 == List.length t2 then
	  let cmp = List.map2 equal_ t1 t2 in
	    List.fold_right (&&) cmp true
	else
	  false
    | _ -> false
	  
let equal a b =
  Boolean (equal_ a b)


let getItem e1 e2 =
  match e1 with 
    | ListValue l 
    | TupleValue l -> 
	begin
	  match e2 with
	    | IntValue i -> 
		if List.length l <= i then
		  Error ("i too big")
		else
		  List.nth l i
	    | _ -> Error ("int expected as index")
	end
    | Map (items) -> 
	let find (x, y) = equal_ x e2 in
	let res = List.filter find !items in
	  begin
	    match res with 
	      | [] -> Error ((to_string e2) ^ " does not exist in map")
	      | (a,b)::x -> b
	  end
    | _ -> Error ("indexing not supported on non lists")
;;

let mk_map lst =
  Map (ref [])

let map_add map newkey newval =
  (* using mutable variable makes me feel unclean *)
  let replaced = ref false in
  let replace (origkey, origval) =
    match equal_ newkey origkey with
      | true  -> replaced.contents <- true; (origkey, newval)
      | false -> (origkey, origval)
  in
  match map with
    | Map (items) -> 
	let newItems = List.map replace !items in
	let newItems = 
	  match !replaced with
	    | true  -> newItems
	    | false -> (newkey, newval) :: newItems
	in
	let _ = items.contents <- newItems in
	  Map (ref newItems)
    | _ -> Error ("not a map")
