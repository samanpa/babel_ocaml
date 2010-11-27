open Lambda
open Utils
open Runtime
open Value
open InitialBasis

let rec doEval bindings = function
  | UnitEx        -> Nothing
  | IntEx (value) -> IntValue (value) 
  | FloatEx f -> FloatValue (f)
  | LetEx (nm, e1, e2) ->
      let e1 = doEval bindings e1 in
	begin
	  match e1 with
	    | Error s -> Error (s) 
	    | _ -> let _ = Env.put bindings nm e1 in
		doEval bindings e2
	end
  | VarEx v -> 
      begin
	match Env.find bindings v
	with
	  | Some v -> v
	  | None -> Error (v ^ " not found")
      end
  | CallEx (f, args) ->
      let f = doEval bindings f in
      let args = List.map (doEval bindings) args in
	begin
	  match (f) with
	    | Primitive (f) -> f args
	    | Function (p, body) ->
		let bindings = Env.create (Some (bindings)) in
		let _ = List.iter2 (fun p arg -> Env.put bindings p arg) p args in
		  doEval bindings body
	    | Error s -> Error s
	    | _ -> Error (to_string f ^ ". Not a function")
	end
  | LamEx (p, body) -> Function (p, body)
  | IfEx (cond, e1, e2) -> 
      let cond = doEval bindings cond in
      let expr = if isTrue cond then e1 else e2 in
	doEval bindings expr
  | StringEx (st) -> StringValue (st)
	  

let bindings = Env.create (Some (initial_basis));;
let rec eval expr =
  doEval bindings expr

