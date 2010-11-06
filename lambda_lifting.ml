open Cgil

let rec lambda_lift acc = function 
  | Llet (nm, e1) ->
      let (acc, e1) = lambda_lift acc e1 in
	(acc, Llet (nm, e1))
  | Lfn (nm, t, p, fnDef) ->
      let res = match fnDef with
	| Ext -> (acc, Lfn (nm, t, p, fnDef))
	| Def b ->
	    let (acc, b) = lambda_lift acc b in
	    let fn = Lfn (nm, t, p, Def (b)) in
	    let _ =       print_endline ("FN [[" ^ (to_string fn) ^ "]]")  in

	      ((fn::acc), Lvar (nm))
      in
	res
  | Lapply (fn, args) ->
      let (acc, fn) = lambda_lift acc fn in
      let args  = List.map (lambda_lift []) args in
      let args' = List.map (fun (_,ex) -> ex) args in
      let acc'  = List.map (fun (acc,_) -> acc) args in
      let acc'  = List.fold_right (@) acc' acc in
	(acc', Lapply (fn, args'))
  | Lif (e1, e2, e3) ->
      let (acc, e1) = lambda_lift acc e1 in
      let (acc, e2) = lambda_lift acc e2 in
      let (acc, e3) = lambda_lift acc e3 in
	(acc, Lif (e1, e2, e3))
  | Llet (nm, e) ->
      let (acc, e) = lambda_lift acc e in
	(acc, Llet (nm, e))
  | Lseq (e1, e2) ->
      let (acc, e1) = lambda_lift acc e1 in
      let (acc, e2) = lambda_lift acc e2 in
	(acc, Lseq (e1, e2))
  | e -> (acc, e)
;;

let lift ex = 
  let lst, ex = lambda_lift [] ex in
  let rec mk_seq = function
    | [] -> failwith "empty expression"
    | [e1] -> e1
    | e1::rest -> print_endline ("EXPR [[" ^ (to_string e1) ^ "]]")  ;Lseq (e1, mk_seq rest)
  in
  let lst = ex :: lst in
  let lst = List.rev lst in
    mk_seq lst
;;
