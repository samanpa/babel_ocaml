open Parse
open Ast
open Utils
open Codegen

let backend = Llvm_codegen.backend

let print_toplevel toplevel = 
  let toplevel_str = Ast.string_of_toplevel toplevel in
  let _   = print_endline ("parsed expr:\n\t" ^ toplevel_str) in
    toplevel

let print_lty lty =
  let str = Cgil.to_string lty in
  let _   = print_endline ("typed expr :\n\t" ^ str) in
    lty

let evaluate eval expr =
  if eval then
    (backend.evaluate expr; ())
  else
    ()
  
let compile module_name lty =
  lty >>
    print_lty >>
    Semant.process >> 
    print_lty >>
   (backend.compile module_name)

(* push a top level expression into the code generation pipeline *)
let rec push module_name toplevel eval = 
  toplevel >>
    print_toplevel >>
    (fun tl -> handle_top_level module_name eval tl)

and handle_top_level module_name eval = function
  | Open (file_name) ->
      let toplevels = parse_file (file_name ^ ".ts") in
      let module_name = file_name in
      let fn toplevel = push module_name toplevel false in
      let _ = List.map fn toplevels in
	()
  | Expr (ex) -> 
      ex >>
	Texpr.convert_ast >>
	InferBasic.inferType >>
	fun (lty, ty) -> (Typing.convert_type ty, lty) >>
	Cgil.wrap >>
	print_lty >>
	compile module_name>>
	(evaluate eval)
  | Extern (nm, tp) -> 
      let tp  = Texpr.convert_from_astTy tp in
      let _   = Gamma.gammaExtend Gamma.gamma0 nm tp in
      let tp  = Typing.convert_type tp in
      let fty = Currying.uncurry_funty tp in (* decurry function *)
      let ps  = match fty with (* get parameer names *)
	| Typing.FunTy (pty, retTy) -> List.map (fun _ -> Utils.get_new_name "params") pty
	| _ -> failwith (nm ^ " does not have a function type ")
      in
      let lam = Cgil.Lfn (nm, fty, ps, Cgil.Ext) in
      let _ = compile module_name lam in
	()
 
;;

