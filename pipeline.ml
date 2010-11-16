open Parse
open Ast
open Utils
open Codegen

let backend = Llvm_codegen.backend

let print_toplevel toplevel = 
  let toplevel_str = Ast.string_of_toplevel toplevel in
  let _   = print_endline ("parsed expr:\n\t" ^ toplevel_str) in
    toplevel

let print_tree fn tree =
  let str = fn tree in
  let _   = print_endline ("tree :\n\t" ^ str) in
    tree

let evaluate eval expr =
  if eval then
    (backend.evaluate expr; ())
  else
    ()
  
let compile module_name cgil =
  cgil >>
    (print_tree Cgil.to_string) >>
    Semant.process >> 
    (print_tree Cgil.to_string) >>
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
	Elaborate.elaborate >>
	Texpr.convert_ast >>
	InferBasic.inferType >>
	fun (lty, ty) -> (Typing.convert_type ty, lty) >>
	Cgil.wrap >>
	(print_tree Cgil.to_string) >>
	compile module_name>>
	(evaluate eval)
  | Extern (nm, ty) -> 
      let _   = Elaborate.elaborate_extern nm in
      let ty  = Texpr.convert_from_astTy ty in
      (* add to gamma to aid in type inference *)
      let _   = Gamma.gammaExtend Gamma.gamma0 nm ty in

      (* convert to Typing.t *)
      let fty = Typing.convert_type ty in 
      let numParams = Currying.count_params fty in
      let params = List.map (fun n -> "params" ^ (string_of_int n)) (Types.range 1 numParams) in

      let lam = Cgil.Lfn (nm, fty, params, Cgil.Ext) in
      let _ = compile module_name lam in
	()
 
;;

