open Llvm
open Llvm_executionengine
open Llvm_target
open Llvm_scalar_opts
open Llvm_bitreader

open Utils
open Typing
open Cgil
open Codegen

type exec_environment = {
  modules     : (string, Llvm.llmodule) Hashtbl.t;
  main_module : Llvm.llmodule;
  fpm         : [ `Function ] Llvm.PassManager.t;
  exec_engine : Llvm_executionengine.ExecutionEngine.t;
  llcontext   : llcontext;
}



let init_fpm exec_engine fpm =
  (* Set up the optimizer pipeline.  Start with registering info about how the
   * target lays out data structures. *)
  TargetData.add (ExecutionEngine.target_data exec_engine) fpm;
  
  (* Promote allocas to registers. *)
  add_memory_to_register_promotion fpm;

  (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
  add_instruction_combination fpm;
  
  (* reassociate expressions. *)
  add_reassociation fpm;

  (* Eliminate Common SubExpressions. *)
  add_gvn fpm;

  (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
  add_cfg_simplification fpm;

  ignore (PassManager.initialize fpm)
  
(* opens a module
 * create a function pass manager
 * run all functions in the module with the fpm *)
let open_module ctx modname =
  let buff = MemoryBuffer.of_file (modname ^ ".bc") in
  let modl = get_module ctx buff in
  let fpm  = PassManager.create_function modl in
  let run_func fn = PassManager.run_function fn fpm; Llvm_analysis.assert_valid_function fn in
  let _    = iter_functions run_func modl in
    (modl, fpm)

let init_context name =
  let ctx = Llvm.global_context () in

  (* Create the JIT. *)
  let _ = initialize_native_target () in
  let (main_module, fpm) = open_module ctx "initial_basis" in
  let exec_engine = ExecutionEngine.create main_module in
  let _   = init_fpm exec_engine fpm in
    {
      modules = Hashtbl.create 4;
      main_module = main_module;
      fpm=fpm;
      exec_engine=exec_engine;
      llcontext = ctx;
    }



(* define a few global bindings *)
let ctx = init_context "main" ;;
let builder        = Llvm.builder ctx.llcontext
let main_module_nm = "main"


let rec lltype_from_ty ty = match ty with
  | Unit              -> Llvm.void_type ctx.llcontext
  | Int               -> Llvm.i32_type ctx.llcontext
  | Float             -> Llvm.float_type ctx.llcontext
  | FunTy (ptys, rty) ->
      let fn ty = 
	let llty = lltype_from_ty ty in
	  match ty with
              (* pass pointer to function types *)
            | FunTy _ -> Llvm.pointer_type llty
            | _       -> llty
      in  
      let llparamTypes = Array.of_list (List.map fn ptys) in
      let llreturnType = lltype_from_ty rty in
        function_type llreturnType llparamTypes
  | String           ->
      let char_type = Llvm.i8_type ctx.llcontext in
	Llvm.pointer_type char_type
  | _ -> raise (Error ("type not supported " ^ (string_of_ty ty)))



let lltype_is_void lltype = match classify_type lltype with
  | TypeKind.Void -> true
  | _ -> false



let translate_name name = 
  match name with
    | "+" -> "plus"
    | "-" -> "minus"
    | "/" -> "diff"
    | "*" -> "times"
    | nm  -> nm
;;

let codegen_proto_in_env env name paramNames ty =
  let ft = lltype_from_ty ty in
  let the_func = declare_function name ft ctx.main_module in
  let _ = Env.put env name the_func in
  let llparams = Array.to_list (Llvm.params the_func) in
    (* Set names for all arguments. *)
    List.iter2 set_value_name paramNames llparams;
    the_func

let rec codegen_lambda env llfunction paramNames body =
  (* create a new environment *)
  let funcEnv = Env.create (Some env) in
	
  (* Create a new basic block to start insertion into. *)
  let func_bb = append_block ctx.llcontext "funcentry" llfunction in
  let _ = position_at_end func_bb builder in
    begin
      let llparams = Array.to_list (params llfunction) in
      let _ = List.iter2 (Env.put funcEnv) paramNames llparams in
	try
	  let ret_val = codegen_expr_in_env funcEnv body in
	    (* Finish off the function. *)
	    match lltype_is_void (type_of ret_val) with
              | true -> build_ret_void builder
              | false -> build_ret ret_val builder
	with e ->
	  delete_function llfunction;
	  raise e
    end
  
and codegen_expr_in_env env expr = match expr with
  | Lint (num)           -> const_int (Llvm.i32_type ctx.llcontext) num
  | Lfn (name, ty, paramNames, body)       ->
      let name = translate_name name in
      let _    = print_endline ("fn -> " ^ name) in
      (* create the function prototype *)
      let llfunction = codegen_proto_in_env env name paramNames ty in
	
      (* check to see if we have an external function (empty body) *)
      let _ = match body with 
	| Ext -> llfunction
	| Def (body) -> codegen_lambda env llfunction paramNames body ;
	    Llvm_analysis.assert_valid_function llfunction;
	    llfunction
      in
	(* Validate the generated code, checking for consistency. *)
	Llvm.dump_module (ctx.main_module);
	PassManager.run_function llfunction ctx.fpm;
	llfunction
  | Llet (name, e1) ->
      let name = translate_name name in
      let e1 = codegen_expr_in_env env e1 in
      let _  = Env.put env name e1 in
	e1
  | Lvar (nm) ->
      let nm = translate_name nm in
      begin
	match Env.find env nm with
	  | Some (value) -> value
	  | None -> raise (Error ("variable " ^ nm ^ " is not found"))
      end
  | Lapply (callee, args) ->
      let callee = codegen_expr_in_env env callee in
      let callee_type = element_type (type_of callee) in
      let param_tys = param_types callee_type in
      let return_type = return_type callee_type in

      (* If argument mismatch error. *)
      let _ = if Array.length param_tys == List.length args then () else
        raise (Error "incorrect # arguments passed");
      in
	(* if the return type is void we can't name the build_call *)
      let name = if lltype_is_void return_type then "" else "calltmp" in
      let args = List.map (codegen_expr_in_env env) args in
      let res = build_call callee (Array.of_list args) name builder in
	Llvm.dump_module (ctx.main_module);
	res
  | Lseq (e1, e2) ->
      let _ = codegen_expr_in_env env e1 in
	codegen_expr_in_env env e2
  | _ -> failwith ("Can't codegenerate " )

let global_env : llvalue Env.t = Env.create None;;

let codegen module_name expr =
  try
    let _ = print_endline module_name in
    let expr = codegen_expr_in_env global_env expr in
    let _ = dump_module ctx.main_module in
      expr
  with
      Error (s) ->
        print_endline ("Unexpected error " ^ s);
        raise (Error s)
    | Not_bound s ->
        print_endline ("Not bound " ^ s);
        raise (Error s)
    | Already_bound s ->
        print_endline ("Already bound " ^ s);
        raise (Error s)

let execute_expr llval funty =
  (* funty is a function type*)
  let retty = return_type funty in
  let numParams = Array.length (param_types funty) in
    if numParams == 0 then
      (* JIT the function, returning a function pointer. *)
      let _ = dump_value llval in
      let _ = Llvm_analysis.assert_valid_module ctx.main_module in
      let _ = print_endline "FDASF" in
      let result = ExecutionEngine.run_function llval [||] ctx.exec_engine in
      let _ = print_endline "FDASF" in
      let kind = classify_type retty in
      let _ = print_string "EVALUATE TO: " in
	begin
	  match kind with
            | TypeKind.Integer -> print_int (GenericValue.as_int result)
            | _ -> raise (Error ("unhandled  type " ^ (string_of_lltype retty)))
	end ;
	print_newline ()
  else
      ()
let eval_expr llval =
  let llty = type_of llval in
    match classify_type llty with
      | TypeKind.Pointer ->
	  begin
	    let elemty = element_type llty in	    
	      match classify_type elemty with
		  (* we expect a function pointer at this point *)
		| TypeKind.Function -> execute_expr llval elemty
		| _ -> ()
	  end
      | _ -> ()


let backend =
  {
    compile  = codegen ;
    evaluate = eval_expr;
  }
