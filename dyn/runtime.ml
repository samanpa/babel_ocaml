open Utils
open Value
open InitialBasis

let int_con v = IntValue (v)
let bool_con b = Boolean (b)
let list_con l = ListValue (l)
let float_con f = FloatValue (f)

let mk_unary fn =
  let wrapped lst =
    match lst with 
      | [a] -> fn a
      | _   -> Error ("number of args not 1")
  in
    Primitive (wrapped)

let mk_binop fn =
  let wrapped lst =
    match lst with
      | [a; b] -> fn a b
      | _ -> Error ("number of args not 2")
  in
    Primitive (wrapped)

let mk_tertiary fn =
  let wrapped lst =
    match lst with
      | [a; b; c] -> fn a b c
      | _ -> Error ("number of args not 3")
  in
    Primitive (wrapped)

let apply_int_op con fn a1 a2 =
  match a1, a2 with
    | Error (err), _ -> Error (err)
    | _, Error (err) -> Error (err)
    | (IntValue a1, IntValue a2) -> con (fn a1 a2)
    | _ -> Error ("can't apply function to non ints")

let apply_float_op con fn a1 a2 =
  match a1, a2 with
    | Error (err), _ -> Error (err)
    | _, Error (err) -> Error (err)
    | (FloatValue a1, FloatValue a2) -> con (fn a1 a2)
    | _ -> Error ("can't apply function to non ints")

let initial_basis : value Env.t = Env.create None;;
List.map 
  (fun (nm, vl) -> Env.put initial_basis nm vl)
  [
    ("int_add",   mk_binop (apply_int_op int_con ( + )));
    ("int_minus", mk_binop (apply_int_op int_con ( - )));
    ("int_div",   mk_binop (apply_int_op int_con ( / )));
    ("int_mul",   mk_binop (apply_int_op int_con ( * )));
    ("int_gt",    mk_binop (apply_int_op bool_con ( > )));
    ("int_lt",    mk_binop (apply_int_op bool_con ( < )));
    ("%",   mk_binop (apply_int_op int_con ( mod )));
    ("int_eq",  mk_binop (apply_int_op bool_con ( == )));


    ("float_add",   mk_binop (apply_float_op float_con ( +. )));
    ("float_minus", mk_binop (apply_float_op float_con ( -. )));
    ("float_div",   mk_binop (apply_float_op float_con ( /. )));
    ("float_mul",   mk_binop (apply_float_op float_con ( *. )));
    ("float_gt",    mk_binop (apply_float_op bool_con ( > )));
    ("float_lt",    mk_binop (apply_float_op bool_con ( < )));
    ("float",    mk_unary to_float);

    ("str_eq",      mk_binop (str_eq));

    ("typeof", mk_unary typeof);
    ("str",    mk_unary str);

    (* list operators *)
    ("::",  mk_binop con);
    ("hd",  mk_unary head);
    ("tl",  mk_unary tail);
    ("getItem", mk_binop getItem);

    ("isEmpty", mk_unary isEmpty);

    (* constructors *)
    ("true",  Boolean (true));
    ("false", Boolean (false));
    ("none",  Nothing);
    ("tuple", Primitive (con_tuple));
    ("error", mk_unary con_error);
    ("nil",   ListValue ([]));
    ("mk_map",  mk_unary mk_map);
      
    ("==",    mk_binop equal);

    (* map operators *)
    ("map_add", mk_tertiary map_add);
  ]
;;
