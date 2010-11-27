exception Already_bound of string
exception Not_bound of string
exception Error of string

let (>>) arg fn = fn arg

let cnt = ref 0
let get_new_name prefix =
  let _ = cnt := 1 + !cnt in
    prefix ^ string_of_int !cnt

module ArrayUtils = struct
  let iter2 f a1 a2 =
    if Array.length a1 <> Array.length a2
    then raise (Invalid_argument "Array.iter2");
    for i = 0 to Array.length a1 - 1 do
      f a1.(i) a2.(i);
    done;;
end

module Env = struct
  type 'a t = {
    bindings : (string, 'a) Hashtbl.t;
    parent   : 'a t option;
    level    : int;
  }

  let create parent_opt = {
    bindings = Hashtbl.create 10;
    parent   = parent_opt;
    level    = match parent_opt with
      | None -> 0
      | Some (parent) -> parent.level + 1
  }


  let rec find env name =
    try
      let res = Hashtbl.find env.bindings name in
        Some (res)
    with Not_found ->
      match env.parent with
        | None -> None
        | Some (parent_env) -> find parent_env name
  ;;

  let put env name value =
    Hashtbl.add env.bindings name value

  let add env name value = Hashtbl.add env.bindings name value
end
