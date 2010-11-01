open Cgil

type 'a backend = {
  (* module name -> expr to generate -> unit *)
  compile  : string -> cgil -> 'a;

  evaluate : 'a -> unit
}
