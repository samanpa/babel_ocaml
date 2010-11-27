open Utils
open Cgil

let bindings : (Typing.t list) Env.t = Env.create None;;


let monomorphize il = 
  il
