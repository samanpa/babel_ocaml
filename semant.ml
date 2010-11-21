open Utils


let process expr = 
  expr >>
    Lambda_lifting.lift >>
    Monomorphize.monomorphize >>
    Currying.uncurry
