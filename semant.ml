open Utils


let process expr = 
  expr >>
    Lambda_lifting.lift >>
    Currying.uncurry
