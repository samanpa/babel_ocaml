external int_add : int -> int -> int ;;
external int_minus : int -> int -> int ;;
external int_div : int -> int -> int ;;
external int_mul : int -> int -> int ;;
external int_gt : int -> int -> bool;;
external int_lt : int -> int -> bool;;

let max a b =
  if int_gt a b then
    a
  else
    b
;;

let min a b = 
  if int_lt a b then
    a
  else
    b
;;
