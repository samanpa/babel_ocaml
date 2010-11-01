external nop : unit -> unit ;;
external int_add : int -> int -> int ;;
external int_minus : int -> int -> int ;;
external int_div : int -> int -> int ;;
external int_mul : int -> int ->int ;;

let ( + ) = int_add;;
let ( - ) = int_minus;;
let ( / ) = int_div;;
let ( * ) = int_mul;;

