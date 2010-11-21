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


let count n =
  if int_gt n 0 then
    int_add 1 (count (int_minus n 1))
  else
    0
;;

let fib n =
  if int_gt n 1 then
    let fib1 = fib (int_minus n 1) in
    let fib2 = fib (int_minus n 2) in
	int_add fib1 fib2 
    else
      1
;;

