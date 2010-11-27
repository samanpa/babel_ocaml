let fib n =
  let ( > ) = int_gt in
  let ( + ) = int_add in
  let ( - ) = int_minus in
    if n > 1 then
      let fib1 = fib (n - 1) in
      let fib2 = fib (n - 2) in
	fib1 + fib2 
    else
      1
;;

