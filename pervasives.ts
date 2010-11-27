let app_num_fn int_fun float_fun a b=
  let a_ty = typeof a in
  let b_ty = typeof b in
    if a_ty == "int" then
      if b_ty == "int" then
	int_fun a b
      else
	if b_ty == "float" then
	  float_fun (float a) b
	else
	  error "not adding two numbers"
    else
      if a_ty == "float" then
	if b_ty == "float" then
	  float_fun a b
	else
	  if b_ty == "int" then
	    float_fun a (float b)
	  else
	    error "not adding two numbers"
      else
	error "not adding two numbers"
;;

let ( + ) a b = app_num_fn int_add float_add a b;;
let ( - ) a b = app_num_fn int_minus float_minus a b;;
let ( / ) a b = app_num_fn int_div float_div a b;;
let ( * ) a b = app_num_fn int_mul float_mul a b;;
let ( > ) a b = app_num_fn int_gt float_gt a b;;
let ( < ) a b = app_num_fn int_lt float_lt a b;;

let map fn lst =
  if isEmpty lst then
    nil
  else
    let head = hd lst in
    let tail = tl lst in
      ((fn head) :: (map fn tail))
;;

let new ty =
  if ty == "str" then
    if ty == "map" then
      mk_map ()
    else if ty == "list" then
      nil
    else
      error "not supported"
  else
    error "string expected"
;;

let filter pred lst =
  if isEmpty lst then
    nil
  else
    let head = hd lst in
    let tail = tl lst in
    let res = pred head in
    let res =
      if res then
	head :: (filter pred tail)
      else
	filter pred tail
    in
      res
;;


let rng acc first last =
  if first > last then
    acc
  else
    first :: (rng acc (first + 1) last)
;;

let range first last =
    rng nil first last
;;

let (..) = range;;

