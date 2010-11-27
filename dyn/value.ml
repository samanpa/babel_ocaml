open Lambda

type value = | IntValue of int
	     | FloatValue of float
	     | StringValue of string
	     | Boolean of bool
	     | Error of string
	     | Primitive of (value list -> value)
	     | Function of name list * lambda
	     | ListValue of value list
	     | TupleValue of value list
	     | Map of (value * value) list ref
	     | Tensor of value list
	     | Nothing
