type datatype_spec = {shift: int; tag: int}

let get_mask (spec : datatype_spec) : int = (1 lsl spec.shift) - 1

let num_spec : datatype_spec = {shift= 2; tag= 0b00}

let bool_spec : datatype_spec = {shift= 7; tag= 0b0011111}

let pair_spec : datatype_spec = {shift= 3; tag= 0b010}

let encode (value : int) (spec : datatype_spec) : int =
  (value lsl spec.shift) lor spec.tag

let encode_int (n : int) : int = encode n num_spec

let encode_bool (b : bool) : int =
  let bit = if b then 1 else 0 in
  encode bit bool_spec
