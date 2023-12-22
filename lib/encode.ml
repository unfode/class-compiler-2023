type value_type_spec = {shift: int; tag: int}

let num_spec : value_type_spec = {shift = 2; tag = 0b00}
let bool_spec : value_type_spec = {shift= 7; tag= 0b0011111}

type reference_type_spec = {tag: int}

let reference_type_shift : int = 3

let pair_spec : reference_type_spec = {tag = 0b010}
let function_spec : reference_type_spec = {tag = 0b110}

let get_mask ~(shift: int) : int = (1 lsl shift) - 1

let encode (value : int) (spec : value_type_spec) : int =
  (value lsl spec.shift) lor spec.tag

let encode_int (n : int) : int = encode n num_spec

let encode_bool (b : bool) : int =
  let bit = if b then 1 else 0 in
  encode bit bool_spec

