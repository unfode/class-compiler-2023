type value_type_spec = {shift: int; tag: int}

val num_spec : value_type_spec
val bool_spec : value_type_spec

type reference_type_spec = {tag: int}

val reference_type_shift : int

val pair_spec : reference_type_spec
val function_spec : reference_type_spec

val get_mask : shift:int -> int

val encode_int : int -> int
val encode_bool : bool -> int

