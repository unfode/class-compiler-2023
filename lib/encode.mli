type value_type_spec = {shift: int; tag: int}

val num_spec : value_type_spec
val bool_spec : value_type_spec

val get_mask : value_type_spec -> int

type reference_type_spec = {tag: int}

val reference_type_mask : int
val reference_type_shift : int

val pair_spec : reference_type_spec
val function_spec : reference_type_spec

val encode_int : int -> int
val encode_bool : bool -> int

