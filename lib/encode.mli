type datatype_spec = {shift: int; tag: int}

val get_mask : datatype_spec -> int

val num_spec : datatype_spec

val bool_spec : datatype_spec

val pair_spec : datatype_spec

val encode_int : int -> int

val encode_bool : bool -> int
