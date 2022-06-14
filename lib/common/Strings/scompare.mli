(* Compare strings by taking into account numerical values 
 *
 * Thus, "a.10.20 ok 030.40" "a.010.020 ok 30.040" are considered equal 
 *
 * Uses Text (utf). *)


val cmp_num_string: Text.t -> Text.t -> int
