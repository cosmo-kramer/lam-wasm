type t

(** Construct a fresh variable with the given base name. *)
val fresh : string -> t

(**
	Return *the* variable with the given name.

	Raises [Invalid_arg] if the name is empty or starts with
	['$'].
*)
val of_string : string -> t

val to_string : t -> string

module Map : Map.S with type key = t
module Set : Set.S with type elt = t
