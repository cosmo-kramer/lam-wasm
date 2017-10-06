type t

val of_string : string -> t
val to_string : t -> string

module Map : Map.S with type key = t
module Set : Set.S with type elt = t
