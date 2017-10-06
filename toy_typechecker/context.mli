type t

val empty : t

val add_exp  : t -> Lab.t -> Var.t -> Syn.ty -> t

val tyctx : t -> Syn.ty Var.Map.t
val specs : t -> Syn.specs
