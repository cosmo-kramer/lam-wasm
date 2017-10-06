exception TyErr of string

type tyctx = Syn.ty Var.Map.t

val check_exp : tyctx -> Syn.exp -> Syn.ty -> unit
val infer_exp : tyctx -> Syn.exp -> Syn.ty

val check_decs : Syn.decs -> unit
