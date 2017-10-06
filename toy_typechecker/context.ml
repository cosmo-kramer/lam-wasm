module S = Syn

type tyctx = S.ty Var.Map.t

type t = {
	vars : tyctx;
	labs : Var.t Lab.Map.t;
	order : Lab.t list;	(* backwards *)
}

let empty : t = {
	vars=Var.Map.empty;
	labs=Lab.Map.empty;
	order=[];
}

let add_exp (ctx : t) (lab : Lab.t) (x : Var.t) (ty : S.ty) : t =
	let vars = Var.Map.add x ty ctx.vars in
	let labs = Lab.Map.add lab x ctx.labs in
	let order = lab :: ctx.order in
	{vars; labs; order}

let tyctx (ctx : t) : tyctx = ctx.vars

type acc = S.specs * Lab.Set.t
let specs' (ctx : t) ((specs, labs) as acc : acc) (lab : Lab.t) : acc =
	if Lab.Set.mem lab labs then
		acc	(* i.e., lab shadowed *)
	else
		let x = Lab.Map.find lab ctx.labs in
		let ty = Var.Map.find x ctx.vars in
		let spec = S.ValSpec (lab, ty) in
		let specs = spec :: specs in
		let labs = Lab.Set.add lab labs in
		specs, labs

let specs (ctx : t) : S.specs =
	let acc = [], Lab.Set.empty in
	let folder = specs' ctx in
	let specs, _ = List.fold_left folder acc ctx.order in
	specs
