module S = Syn

exception TyErr of string

type tyctx = S.ty Var.Map.t

let int_type (ty : S.ty) : unit =
	match ty with
	| S.Tint | S.Tun -> ()
	| _ -> raise (TyErr "expected int or Un")

let unit_type (ty : S.ty) : unit =
	match ty with
	| S.Tunit | S.Tun -> ()
	| _ -> raise (TyErr "expected unit or Un")

let arrow_types (ty : S.ty) : S.ty * S.ty =
	match ty with
	| S.Tfun (t1, t2) -> t1, t2
	| S.Tun -> S.Tun, S.Tun
	| _ -> raise (TyErr "expected function or Un")

let contents_type (ty : S.ty) : S.ty =
	match ty with
	| S.Tref ty -> ty
	| S.Tun -> S.Tun
	| _ -> raise (TyErr "expected reference or Un")

let rec check_exp (ctx : tyctx) (e : S.exp) (ty : S.ty) : unit =
	match e with
	| S.Int _ -> int_type ty
	| S.Unit -> unit_type ty
	| S.Abs (x, e) ->
		let t1, t2 = arrow_types ty in
		let ctx = Var.Map.add x t1 ctx in
		check_exp ctx e t2
	| S.Let (x, e1, e2) ->
		let t1 = infer_exp ctx e1 in
		let ctx = Var.Map.add x t1 ctx in
		check_exp ctx e2 ty
	| S.Ref e -> check_exp ctx e (contents_type ty)
	| _ ->
		let t2 = infer_exp ctx e in
		if ty = t2 then ()
		else raise (TyErr "subsumption")

and infer_contents_type (ctx : tyctx) (e : S.exp) : S.ty =
	let ty = infer_exp ctx e in
	contents_type ty

and infer_exp (ctx : tyctx) (e : S.exp) : S.ty =
	match e with
	| S.Var x ->
		if Var.Map.mem x ctx then Var.Map.find x ctx
		else raise (TyErr ("variable " ^ Var.to_string x))
	| S.App (e1, e2) ->
		let ty = infer_exp ctx e1 in
		let t1, t2 = arrow_types ty in
		check_exp ctx e2 t1;
		t2
	| S.Let (x, e1, e2) ->
		let t1 = infer_exp ctx e1 in
		let ctx = Var.Map.add x t1 ctx in
		infer_exp ctx e2
	| S.Deref e -> infer_contents_type ctx e
	| S.Assign (e1, e2) ->
		let t2 = infer_contents_type ctx e1 in
		check_exp ctx e2 t2;
		S.Tunit
	| S.Asc (e, ty) -> check_exp ctx e ty; ty
	| _ -> raise (TyErr "type ascription required")

let check_dec (ctx : Context.t) (dec : S.dec) : Context.t =
	let S.ValDec (lab, x, e) = dec in
	let ty = infer_exp (Context.tyctx ctx) e in
	Context.add_exp ctx lab x ty

let check_decs (decs : S.decs) : unit =
	let ctx = List.fold_left check_dec Context.empty decs in
	ignore (Context.specs ctx)	(* i.e., we can infer a specs s.t. decs : specs *)
