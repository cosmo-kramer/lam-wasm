open Map
open Utils
module S = Utils
exception TyErr of string
(*
let global_ctx: ((ty Global_ctx.t) ref) = ref Global_ctx.empty

*)

let check_ref erased phi1 phi2 = true

let int_type (ty : S.ty) (t: term): unit =
	match ty with
        | S.R (Tint, phi)  ->      (match (t, phi) with
                                        | (Val n, Eq (None, Some (Val i))) | (Val n, Eq (Some (Val i), None))  -> if i = n then () else raise (TyErr "int_type pred mismatch(i!=n)!")
                                        | _ -> raise (TyErr "int_type pred mismatch!"))
        | S.Tun -> ()
	| _ -> raise (TyErr "expected int or Un")

let closed = function
        | Eq (Some _, Some _) | Leq (Some _, Some _) | Un (Some _) | Tr -> true
        | _ -> false
(*
let unit_type (ty : S.ty) ctx : unit =
	match ty with
	| S.R (Tunit, phi) -> if closed phi && check_ref (erase ctx) Tr phi then () (* Tunit expects a closed phi, Val 1 is dummy *)
                                else raise (TyErr "expected unit or Un")
        | S.Tun -> ()           
	| _ -> raise (TyErr "expected unit or Un")
*)
let arrow_types (ty : S.ty) : S.ty * S.ty =
	match ty with
	| S.Tfun (t1, t2) -> t1, t2
	| S.Tun -> S.Tun, S.Tun
	| _ -> raise (TyErr "expected function or Un")

let contents_type (ty : S.ty) : S.ty =
	match ty with
	| S.Tref (x, phi) -> S.R (x, phi)
	| S.Tun -> S.Tun
	| _ -> raise (TyErr "expected reference or Un")


let rec subtype t2 ty ctx : bool = match (t2, ty) with
        | (Tun, R (_, Un None)) -> true
        | (R (_, Un None), Tun) -> true
        | (R (t1, phi1), (R (t2, phi2))) -> (t1=t2) && check_ref (erase ctx) phi1 phi2     
        | _ -> false 


let rec check_exp (ctx : (S.ty Context.t)) (e : S.term) (ty : S.ty) : unit =
	match e with
	| S.Val _ -> int_type ty e 
	| S.Abs (_, x, e) ->
		let t1, t2 = arrow_types ty in
		let ctx = Context.add x t1 ctx in
		check_exp ctx e t2
	| S.Let (x, e1, e2) ->
		let t1 = infer_exp ctx e1 in
		let ctx = Context.add x t1 ctx in
		check_exp ctx e2 ty
	| S.Ref e -> check_exp ctx e (contents_type ty)
	| _ ->
		let t2 = infer_exp ctx e in
		if subtype t2 ty ctx then ()                (* subtype t2 ty -> t2 is subtype of ty *)
		else raise (TyErr "subsumption")

and infer_contents_type ctx (e : S.term) : S.ty =
	let ty = infer_exp ctx e in
	contents_type ty

and infer_exp ctx (e : S.term) : S.ty =
	match e with
        | S.Var name ->   
                        ( try 
                Context.find name ctx
                        with Not_found -> raise (TyErr ("Var "^name^" not found!")))
	| S.App (e1, e2) ->
		let ty = infer_exp ctx e1 in
		let t1, t2 = arrow_types ty in
		check_exp ctx e2 t1;
		t2
	| S.Let (x, e1, e2) ->
		let t1 = infer_exp ctx e1 in
		let ctx = Context.add x t1 ctx in
		infer_exp ctx e2
	| S.Deref e -> infer_contents_type ctx e
	| S.Assign (e1, e2) ->
		let t2 = infer_contents_type ctx e1 in
		check_exp ctx e2 t2;
		(S.R (S.Tunit, Tr)) 
	| S.Asc (e, ty) -> check_exp ctx e ty; ty
        | _ -> raise (TyErr ("type ascription required  "^S.term_to_string e)); Tun 

       (*
let check_dec (ctx : Context.t) (dec : S.dec) : Context.t =
	let S.ValDec (lab, x, e) = dec in
	let ty = infer_exp (Context.tyctx ctx) e in
	Context.add_exp ctx lab x ty

let check_decs (decs : S.decs) : unit =
	let ctx = List.fold_left check_dec Context.empty decs in
	ignore (Context.specs ctx)	(* i.e., we can infer a specs s.t. decs : specs *)

let rec type_check_decl (d: global_decls) ctx =
        match d with
        | Decl (n, t) -> let ty = infer_exp ctx t in global_ctx := Global_ctx.add n ty !global_ctx; Unit
        | Decls (s, d) -> type_check_decl s ctx; type_check_decl (Decl d) ctx;  Unit 
*)

let rec type_check ctx (t: tcompUnit) = match t with
| Lcomp (name, t1, t2) -> type_check (Context.add name (infer_exp ctx t1) ctx) t2
| Lterm (name, t1, t2) -> infer_exp (Context.add name (infer_exp ctx t1) ctx) t2

let rec get_fv (t: S.term) bv st = match t with
| Var name -> if (BoundVars.mem name bv || Global_ctx.mem name st.globals) then [] else name::[]
| Abs (_, nm, b) -> get_fv b (BoundVars.add nm bv) st 
| App (t1, t2) -> List.concat [(get_fv t1 bv st); (get_fv t2 bv st)]
| Ref t -> get_fv t bv st
| Deref t -> get_fv t bv st
| Assign (t1, t2) -> List.concat [(get_fv t1 bv st); (get_fv t2 bv st)]
| Unit -> []
| Val x -> []
| Let (s, t1, t2) -> List.concat [(get_fv t1 bv st); (get_fv t2 (BoundVars.add s bv) st)] 
| Asc (e, t) -> get_fv e bv st


