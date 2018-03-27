open Map
open Utils
(*open Why3*)
module S = Utils
exception TyErr of string
(*
let global_ctx: ((ty Global_ctx.t) ref) = ref Global_ctx.empty

*)
       

let arrow_types (ty : S.ty) : S.ty * S.ty =
	match ty with
	| S.Tfun (t1, t2) -> t1, t2
	| S.Tun -> S.Tun, S.Tun
	| _ -> raise (TyErr "expected function or Un")

let contents_type (ty : S.ty) : S.ty =
	match ty with
	| S.Tref (x, b, phi) -> S.R (x, b, phi)
	| S.Tun -> S.Tun
	| _ -> raise (TyErr "expected reference or Un")


let rec check_exp (ctx : (S.ty Context.t)) (e : S.term) (ty : S.ty) : unit =
	match e with
        | S.Plus (e1, e2) -> check_exp ctx e1 intT; check_exp ctx e2 intT;  
                             (match ty with
                              | S.R (x, intT, phi) -> check_ref ctx (erase ctx) ("__NONE__", Tr) (x, phi)
                              | _ -> raise (TyErr "Type check failed in addition!"); ()
                                ) 

        | S.Abs (_, x, e) ->
		let t1, t2 = arrow_types ty in
		let ctx = Context.add x t1 ctx in
		check_exp ctx e t2
	| S.Let (x, e1, e2) ->
		let t1 = infer_exp ctx e1 in
		let ctx = Context.add x t1 ctx in
		check_exp ctx e2 ty
	| S.Ref e -> check_exp ctx e (contents_type ty)
        | S.Fix f -> check_exp ctx f (S.Tfun (ty, ty));                        
        | _ ->
	let t2 = infer_exp ctx e in
	if subtype t2 ty ctx then ()                (* subtype t2 ty -> t2 is subtype of ty *)
	else raise (TyErr ("subsumption "^term_to_string e))

and infer_contents_type ctx (e : S.term) : S.ty =
	let ty = infer_exp ctx e in
	contents_type ty

and infer_exp ctx (e : S.term) : S.ty =
	match e with
        | S.Val n -> let x = Context.fold (fun a b c -> a^c) ctx "__" in  Rty (x, intT, Eq (Var x, Val n)) 
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
		(S.Rty ("__NONE__", S.unitT, Tr)) 
	| S.Asc (e, ty) -> check_exp ctx e ty; ty
        | S.Constructor (s, l) ->
                                        let pred = ( fun bT -> let Base (_, cs) = bT in 
                                                               find_opt (fun x -> let (nm, _) = x in nm == s) cs ) in
                                        let ty = ref "__NONE__" in
                                        let tList = ref [] in
                                        (Base_types.iter (fun k v ->
                                                        if pred v != None then 
                                                                let Some (nm, ll) = pred v in 
                                                                ty := k;tList := ll else ()
                                                  ) !base_types);
                                        let tL = List.combine l !tList in
                                        List.iter (fun x ->
                                                let (t, ty) = x in
                                                check_exp ctx t ty;
                                                ) tL;
                                        if !ty == "__NONE__" then (raise (TyErr "Invalid  cons!")) else S.R ("__NONE__", !ty, Tr)  
                                                                     
        | _ -> raise (TyErr ("type ascription required  "^S.term_to_string e)); Tun 
and 
    simpl = function 
        | Val n -> Val n
        | Var x -> Var x
        | Plus (t1, t2) -> Plus (simpl t1, simpl t2)
        | Abs (n, x, b) -> Abs (n, x, b)
        | App (e1, e2) -> match e1 with
                        | Abs (n, x, bd) -> subst x e2 bd
                        | _ -> raise (Error "simplification failed, term applied to non-func value!"); Unit
        | Unit -> Unit
        | Asc (t, ty) -> simpl t
        | Let (x, t1, t2) -> simpl (App (Abs ("__NONE__", x, t2), t1))
        | _ -> raise (Error "Impure phi! simpl error!"); Unit

and reduce ctx phi = match phi with
        | Eq (t1, t2) -> Eq ((simpl t1), (simpl t2))
        | Leq (t1, t2) -> Leq ((simpl t1), (simpl t2))
        | Un t -> Un (simpl t)
        | Tr -> Tr

and substInRefinement x t phi = match phi with
| Eq (t1, t2) -> Eq ((subst x t t1), (subst x t t2))
| Leq (t1, t2) -> Leq ((subst x t t1), (subst x t t2))
| Un t1 -> Un (subst x t t1)
| Tr -> Tr
and check_ref ctx (erased: refinement list) (ref1: string*refinement) (ref2: string*refinement) = 
        let (x, phi1) = ref1 in 
        let (y, phi2) = ref2 in
        let erased = List.map (reduce ctx) erased in
        let phi1 = sub erased (reduce ctx phi1) in
        let phi2 = sub erased (reduce ctx phi2) in
        match phi2 with
        | Eq (Abs (n1, x1, b1), Abs (n2, x2, b2)) -> check_ref ctx erased ref1 (x1, (Eq (b1, subst x2 (Var x1) b2)))
        | Eq (Var x, Val n) ->   ()
        | Eq (Plus (t1, t2), Val n) ->  () 
        | _-> "Refinement check failed! (type_check.ml 53)"; ()
         
and subtype t2 ty ctx : bool = match (t2, ty) with
        | (Tun, R (x, _, Un (Var y))) -> y = x
        | (R (x, _, Un (Var y)), Tun) -> x = y
        | (R (x, t1, phi1), (R (y, t2, phi2))) -> Base_types.mem t1 !base_types && Base_types.mem t2 !base_types && (t1=t2) && (check_ref ctx (erase ctx) (x, phi1) (y, phi2); true)    
        | _ -> true 

and sub erased phi = match erased with
| [] -> phi
| h::t -> match h with
        | Eq (t1, t2) -> match (t1, t2) with 
                         | (Var x, Var y) -> sub t (substInRefinement y (Var x) phi)
                         | (Var x, e) -> sub t (substInRefinement x e phi)
                         | _ -> sub t phi
        | _ -> Printf.printf "Ignoring!\n"; sub t phi
                                 
 

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
| Lterm t -> infer_exp ctx t

let rec get_fv (t: S.term) bv st = match t with
| Var name -> if (BoundVars.mem name bv || Global_ctx.mem name st.globals) then [] else name::[]
| Abs (_, nm, b) -> get_fv b (BoundVars.add nm bv) st 
| App (t1, t2) -> List.concat [(get_fv t1 bv st); (get_fv t2 bv st)]
| Plus (t1, t2) -> List.concat [(get_fv t1 bv st); (get_fv t2 bv st)]
| Ref t -> get_fv t bv st
| Deref t -> get_fv t bv st
| Assign (t1, t2) -> List.concat [(get_fv t1 bv st); (get_fv t2 bv st)]
| Unit -> []
| Val x -> []
| Let (s, t1, t2) -> List.concat [(get_fv t1 bv st); (get_fv t2 (BoundVars.add s bv) st)] 
| Asc (e, t) -> get_fv e bv st


