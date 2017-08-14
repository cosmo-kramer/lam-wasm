open Map
open Printf

type ty = 
        | I 
        | F of ty*ty
        | Tref of ty
        | Unit

type term =
        | Var of string  
        | Abs of string*ty*term   (* name, type, body *)
        | App of term*term
        | Ref of term 
        | Deref of term
        | Assign of term*term
        | Location of int*ty
        | Unit
        | Zero 

let rec pr_type = function 
        | I -> "I"
        | F (a, b) -> "("^pr_type a^") -> ("^pr_type b^")"
        | Tref t -> "Ref "^pr_type t
        | Unit -> "Unit"


let rec to_string = function 
        | Var name -> name
        | Abs (name, tp, b) -> "/" ^ name ^ ".(" ^ to_string b ^ ")"
        | App (t1, t2) -> to_string t1 ^ "( "^to_string t2^" )"
        | Ref t -> "Ref ("^to_string t^")"
        | Deref t -> "! ("^to_string t^")"
        | Assign (t1, t2) -> to_string t1^" := "^to_string t2
        | Location (i, tp) -> string_of_int i
        | Unit -> "Unit"
        | Zero -> "0"
        
        
        (* Exceptions *)
exception Type_resolution_failed of string
exception Application_failed of string
exception Unbound_Var of string 
exception Eval_error of string 


(* Context Initialization *)
module Context = Map.Make(String)
module Store = Map.Make(struct type t = int let compare : int -> int -> int = compare end)
let store_size = ref 0

let rec typeOf ctx t = match t with 
        | Var name ->    
                        Context.find name ctx
                       (*  with Not_found -> raise (Type_resolution_failed "Var "^name^" not found!")*)
        | Abs (name, tp, b) -> let new_ctx = Context.add name tp ctx in 
                                                F (tp, typeOf new_ctx b)
        | App (t1, t2) -> (match typeOf ctx t1 with
                                | F (a, b) -> if (typeOf ctx t2) = b 
                                              then a
                                              else raise (Type_resolution_failed (
                                                      "Arg type mismatch!"^pr_type (typeOf ctx t1)^" and  "^pr_type (typeOf ctx t2)
                                                      )
                                              )
                                | _ -> raise (Type_resolution_failed "Exp not of func type!") 
                           )
        | Ref t1 -> Tref (typeOf ctx t1)
        | Deref t1 -> (match typeOf ctx t1 with
                     | Tref y -> y
                     | _ -> raise (Type_resolution_failed "Can not deref a non ref value!"))
        | Assign (_, _) -> Unit
        | Location (i, tp) -> Tref tp
        | Unit -> Unit 
        | Zero -> I

                                (* In v1 replace name by v2 *)
let rec subs v1 name v2 = match v1 with
                        | Var nm -> if nm = name then v2 else v1
                        | Abs (nm, tp, b) -> Abs(nm, tp, subs b name v2)
                        | App (t1, t2) -> App (subs t1 name v2, subs t2 name v2)
                        | Ref t -> Ref (subs t name v2)
                        | Deref t -> Deref (subs t name v2)
                        | Assign (x, y) -> Assign (subs x name v2, subs y name v2)
                        | Location (i, tp) -> v1
                        | Unit -> Unit
                        | Zero -> Zero

let rec eval ctx store t = match t with
        | Var(name) -> (try
                                Context.find name ctx; (t, store) 
                        with Not_found -> raise (Unbound_Var name))
        | Abs (name, arg_type, func_body) -> (t, store)  (* let new_ctx = Context.add name arg_type ctx in 
                                                                let new_body = eval new_ctx func_body in 
                                                                Abs (name, arg_type, new_body) *)
        | App (t1, t2) -> let (v1, store_new) = eval ctx store t1 in 
                          let (v2, store_n) = eval ctx store_new t2 in 
                          apply ctx store_n v1 v2
        | Ref t1 -> store_size := !store_size + 1;
                    let (v, store_new) = eval ctx store t1 in 
                    let store_n = Store.add  !store_size v store in
                    (Location (!store_size, (typeOf ctx t1)), store_n)
        | Deref t1 -> let (v, store_new) = eval ctx store t1 in
                     (match v with
                     | Location (i, _) -> (Store.find i store_new, store_new) 
                     | _ -> raise (Eval_error "Dereferencing_Non_Loc"))
        | Assign (t1, t2) -> if typeOf ctx t1 = Tref (typeOf ctx t2) then 
                                 let (v1, store_new) = eval ctx store t1 in
                                 let (v2, store_n) = eval ctx store_new t2 in 
                                 (
                                         match v1 with
                                         | Location (i, tp) -> (Unit, Store.add i v2 store)
                                         | _ -> raise (Eval_error "Assigning to non location value")
                                 )
                             else raise (Eval_error "Assignment type mismatch!")

        | Location _ -> (t, store)
        | Unit -> (Unit, store)
        | Zero -> (Zero, store)


and apply ctx store v1 v2 = match typeOf ctx v1 with
                          | F (a, b) -> (match v1 with 
                                        | Abs (name, tp, b) -> if a = typeOf ctx v2 
                                                               then let new_ctx = Context.add name tp ctx in eval new_ctx store (subs b name v2) 
                                                               else raise (Application_failed (
                                                                 "Arg type mismatch  2!"^pr_type (a)^" and  "^pr_type (typeOf ctx v2)
                                                                        )
                                                               )
                                        | _ -> raise (Application_failed "Not a function 2!")) 
                          | _ -> raise (Application_failed "Not a function!")

