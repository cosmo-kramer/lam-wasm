open Map
open Printf

type ty = 
        | I 
        | F of ty*ty

type term =
        | Var of string  
        | Abs of string*ty*term   (* name, type, body *)
        | App of term*term



let rec pr_type = function 
        | I -> "I"
        | F (a, b) -> "("^pr_type a^") -> ("^pr_type b^")"

let rec to_string = function 
        | Var name -> name
        | Abs (name, tp, b) -> "/" ^ name ^ ".(" ^ to_string b ^ ")"
        | App (t1, t2) -> to_string t1 ^ "( "^to_string t2^" )"



        (* Exceptions *)
exception Type_resolution_failed of string
exception Application_failed of string
exception Unbound_Var of string 



(* Context Initialization *)
module Context = Map.Make(String)
let global_context = Context.empty 

let rec typeOf ctx t = match t with 
        | Var name ->    
                        Context.find name ctx
                       (*  with Not_found -> raise (Type_resolution_failed "Var "^name^" not found!")*)
        | Abs (name, tp, b) -> let new_ctx = Context.add name tp ctx in 
                                                F (tp, typeOf new_ctx b)
        | App (t1, t2) -> match typeOf ctx t1 with
                                | I -> raise (Type_resolution_failed "Exp not of func type!") 
                                | F (a, b) -> if (typeOf ctx t2) = b 
                                              then a
                                              else raise (Type_resolution_failed (
                                                      "Arg type mismatch!"^pr_type (typeOf ctx t1)^" and  "^pr_type (typeOf ctx t2)
                                                      )
                                              ) 


                                (* In v1 replace name by v2 *)
let rec subs v1 name v2 = match v1 with
                        | Var nm -> if nm = name then v2 else v1
                        | Abs (nm, tp, b) -> Abs(nm, tp, subs b name v2)
                        | App (t1, t2) -> App (subs t1 name v2, subs t2 name v2)


let rec eval ctx t = match t with
        | Var(name) -> (try
                                Context.find name ctx; t 
                        with Not_found -> raise (Unbound_Var name))
        | Abs (name, arg_type, func_body) -> t  (* let new_ctx = Context.add name arg_type ctx in 
                                                                let new_body = eval new_ctx func_body in 
                                                                Abs (name, arg_type, new_body) *)
        | App (t1, t2) -> let v2 = eval ctx t2 in 
                          let v1 = eval ctx t1 in 
                          apply ctx v1 v2 

and apply ctx v1 v2 = match typeOf ctx v1 with
                          | I -> raise (Application_failed "Not a function!")
                          | F (a, b) -> match v1 with 
                                        | Abs (name, tp, b) -> if a = typeOf ctx v2 
                                                               then let new_ctx = Context.add name tp ctx in eval new_ctx (subs b name v2) 
                                                               else raise (Application_failed (
                                                                 "Arg type mismatch  2!"^pr_type (a)^" and  "^pr_type (typeOf ctx v2)
                                                                        )
                                                               )
                                        | _ -> raise (Application_failed "Not a function 2!") 
                  


