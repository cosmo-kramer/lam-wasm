type ty = 
        | I 
        | F of ty*ty


(* Exceptions *)
exception Type_resolution_failed of string
exception Application_failed of string
exception Unbound_Var of string 



(* Context Initialization *)
open Map
open Printf
module Context = Map.Make(String)
let global_context = Context.empty 

type term =
        | Var of string  
        | Abs of string*ty*term   (* name, type, body *)
        | App of term*term

let rec typeOf ctx t :ty = match t with 
        | Var name ->    
                                Context.find name ctx
                       (*  with Not_found -> raise (Type_resolution_failed "Var "^name^" not found!\n")*)
        | Abs (name, tp, b) -> let new_ctx = Context.add name tp ctx in 
                               F (tp, typeOf new_ctx b)
        | App (t1, t2) -> match typeOf ctx t1 with
                                | I -> raise (Type_resolution_failed "Exp not of func type!\n") 
                                | F (a, b) -> if (typeOf ctx t2) == b then a else raise (Type_resolution_failed "Arg type mismatch!\n") 


(* In v1 replace name by v2 *)
let rec subs v1 name v2 = match v1 with
                          | Var nm -> if nm == name then v2 else v1
                          | Abs (nm, tp, b) -> subs b name v2
                          | App (t1, t2) -> App (subs t1 name v2, subs t2 name v2)



let apply ctx v1 v2 = match v1 with
        | Abs (name, arg_type, func_body) ->  ( match typeOf ctx v1 with
                                                | I -> raise (Application_failed "Not a function!\n")
                                                | F (a, b) -> if a == typeOf ctx v2 then subs v1 name v2 else raise (Application_failed "type mismatch!\n")) 
        | _ -> raise (Application_failed "Malformed substitution!\n")

let rec eval ctx t: term = match t with
        | Var(name) -> (try
                Context.find name ctx; t 
                       with Not_found -> raise (Unbound_Var name))
        | Abs (name, arg_type, func_body) -> let new_ctx = Context.add name arg_type ctx in 
                                             let new_body = eval new_ctx func_body in 
                                             Abs (name, arg_type, new_body)
        | App (t1, t2) -> let v2 = eval ctx t2 in 
                          let v1 = eval ctx t1 in 
                          apply ctx v1 v2 
                                                                                                                       
let rec to_string = function 
        | Var name -> name
        | Abs (name, tp, b) -> "/" ^ name ^ ".(" ^ to_string b ^ ")"
        | App (t1, t2) -> to_string t1 ^ "( "^to_string t2^" )"

