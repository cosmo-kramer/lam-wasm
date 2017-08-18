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

let func_num = ref 0
let get_func_num: int = (func_num := !func_num + 1; !func_num) 

(* Context Initialization *)
module Context = Map.Make(String)
module Closures = Map.Make(struct type t = int let compare : int -> int -> int = compare end)

let rec typeOf ctx t = match t with 
        | Var name ->  ( try 
                Context.find name ctx
                        with Not_found -> raise (Type_resolution_failed ("Var "^name^" not found!")))
        | Abs (name, tp, b) -> let new_ctx = Context.add name tp ctx in 
                                                F (tp, typeOf new_ctx b)
        | App (t1, t2) -> (match typeOf ctx t1 with
                                | F (a, b) -> if (typeOf ctx t2) = a 
                                              then b
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

let funcs_code = ref ""
let heap_ctr = ref 0


let create_code t = let main_code = gen_webAsm Context.empty Closures.empty in
                    
                    let code = ref "(module \n
                                      (table 

let rec gen_webAsm t ctx clsr = match t with
                           | Var x -> (try 
                                        (Context.find x ctx);
                                        "(get_local $"^x^")"
                                      with Not_found -> raise (Unbound_Var x))
                           | Abs (name, arg_type, func_body) -> let f_num: int = get_func_num in
                                                                let header = "func $FF"^string_of_int f_num^" (param $"^name^"i32) (result i32) (\n" in
                                                                let new_clsr = Closures.add f_num !heap_ctr clsr in  
                                                                let cl_it = ref 0 in 
                                                                let cl_init = ref ("(i32.store offset="^string_of_int (4*(!cl_it))^"\n(i32.const "^string_of_int f_num^")\n(i32.const "^string_of_int !heap_ctr^")\n)\n") in 
                                                                
                                                                Context.iter (fun k v -> heap_ctr := !heap_ctr + 4; 
                                                                              cl_it := !cl_it+1;
                                                                             cl_init := (!cl_init)^"(i32.store offset="^string_of_int (4*(!cl_it))^"\n(get_local $"^k^")\n(i32.const "^string_of_int !heap_ctr^")\n)\n") ctx; 
                                                                let new_ctx = Context.add name arg_type ctx in
                                                                let bd = gen_webAsm func_body new_ctx new_clsr in 
                                                                funcs_code := !funcs_code^"\n\n"^header^bd^")\n";
                                                                "( " ^ !cl_init ^ "\n(i32.const "^string_of_int (Closures.find f_num new_clsr)^")"
                                                                
                                                                

                           | App (t1, t2) -> "Not yet ready!!\n" 
                                        


