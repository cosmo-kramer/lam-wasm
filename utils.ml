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
        | Val of int 

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
        | Val x -> string_of_int x
        
        
        (* Exceptions *)
exception Type_resolution_failed of string
exception Application_failed of string
exception Unbound_Var of string 
exception Eval_error of string 

let func_num = ref 0
let get_func_num () = (func_num := !func_num + 1; !func_num) 

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
        | Val _ -> I

let funcs_code = ref ""
let heap_ctr = ref 0



let rec gen_webAsm t ctx clsr = match t with
                           | Var x -> (try 
                                        (Context.find x ctx);
                                        ("(get_local $"^x^")", clsr)
                                      with Not_found -> raise (Unbound_Var x))
                           | Abs (name, arg_type, func_body) -> (let f_num: int = get_func_num () in
                                                                let header = "(func $FF"^string_of_int f_num^" (param $"^name^" i32) (result i32) \n" in
                                                                heap_ctr := !heap_ctr + 4;
                                                                let new_clsr = Closures.add f_num !heap_ctr clsr in 
                                                                let cl_it = ref 0 in 
                                                                let cl_init = ref ("(i32.store \n(i32.const "^string_of_int !heap_ctr^")\n(i32.const "^string_of_int (f_num-1)^")\n)\n") in 
                                                                
                                                                Context.iter (fun k v -> heap_ctr := !heap_ctr + 4; 
                                                                              cl_it := !cl_it+1;
                                                                             cl_init := (!cl_init)^"(i32.store \n(i32.const "^string_of_int !heap_ctr^")\n(get_local $"^k^")\n)\n") ctx; 
                                                                let get_cl = ref "" in
                                                                
                                                                let cl_addr = ref (Closures.find f_num new_clsr) in 
                                                                Context.iter (fun k v -> 
                                                                        cl_addr := !cl_addr+4;
                                                                        get_cl := !get_cl^(
                                                                                "(local $"^k^" i32)\n(i32.load (i32.const "^
                                                                                string_of_int !cl_addr^")\n)\n(set_local $"^k^")\n")) ctx; 
                                                                let new_ctx = Context.add name arg_type ctx in
                                                                let (bd, new_clsr) = gen_webAsm func_body new_ctx new_clsr in 
                                                                funcs_code := !funcs_code^"\n\n"^header^(!get_cl)^bd^")\n";
                                                                try 
                                                                (!cl_init ^ "\n(i32.const "^string_of_int (Closures.find f_num new_clsr)^")", new_clsr)
                                                                with Not_found -> raise (Unbound_Var (string_of_int f_num))
                                                                )  
                           
                          | App (t1, t2) -> let (code2, new_clsr) = gen_webAsm t2 ctx clsr in
                                             let (code1, n_clsr) = gen_webAsm t1 ctx new_clsr in

                                             (code2^code1^"(i32.load)\n(call_indirect $GG)\n", n_clsr)
                                              
                          | Val x -> ("(i32.const "^string_of_int x^")\n", clsr)
                          | Ref t -> 
                                     heap_ctr := !heap_ctr + 4;
                                     let hc = !heap_ctr in 
                                     let res = "\n(i32.const "^string_of_int hc ^ ")\n" in
                                    let (cd, new_clsr) = gen_webAsm t ctx clsr in
                                     (res ^ cd ^ "\n (i32.store) \n" ^ "(i32.const "^ string_of_int hc ^ ")\n", new_clsr)  
                          | Deref t -> let (cd, new_clsr) = gen_webAsm t ctx clsr in
                                       (cd ^ "\n(i32.load)\n", new_clsr)

let create_code t = let (cd, clsr) = (gen_webAsm t Context.empty Closures.empty) in
                                       let tab_ins = ref "" in
                                       (Closures.iter (fun k v -> tab_ins := (!tab_ins)^"(elem (i32.const "^string_of_int (k-1)^") $FF"^string_of_int k^")\n") clsr);
                                       "(module \n"^
                                       "(type $GG (func (param i32) (result i32)))\n"^
                                        "(table " ^ string_of_int (Closures.cardinal clsr) ^ " anyfunc)\n"^(!tab_ins)^
                                       "(memory $0 100)\n"^
                                       "(export \"memory\" (memory $0))\n"^
                                       "(export \"main\" (func $mm))\n" ^ !funcs_code ^ "\n (func $mm (result i32)"^cd^"\n)\n)" 
